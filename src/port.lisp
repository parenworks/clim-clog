;;;; port.lisp
;;;;
;;;; The PORT is the logical representation of a display service.  For the
;;;; CLX backend a port wraps the socket to the X11 server; for clim-clog a
;;;; port wraps a live CLOG connection (a browser tab) plus the <canvas>
;;;; and its 2D drawing context that we render into.
;;;;
;;;; Ports are looked up by a "server path" whose first element is the
;;;; backend designator.  Registering FIND-PORT-TYPE for :clog lets
;;;; (clim:find-port :server-path '(:clog ...)) create one of these.
;;;;
;;;; STATUS (Phase 1): mirrored sheets.  Each top-level (or unmanaged)
;;;; sheet is realized as a CLOG <canvas> wrapped in a CLOG-MIRROR.  The
;;;; medium draws onto the mirror's 2D context (see medium.lisp).  Pane
;;;; layout and repaint are driven by McCLIM through the standard
;;;; frame-manager.  Input (Phase 2) is still stubbed in PROCESS-NEXT-EVENT.
;;;;
;;;; We mirror only top-level sheets (child panes share the top-level
;;;; mirror via native transformations), the same strategy the CLX backend
;;;; uses by default.

(in-package #:clim-clog)

(defvar *clog-server-path* '(:clog)
  "Default server path used when opening a clim-clog port.")

;;; A pointer that caches its last known screen position.  McCLIM's
;;; STANDARD-POINTER hardcodes POINTER-POSITION to (values 0 0) and errors on
;;; SETF -- the comment in pointer.lisp says the backend is expected to track
;;; the position from pointer events.  Presentation highlighting and pointer
;;; documentation work by SYNTHESIZING a motion event at (POINTER-POSITION
;;; pointer); without a live position the tracker always probes (0,0), finds
;;; no presentation, and nothing highlights.  We update this from every CLOG
;;; pointer event (see MAKE-CLOG-POINTER-EVENT in input.lisp).
(defclass clog-pointer (standard-pointer)
  ((x :initform 0 :accessor %clog-pointer-x)
   (y :initform 0 :accessor %clog-pointer-y)))

(defmethod pointer-position ((pointer clog-pointer))
  (values (%clog-pointer-x pointer) (%clog-pointer-y pointer)))

(clim-sys:defmethod* (setf pointer-position) (x y (pointer clog-pointer))
  (setf (%clog-pointer-x pointer) x
        (%clog-pointer-y pointer) y))

(defclass clog-port (basic-port)
  ((id)
   ;; The CLOG object we create the canvas under (typically a window
   ;; body).  Supplied via the server path as (:clog :body <clog-obj>).
   (body    :initarg :body    :initform nil :accessor clog-port-body)
   ;; The CLOG-CANVAS element and its CLOG-CONTEXT2D handle.  These are
   ;; the "mirror": the device object the medium actually draws on.
   (canvas  :initarg :canvas  :initform nil :accessor clog-port-canvas)
   (context :initarg :context :initform nil :accessor clog-port-context)
   (width   :initarg :width   :initform 800 :accessor clog-port-width)
   (height  :initarg :height  :initform 600 :accessor clog-port-height)
   ;; Phase 2 input: CLOG event handlers (running in CLOG worker threads)
   ;; push CLIM events here; the port I/O loop drains them in
   ;; PROCESS-NEXT-EVENT (see input.lisp).
   (event-queue :initform '() :accessor clog-port-event-queue)
   (event-lock  :initform (clim-sys:make-recursive-lock "clog event queue")
                :reader clog-port-event-lock)
   ;; Phase 3: cache of real browser text metrics, keyed by CSS-font string
   ;; (font-level ascent/descent/em-width) and by (css . char) for per-glyph
   ;; widths.  CLIM asks for text-size synchronously during layout; we
   ;; round-trip MEASURE-TEXT over the websocket once and remember the answer
   ;; (the browser's metrics are stable for the life of the connection).
   (text-metrics-cache :initform (make-hash-table :test 'equal)
                       :reader clog-port-text-metrics-cache))
  (:default-initargs :pointer (make-instance 'clog-pointer)))

;;; Backend registration.  The second value is the server-path parser; we
;;; pass the path through unchanged (IDENTITY), exactly like the Null
;;; backend.
(defmethod find-port-type ((type (eql :clog)))
  (values 'clog-port 'identity))

(defmethod initialize-instance :after ((port clog-port) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value port 'id) (gensym "CLOG-PORT-"))
  ;; Pull options out of the server path: (:clog :body B :width W :height H).
  (let ((options (cdr (port-server-path port))))
    (when (getf options :body)   (setf (clog-port-body port)   (getf options :body)))
    (when (getf options :width)  (setf (clog-port-width port)  (getf options :width)))
    (when (getf options :height) (setf (clog-port-height port) (getf options :height))))
  ;; Every port needs at least one frame manager registered.
  (push (make-instance 'clog-frame-manager :port port)
        (slot-value port 'climi::frame-managers))
  ;; Start the port I/O loop (a thread looping PROCESS-NEXT-EVENT), the
  ;; single consumer of our event queue.  CLX does this here too.
  (restart-port port))

(defmethod print-object ((object clog-port) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "~S ~S" :id (slot-value object 'id))))

;;; ------------------------------------------------------------------
;;; Mirror protocol
;;;
;;; A "mirror" is the windowing-system object backing a sheet.  Ours wraps
;;; the CLOG <canvas> created for a top-level sheet plus its 2D drawing
;;; context.  McCLIM stores it via (SETF SHEET-DIRECT-MIRROR) in the
;;; REALIZE-MIRROR :around method on BASIC-PORT.
;;; ------------------------------------------------------------------

(defclass clog-mirror ()
  ((canvas  :initarg :canvas  :reader clog-mirror-canvas)
   (context :initarg :context :reader clog-mirror-context)
   (sheet   :initarg :sheet   :reader clog-mirror-sheet)
   ;; Double buffering (Phase 3): the medium draws into an OFFSCREEN canvas;
   ;; CLOG-MIRROR-FLUSH blits the whole offscreen onto the visible CANVAS in
   ;; one DRAW-IMAGE op.  Without this each CLIM drawing op renders
   ;; individually over the websocket, so a full redisplay visibly flashes
   ;; (clear, then content pops back in).  See DESIGN.org Phase 3.
   (offscreen-canvas  :initarg :offscreen-canvas  :reader clog-mirror-offscreen-canvas)
   (offscreen-context :initarg :offscreen-context :reader clog-mirror-offscreen-context))
  (:documentation "Backing object for a mirrored sheet: a visible CLOG canvas
plus an offscreen canvas the medium draws into and that is blitted to the
visible one (double buffering)."))

(defun clog-mirror-flush (mirror)
  "Blit MIRROR's offscreen canvas onto its visible canvas in a single
DRAW-IMAGE.  This is the double-buffer page-flip."
  (clog:draw-image (clog-mirror-context mirror)
                   (clog-mirror-offscreen-canvas mirror)
                   0 0))

;;; ------------------------------------------------------------------
;;; Pixmaps (Phase 3)
;;;
;;; A CLIM pixmap is an off-screen drawable the medium can render into and
;;; later blit onto a sheet (MEDIUM-COPY-AREA / COPY-FROM-PIXMAP), the basis
;;; for double-buffered application drawing, scrolling, and sprite-style
;;; effects.  We back each one with its own hidden CLOG <canvas> + 2D context.
;;; WITH-OUTPUT-TO-PIXMAP binds a medium's drawable to the pixmap (see
;;; MEDIUM-DRAWABLE in medium.lisp) so the ordinary MEDIUM-DRAW-* protocol
;;; renders into the pixmap's context.
;;; ------------------------------------------------------------------

(defclass clog-pixmap ()
  ((canvas  :initarg :canvas  :reader clog-pixmap-canvas)
   (context :initarg :context :reader clog-pixmap-context)
   (width   :initarg :width   :reader pixmap-width)
   (height  :initarg :height  :reader pixmap-height))
  (:documentation "An off-screen CLOG canvas + 2D context backing a CLIM pixmap."))

(defmethod pixmap-depth ((pixmap clog-pixmap))
  ;; RGBA canvas backing store.
  32)

(defmethod deallocate-pixmap ((pixmap clog-pixmap))
  (ignore-errors (clog:destroy (clog-pixmap-canvas pixmap))))

;;; ALLOCATE-PIXMAP and the MEDIUM-COPY-AREA methods specialise on
;;; CLOG-MEDIUM and so live in medium.lisp (compiled after this file).

(defun %sheet-pixel-size (sheet port)
  "Return (values width height) in device pixels for SHEET, falling back to
the port's configured size when the sheet region is degenerate."
  (let* ((region (sheet-region sheet)))
    (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* region)
      (let ((w (ceiling (- x2 x1)))
            (h (ceiling (- y2 y1))))
        (values (if (plusp w) w (clog-port-width port))
                (if (plusp h) h (clog-port-height port)))))))

(defmethod realize-mirror ((port clog-port) (sheet mirrored-sheet-mixin))
  (let ((body (clog-port-body port)))
    (unless body
      (error "clim-clog: cannot realize a mirror without a CLOG body ~
              (port was created without :body)."))
    (multiple-value-bind (w h) (%sheet-pixel-size sheet port)
      (let* ((canvas  (clog:create-canvas body :width w :height h))
             (context (clog:create-context2d canvas))
             ;; Offscreen back-buffer the medium draws into (double
             ;; buffering, Phase 3); hidden so it never paints to the page.
             (offscreen (clog:create-canvas body :width w :height h :hidden t))
             (offscreen-context (clog:create-context2d offscreen)))
        ;; The visible context is used ONLY for the back-buffer blit.  With
        ;; the "copy" composite mode a single DRAW-IMAGE replaces the whole
        ;; visible canvas with the offscreen contents -- including any
        ;; transparent (cleared) regions -- so a WINDOW-CLEAR on the buffer
        ;; is reflected exactly, with no stale pixels and no intermediate
        ;; clear that would flash.
        (setf (clog:global-composite-operation context) "copy")
        ;; Keep a port-level handle so a bare medium (Phase 0 harness) and
        ;; PORT-FORCE-OUTPUT can still reach a context.
        (setf (clog-port-canvas port) canvas
              (clog-port-context port) context)
        (let ((mirror (make-instance 'clog-mirror
                                     :canvas canvas
                                     :context context
                                     :offscreen-canvas offscreen
                                     :offscreen-context offscreen-context
                                     :sheet sheet)))
          ;; Wire browser pointer/keyboard events on this canvas into the
          ;; CLIM event queue (see input.lisp).
          (install-clog-input-handlers port sheet canvas)
          mirror)))))

(defmethod destroy-mirror ((port clog-port) (sheet mirrored-sheet-mixin))
  (let ((mirror (sheet-direct-mirror sheet)))
    (when mirror
      (ignore-errors (clog:destroy (clog-mirror-canvas mirror)))
      (ignore-errors (clog:destroy (clog-mirror-offscreen-canvas mirror))))))

(defmethod set-mirror-geometry ((port clog-port) (sheet mirrored-sheet-mixin) region)
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* region)
    (let ((mirror (sheet-direct-mirror sheet)))
      (when mirror
        (let ((canvas (clog-mirror-canvas mirror))
              (offscreen (clog-mirror-offscreen-canvas mirror))
              (w (ceiling (- x2 x1)))
              (h (ceiling (- y2 y1)))
              (cur-w (ignore-errors
                      (parse-integer (clog:property canvas "width")
                                     :junk-allowed t)))
              (cur-h (ignore-errors
                      (parse-integer (clog:property canvas "height")
                                     :junk-allowed t))))
          ;; The intrinsic <canvas> width/height attributes set the drawing
          ;; surface size (not just the CSS box).  Writing EITHER attribute
          ;; -- even to its current value -- resets the canvas bitmap to
          ;; transparent, which on a visible canvas reads as a page flash.
          ;; So only touch the bitmap when the size actually changes, and
          ;; after a real resize immediately re-blit the offscreen so the
          ;; viewer never sees the blanked frame between resize and repaint.
          (unless (and (eql w cur-w) (eql h cur-h))
            (ignore-errors
             (setf (clog:property canvas "width")  (princ-to-string w)
                   (clog:property canvas "height") (princ-to-string h)
                   (clog:property offscreen "width")  (princ-to-string w)
                   (clog:property offscreen "height") (princ-to-string h))
             ;; Setting a canvas's intrinsic size resets its 2D context state,
             ;; so re-apply the blit composite mode on the visible context.
             (setf (clog:global-composite-operation (clog-mirror-context mirror))
                   "copy")
             (clog-mirror-flush mirror))))))
    (values x1 y1 x2 y2)))

;;; The double-buffer page-flip hook.  CLIM's display functions only *record*
;;; output; the actual drawing to the medium happens during repaint, when
;;; DISPATCH-REPAINT replays the records.  REPAINT-SHEET is invoked on the
;;; mirrored ancestor and recurses into its (non-mirrored) child panes, so an
;;; :AFTER here fires exactly once after the whole subtree has been drawn into
;;; the offscreen buffer -- covering the initial paint and every partial
;;; repaint (e.g. presentation highlighting).  MEDIUM-FINISH-OUTPUT also
;;; blits, for code paths that draw and explicitly finish output.
(defmethod repaint-sheet :after ((sheet mirrored-sheet-mixin) region)
  (declare (ignore region))
  (let ((mirror (sheet-direct-mirror sheet)))
    (when (typep mirror 'clog-mirror)
      (ignore-errors (clog-mirror-flush mirror)))))

(defmethod enable-mirror  ((port clog-port) (sheet mirrored-sheet-mixin)) nil)
(defmethod disable-mirror ((port clog-port) (sheet mirrored-sheet-mixin)) nil)
(defmethod shrink-mirror  ((port clog-port) (mirror mirrored-sheet-mixin)) nil)
(defmethod raise-mirror   ((port clog-port) (sheet mirrored-sheet-mixin)) nil)
(defmethod bury-mirror    ((port clog-port) (sheet mirrored-sheet-mixin)) nil)

;;; CLIM sets a frame's title via (SETF SHEET-PRETTY-NAME), which calls
;;; SET-MIRROR-NAME on the port; without a method the command loop dies the
;;; first time the title is set (e.g. when a frame is enabled or a dialog is
;;; run).  We reflect the name onto the browser document title.  SET-MIRROR-ICON
;;; has no canvas equivalent, so it is a no-op.
(defmethod set-mirror-name ((port clog-port) (sheet mirrored-sheet-mixin) name)
  (let ((body (clog-port-body port)))
    (when body
      (ignore-errors
       (setf (clog:title (clog:html-document body)) (princ-to-string name)))))
  name)

(defmethod set-mirror-icon ((port clog-port) (sheet mirrored-sheet-mixin) icon)
  (declare (ignore icon))
  nil)

;;; PROCESS-NEXT-EVENT and the CLOG input handlers live in input.lisp.

(defmethod make-graft ((port clog-port) &key (orientation :default) (units :device))
  (make-instance 'clog-graft
                 :port port :mirror t
                 :region (make-bounding-rectangle 0 0
                                                  (clog-port-width port)
                                                  (clog-port-height port))
                 :orientation orientation :units units))

(defmethod make-medium ((port clog-port) sheet)
  (make-instance 'clog-medium :port port :sheet sheet))

;;; Text-style mapping: the browser resolves fonts itself, so we map a
;;; CLIM text style to a CSS font shorthand string (see medium.lisp).
(defmethod text-style-mapping ((port clog-port) (text-style text-style)
                               &optional character-set)
  (declare (ignore character-set))
  (text-style->css text-style))

(defmethod (setf text-style-mapping) (font-name (port clog-port)
                                      (text-style text-style)
                                      &optional character-set)
  (declare (ignore character-set))
  font-name)

(defmethod port-modifier-state ((port clog-port)) nil)
;; NOTE: do NOT stub PORT-KEYBOARD-INPUT-FOCUS / (SETF ...) here.  BASIC-PORT
;; already implements them over its FOCUSED-SHEET slot, and DISTRIBUTE-EVENT
;; for keyboard events routes through PORT-KEYBOARD-INPUT-FOCUS; overriding it
;; to NIL silently drops every key event (it falls back to the top-level
;; sheet instead of the focused pane).
(defmethod port-force-output ((port clog-port)) nil)
(defmethod set-sheet-pointer-cursor ((port clog-port) sheet cursor)
  (declare (ignore sheet cursor))
  nil)
