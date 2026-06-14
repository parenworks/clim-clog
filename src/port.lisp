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
                :reader clog-port-event-lock))
  (:default-initargs :pointer (make-instance 'standard-pointer)))

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
   (sheet   :initarg :sheet   :reader clog-mirror-sheet))
  (:documentation "Backing object for a mirrored sheet: a CLOG canvas and
its 2D context."))

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
             (context (clog:create-context2d canvas)))
        ;; Keep a port-level handle so a bare medium (Phase 0 harness) and
        ;; PORT-FORCE-OUTPUT can still reach a context.
        (setf (clog-port-canvas port) canvas
              (clog-port-context port) context)
        (let ((mirror (make-instance 'clog-mirror :canvas canvas
                                                  :context context
                                                  :sheet sheet)))
          ;; Wire browser pointer/keyboard events on this canvas into the
          ;; CLIM event queue (see input.lisp).
          (install-clog-input-handlers port sheet canvas)
          mirror)))))

(defmethod destroy-mirror ((port clog-port) (sheet mirrored-sheet-mixin))
  (let ((mirror (sheet-direct-mirror sheet)))
    (when mirror
      (ignore-errors (clog:destroy (clog-mirror-canvas mirror))))))

(defmethod set-mirror-geometry ((port clog-port) (sheet mirrored-sheet-mixin) region)
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* region)
    (let ((mirror (sheet-direct-mirror sheet)))
      (when mirror
        (let ((canvas (clog-mirror-canvas mirror))
              (w (ceiling (- x2 x1)))
              (h (ceiling (- y2 y1))))
          ;; The intrinsic <canvas> width/height attributes set the drawing
          ;; surface size (not just the CSS box).
          (ignore-errors
           (setf (clog:property canvas "width")  (princ-to-string w)
                 (clog:property canvas "height") (princ-to-string h))))))
    (values x1 y1 x2 y2)))

(defmethod enable-mirror  ((port clog-port) (sheet mirrored-sheet-mixin)) nil)
(defmethod disable-mirror ((port clog-port) (sheet mirrored-sheet-mixin)) nil)
(defmethod shrink-mirror  ((port clog-port) (mirror mirrored-sheet-mixin)) nil)
(defmethod raise-mirror   ((port clog-port) (sheet mirrored-sheet-mixin)) nil)
(defmethod bury-mirror    ((port clog-port) (sheet mirrored-sheet-mixin)) nil)

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
(defmethod (setf port-keyboard-input-focus) (focus (port clog-port)) focus)
(defmethod port-keyboard-input-focus ((port clog-port)) nil)
(defmethod port-force-output ((port clog-port)) nil)
(defmethod set-sheet-pointer-cursor ((port clog-port) sheet cursor)
  (declare (ignore sheet cursor))
  nil)
