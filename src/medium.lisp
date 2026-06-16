;;;; medium.lisp
;;;;
;;;; The MEDIUM maintains the drawing context for a sheet, and the actual
;;;; drawing operations (MEDIUM-DRAW-LINE*, MEDIUM-DRAW-TEXT*, ...) are
;;;; specialised on it.  This is where CLIM's drawing protocol is mapped
;;;; onto CLOG's HTML5 canvas API.
;;;;
;;;; The "device object" we draw on -- the mirror, returned by
;;;; MEDIUM-DRAWABLE -- is a CLOG-CONTEXT2D.  Where the turtleware
;;;; McCLIM-backends tutorial emits JavaScript strings like
;;;; "context.lineTo(x, y)", we
;;;; instead call the equivalent CLOG method (CLOG:LINE-TO ctx x y); CLOG
;;;; marshals it over the websocket for us.
;;;;
;;;; CLIM canvas op            CLOG method
;;;; ----------------------    ------------------------------
;;;; begin a path              CLOG:BEGIN-PATH
;;;; move pen                  CLOG:MOVE-TO
;;;; add a segment             CLOG:LINE-TO
;;;; rectangle path            CLOG:RECT
;;;; arc / ellipse             CLOG:ARC
;;;; fill current path         CLOG:PATH-FILL
;;;; stroke current path       CLOG:PATH-STROKE
;;;; clip to current path      CLOG:PATH-CLIP
;;;; filled text               CLOG:FILL-TEXT
;;;; outlined text             CLOG:STROKE-TEXT
;;;; text metrics              CLOG:MEASURE-TEXT
;;;; clear a region            CLOG:CLEAR-RECT
;;;;
;;;; STATUS (Phase 3): interactive.  Inks are handled for solid colours;
;;;; transformations are applied to coordinates; drawing is double-buffered
;;;; (see port.lisp / MEDIUM-FINISH-OUTPUT) and text metrics round-trip the
;;;; browser's MEASURE-TEXT (cached on the port).  Line dashes, skewed
;;;; ellipses, gradients/patterns and full (multi-rectangle) clipping fidelity
;;;; remain Roadmap items (see DESIGN.org Phase 3).

(in-package #:clim-clog)

(defclass clog-medium (basic-medium)
  ())

;;; The CLOG-MIRROR backing this medium's sheet, or NIL.  Child panes share
;;; their mirrored ancestor's (the top-level sheet's) mirror, so we walk up.
(defun medium-clog-mirror (medium)
  (let* ((sheet (medium-sheet medium))
         (ancestor (and sheet (sheet-mirrored-ancestor sheet)))
         (mirror (and ancestor (sheet-direct-mirror ancestor))))
    (and (typep mirror 'clog-mirror) mirror)))

;;; The device object the medium draws on.  With double buffering (Phase 3)
;;; the medium draws into the mirror's OFFSCREEN context; MEDIUM-FINISH-OUTPUT
;;; / MEDIUM-FORCE-OUTPUT blit it to the visible canvas.  Falls back to the
;;; port-level context for the bare-medium Phase-0 harness (no sheet
;;; hierarchy, no mirror), which draws straight to the visible canvas.
;;;
;;; WITH-OUTPUT-TO-PIXMAP rebinds (SETF MEDIUM-DRAWABLE) to a pixmap so the
;;; same drawing protocol renders off-screen into it; we honour that explicit
;;; drawable first (a CLOG-PIXMAP resolves to its own context, a raw context
;;; is used as-is) before falling back to the mirror / port context.
(defmethod medium-drawable ((medium clog-medium))
  (let ((explicit (climi::%medium-drawable medium)))
    (typecase explicit
      (clog-pixmap (clog-pixmap-context explicit))
      (clog:clog-context2d explicit)
      (t (let ((mirror (medium-clog-mirror medium)))
           (if mirror
               (clog-mirror-offscreen-context mirror)
               (clog-port-context (port medium))))))))

;;; ------------------------------------------------------------------
;;; Helpers: CLIM -> CSS
;;; ------------------------------------------------------------------

(defun ink->css (ink)
  "Translate a CLIM INK to a CSS colour string.

Solid colours and the standard indirect inks are handled.  +FLIPPING-INK+
and +TRANSPARENT-INK+ render as fully transparent: the command loop uses
+FLIPPING-INK+ for XOR presentation highlighting, which a 2D canvas cannot
do directly, so we no-op it (true highlight via canvas compositing is a
Roadmap item; hover feedback comes from the pointer-documentation pane).
Patterns and gradients are also Roadmap items and fall back to black."
  (cond
    ((typep ink 'clim:color)
     (multiple-value-bind (r g b) (clim:color-rgb ink)
       (format nil "rgb(~d,~d,~d)"
               (round (* 255 r)) (round (* 255 g)) (round (* 255 b)))))
    ((eq ink clim:+foreground-ink+) "black")
    ((eq ink clim:+background-ink+) "white")
    ((or (eq ink clim:+transparent-ink+) (eq ink clim:+flipping-ink+))
     "rgba(0,0,0,0)")
    (t "black")))

(defun text-style->css (text-style)
  "Translate a CLIM TEXT-STYLE to a CSS `font' shorthand string, e.g.
\"italic bold 14px sans-serif\"."
  (multiple-value-bind (family face size) (clim:text-style-components text-style)
    (let* ((px     (etypecase size
                     (number size)
                     (null 12)
                     (symbol (case size
                               (:tiny 8) (:very-small 9) (:small 10)
                               (:normal 12) (:large 16) (:very-large 20)
                               (:huge 24) (t 12)))))
           (css-family (case family
                         (:fix "monospace")
                         (:serif "serif")
                         (:sans-serif "sans-serif")
                         (t "sans-serif")))
           (weight (if (member face '(:bold :bold-italic)) "bold" "normal"))
           (slant  (if (member face '(:italic :bold-italic)) "italic" "normal")))
      (format nil "~a ~a ~dpx ~a" slant weight (round px) css-family))))

;;; ------------------------------------------------------------------
;;; Helpers: coordinate transformation and drawing-state application
;;; ------------------------------------------------------------------

(declaim (inline %js))
(defun %js (n)
  "Coerce N to a number that prints as a valid JavaScript numeric literal.

CLOG formats numbers into JS with ~A, and a Lisp double-float prints with a
`d0' exponent marker (e.g. 6.283185307179586d0) that is invalid JavaScript,
so the emitted canvas call silently fails in the browser.  Integers are
left exact; every other real is coerced to single-float, which prints with
no Lisp type marker under the default *read-default-float-format*."
  (if (integerp n) n (float n 1.0)))

(defun %native-position (medium x y)
  "Map (X,Y) to device/mirror coordinates.

McCLIM's standard medium drawing generics apply the *medium* transformation
via :around methods (TRANSFORM-COORDINATES-MIXIN) before dispatching to our
primary methods, so X/Y arrive in the sheet's coordinate system.  But only
the top-level sheet is mirrored -- every child pane shares that one canvas --
so each pane's drawing must additionally be offset by its position within the
canvas, i.e. by the sheet's NATIVE transformation.  Without this all panes
draw at the canvas origin and overlap, while MEDIUM-DEVICE-REGION (used for
clipping) is already in device space, so child output is clipped away.

For the top-level sheet the native transformation is the identity, so the
single-pane demos are unaffected."
  (let* ((sheet (medium-sheet medium))
         (nt    (and sheet (sheet-native-transformation sheet))))
    (if nt
        (transform-position nt x y)
        (values x y))))

(defmacro with-device-position ((dx dy medium x y) &body body)
  "Bind DX/DY to the device coordinates for X/Y, coerced JS-safe via %JS.
The incoming X/Y (already medium-transformed) are mapped through the sheet's
native transformation so child panes draw at their true canvas position."
  (let ((m (gensym "MEDIUM")) (nx (gensym "X")) (ny (gensym "Y")))
    `(let ((,m ,medium))
       (multiple-value-bind (,nx ,ny) (%native-position ,m ,x ,y)
         (let ((,dx (%js ,nx)) (,dy (%js ,ny)))
           ,@body)))))

(defun %dash-array-js (line-style)
  "Return a JavaScript array literal string for LINE-STYLE's dash pattern,
suitable for CLOG:SET-LINE-DASH (which interpolates its argument raw into
`setLineDash(...)').  CLIM's LINE-STYLE-DASHES is NIL (solid line, \"[]\"),
T (a default on/off pattern), or a sequence of segment lengths."
  (let ((dashes (line-style-dashes line-style)))
    (cond
      ((null dashes) "[]")
      ((eq dashes t) "[5,5]")
      (t (format nil "[~{~a~^,~}]"
                 (map 'list (lambda (n) (%js n)) dashes))))))

(defun apply-stroke-state (ctx medium)
  "Push MEDIUM's ink, line thickness and dash pattern onto the context."
  (let ((line-style (medium-line-style medium)))
    (setf (clog:stroke-style ctx) (ink->css (medium-ink medium)))
    (setf (clog:line-width ctx)
          (max 1 (round (line-style-thickness line-style))))
    (clog:set-line-dash ctx (%dash-array-js line-style))))

(defun apply-fill-state (ctx medium)
  "Push MEDIUM's ink onto the context as fill state."
  (setf (clog:fill-style ctx) (ink->css (medium-ink medium))))

;;; ------------------------------------------------------------------
;;; Clipping
;;;
;;; McCLIM confines drawing to MEDIUM-DEVICE-REGION -- the sheet's region
;;; intersected with the user clipping region, expressed in device
;;; coordinates.  Honouring it is essential for partial repaints: when the
;;; command loop un-highlights a presentation it dispatches a repaint of
;;; just that region and replays the output history clipped to it.  If we
;;; ignored the clip, a full-extent record (e.g. a background rectangle)
;;; would repaint the entire canvas and erase everything outside the region.
;;; We map the clip onto the canvas with save / rect+clip / restore.
;;; ------------------------------------------------------------------

(defun %add-region-rectangles (ctx region)
  "Add a canvas sub-path for every rectangle making up REGION.

REGION is the device clipping region.  A STANDARD-RECTANGLE-SET (produced by,
e.g., overlapping partial repaints) is several disjoint rectangles; we add one
canvas RECT per member so the subsequent PATH-CLIP clips to their *union*
rather than to the single bounding rectangle (which would leak drawing into
the gaps).  Non-rectangular sub-regions fall back to their bounding box."
  (clim:map-over-region-set-regions
   (lambda (r)
     (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* r)
       (clog:rect ctx (%js x1) (%js y1) (%js (- x2 x1)) (%js (- y2 y1)))))
   region))

(defun call-with-clipped-context (medium fn)
  "Call FN with the medium's drawable, the device clipping region applied.
FN is not called at all when the clip is empty (+NOWHERE+)."
  (let* ((ctx  (medium-drawable medium))
         (clip (medium-device-region medium)))
    (typecase clip
      (climi::nowhere-region nil)
      (climi::everywhere-region (funcall fn ctx))
      (t
       (clog:canvas-save ctx)
       (unwind-protect
            (progn
              ;; Build one path from every rectangle of the clip region, then
              ;; clip to it; for a rectangle set this is the true multi-rect
              ;; clip, not its bounding rectangle.
              (clog:begin-path ctx)
              (%add-region-rectangles ctx clip)
              (clog:path-clip ctx)
              (funcall fn ctx))
         (clog:canvas-restore ctx))))))

(defmacro with-clipped-context ((ctx medium) &body body)
  "Bind CTX to MEDIUM's drawable with the device clipping region applied,
restoring the prior canvas state afterward."
  `(call-with-clipped-context ,medium (lambda (,ctx) ,@body)))

;;; ------------------------------------------------------------------
;;; Drawing operations
;;; ------------------------------------------------------------------

(defmethod medium-draw-point* ((medium clog-medium) x y)
  (with-clipped-context (ctx medium)
    (let ((r (max 0.5 (/ (line-style-thickness (medium-line-style medium)) 2))))
      (with-device-position (dx dy medium x y)
        (apply-fill-state ctx medium)
        (clog:begin-path ctx)
        (clog:arc ctx dx dy (%js r) 0 (%js (* 2 pi)))
        (clog:path-fill ctx)))))

(defmethod medium-draw-points* ((medium clog-medium) coord-seq)
  (loop for i from 0 below (length coord-seq) by 2
        do (medium-draw-point* medium (elt coord-seq i) (elt coord-seq (1+ i)))))

(defmethod medium-draw-line* ((medium clog-medium) x1 y1 x2 y2)
  (with-clipped-context (ctx medium)
    (apply-stroke-state ctx medium)
    (clog:begin-path ctx)
    (with-device-position (dx dy medium x1 y1) (clog:move-to ctx dx dy))
    (with-device-position (dx dy medium x2 y2) (clog:line-to ctx dx dy))
    (clog:path-stroke ctx)))

(defmethod medium-draw-lines* ((medium clog-medium) coord-seq)
  (with-clipped-context (ctx medium)
    (apply-stroke-state ctx medium)
    (loop for i from 0 below (length coord-seq) by 4
          do (clog:begin-path ctx)
             (with-device-position (dx dy medium (elt coord-seq i) (elt coord-seq (+ i 1)))
               (clog:move-to ctx dx dy))
             (with-device-position (dx dy medium (elt coord-seq (+ i 2)) (elt coord-seq (+ i 3)))
               (clog:line-to ctx dx dy))
             (clog:path-stroke ctx))))

(defun %trace-polygon (ctx medium coord-seq closed)
  "Emit a canvas path tracing COORD-SEQ (alternating x y).  Caller is
responsible for begin-path and the fill/stroke step."
  (loop for i from 0 below (length coord-seq) by 2
        for first = t then nil
        do (with-device-position (dx dy medium (elt coord-seq i) (elt coord-seq (1+ i)))
             (if first (clog:move-to ctx dx dy) (clog:line-to ctx dx dy))))
  (when closed (clog:close-path ctx)))

(defmethod medium-draw-polygon* ((medium clog-medium) coord-seq closed filled)
  (with-clipped-context (ctx medium)
    (clog:begin-path ctx)
    (%trace-polygon ctx medium coord-seq (or closed filled))
    (cond (filled (apply-fill-state ctx medium)   (clog:path-fill ctx))
          (t      (apply-stroke-state ctx medium)  (clog:path-stroke ctx)))))

(defmethod medium-draw-rectangle* ((medium clog-medium) left top right bottom filled)
  (with-clipped-context (ctx medium)
    (with-device-position (dl dt medium left top)
      (with-device-position (dr db medium right bottom)
        (let ((w (- dr dl)) (h (- db dt)))
          (cond (filled (apply-fill-state ctx medium)   (clog:fill-rect ctx dl dt w h))
                (t      (apply-stroke-state ctx medium)  (clog:stroke-rect ctx dl dt w h))))))))

(defmethod medium-draw-rectangles* ((medium clog-medium) position-seq filled)
  (loop for i from 0 below (length position-seq) by 4
        do (medium-draw-rectangle* medium
                                   (elt position-seq i) (elt position-seq (+ i 1))
                                   (elt position-seq (+ i 2)) (elt position-seq (+ i 3))
                                   filled)))

(defmethod medium-draw-ellipse* ((medium clog-medium) center-x center-y
                                 radius-1-dx radius-1-dy radius-2-dx radius-2-dy
                                 start-angle end-angle filled)
  ;; CLIM specifies an ellipse by a centre and two radius vectors R1, R2 that
  ;; need not be orthogonal (a general/skewed ellipse).  Rather than reduce
  ;; them to canvas ellipse()'s radius-x / radius-y / rotation -- which is only
  ;; exact when R1 _|_ R2 -- we install the affine map that sends the unit
  ;; circle to the ellipse and draw a unit arc through it.  A point (cos a,
  ;; sin a) on the unit circle maps to centre + cos a * R1 + sin a * R2, i.e.
  ;; the canvas matrix [a b c d e f] = [R1x R1y R2x R2y Cx Cy].  This is exact
  ;; for any (including skewed) radii.  The clip path was already rasterised in
  ;; device space before this transform, so it still applies correctly.
  (with-clipped-context (ctx medium)
    (with-device-position (cx cy medium center-x center-y)
      (clog:canvas-save ctx)
      (unwind-protect
           (progn
             (clog:transform ctx
                             (%js radius-1-dx) (%js radius-1-dy)
                             (%js radius-2-dx) (%js radius-2-dy)
                             cx cy)
             (clog:begin-path ctx)
             (clog:arc ctx 0 0 1 (%js start-angle) (%js end-angle))
             (cond (filled (apply-fill-state ctx medium)  (clog:path-fill ctx))
                   (t      (apply-stroke-state ctx medium) (clog:path-stroke ctx))))
        (clog:canvas-restore ctx)))))

(defun %text-align->css (align-x)
  "Map a CLIM ALIGN-X keyword to a canvas textAlign value."
  (case align-x
    (:right "right")
    (:center "center")
    (t "left")))

(defun %text-baseline->css (align-y)
  "Map a CLIM ALIGN-Y keyword to a canvas textBaseline value.
CLIM's default text alignment is :BASELINE; the canvas equivalent is
\"alphabetic\".  Honouring this is what places, e.g., the LABELLING pane's
label correctly instead of drawing it at the baseline and clipping the top."
  (case align-y
    (:top "top")
    ((:center :middle) "middle")
    (:bottom "bottom")
    (t "alphabetic")))

(defmethod medium-draw-text* ((medium clog-medium) string x y
                              start end align-x align-y
                              toward-x toward-y transform-glyphs)
  (declare (ignore toward-x toward-y transform-glyphs))
  ;; Unlike the other MEDIUM-DRAW-* generics, MEDIUM-DRAW-TEXT* has *no*
  ;; active TRANSFORM-COORDINATES-MIXIN :around method in McCLIM (its
  ;; definition is guarded by #+(or), i.e. disabled), so X/Y arrive in the
  ;; medium's *user* coordinate system with the medium transformation NOT yet
  ;; applied.  DREI/output-record replay positions a text run by setting the
  ;; medium transformation to translate to the run's origin and then drawing
  ;; the string at (0,0); we must therefore apply MEDIUM-TRANSFORMATION here
  ;; ourselves before mapping through the sheet's native transformation.
  ;; Without this, every text run lands at the pane origin -- outside its own
  ;; clip rectangle -- and is invisible (e.g. typed text in the interactor).
  (multiple-value-bind (sx sy)
      (transform-position (medium-transformation medium) x y)
    (with-clipped-context (ctx medium)
      (let ((str (subseq string (or start 0) (or end (length string)))))
        (setf (clog:font-style ctx) (text-style->css (medium-text-style medium)))
        (setf (clog:text-align ctx) (%text-align->css align-x))
        (setf (clog:text-baseline ctx) (%text-baseline->css align-y))
        (apply-fill-state ctx medium)
        (with-device-position (dx dy medium sx sy)
          (clog:fill-text ctx str dx dy))))))

(defmethod medium-clear-area ((medium clog-medium) left top right bottom)
  (with-clipped-context (ctx medium)
    (with-device-position (dl dt medium left top)
      (with-device-position (dr db medium right bottom)
        (clog:clear-rect ctx dl dt (- dr dl) (- db dt))))))

;;; ------------------------------------------------------------------
;;; Pixmaps and MEDIUM-COPY-AREA (Phase 3)
;;;
;;; COPY-AREA / COPY-{TO,FROM}-PIXMAP all funnel through MEDIUM-COPY-AREA,
;;; dispatched on the source and destination drawables (a medium or a
;;; pixmap).  The HTML5 canvas DRAW-IMAGE that CLOG exposes has no source
;;; sub-rectangle form, so we move a rectangle of pixels with GET-IMAGE-DATA
;;; (read sx,sy,w,h from the source context) + PUT-IMAGE-DATA (write at dx,dy
;;; on the destination context).  PUT-IMAGE-DATA writes raw device pixels --
;;; ignoring the current transform, clip and compositing -- which is exactly
;;; the copy-area semantics, and our drawing contexts use an identity
;;; transform (coordinates arrive already in device space), so no adjustment
;;; is needed beyond mapping medium (user) coordinates into device space.
;;; ------------------------------------------------------------------

(defmethod allocate-pixmap ((medium clog-medium) width height)
  (let ((body (clog-port-body (port medium)))
        (w (max 1 (ceiling width)))
        (h (max 1 (ceiling height))))
    (unless body
      (error "clim-clog: cannot allocate a pixmap without a CLOG body."))
    (let* ((canvas  (clog:create-canvas body :width w :height h :hidden t))
           (context (clog:create-context2d canvas)))
      (make-instance 'clog-pixmap :canvas canvas :context context
                                  :width w :height h))))

(defun %clog-blit (from-ctx fx fy w h to-ctx tx ty)
  "Copy the W x H pixel rectangle at (FX,FY) in FROM-CTX to (TX,TY) in TO-CTX.
All coordinates are device pixels; FROM-CTX and TO-CTX may be the same."
  (let ((iw (max 1 (round w)))
        (ih (max 1 (round h))))
    (when (and from-ctx to-ctx)
      (let ((data (clog:get-image-data from-ctx (round fx) (round fy) iw ih)))
        (clog:put-image-data to-ctx data (round tx) (round ty))))))

(defmethod medium-copy-area ((from clog-medium) from-x from-y width height
                             (to clog-medium) to-x to-y)
  (multiple-value-bind (dfx dfy)
      (transform-position (medium-native-transformation from) from-x from-y)
    (multiple-value-bind (dtx dty)
        (transform-position (medium-native-transformation to) to-x to-y)
      (multiple-value-bind (w h)
          (transform-distance (medium-transformation from) width height)
        (%clog-blit (medium-drawable from) dfx dfy w h
                    (medium-drawable to) dtx dty)))))

(defmethod medium-copy-area ((from clog-medium) from-x from-y width height
                             (to clog-pixmap) to-x to-y)
  (multiple-value-bind (dfx dfy)
      (transform-position (medium-native-transformation from) from-x from-y)
    (multiple-value-bind (w h)
        (transform-distance (medium-transformation from) width height)
      (%clog-blit (medium-drawable from) dfx dfy w h
                  (clog-pixmap-context to) to-x to-y))))

(defmethod medium-copy-area ((from clog-pixmap) from-x from-y width height
                             (to clog-medium) to-x to-y)
  (multiple-value-bind (dtx dty)
      (transform-position (medium-native-transformation to) to-x to-y)
    (%clog-blit (clog-pixmap-context from) from-x from-y width height
                (medium-drawable to) dtx dty)))

(defmethod medium-copy-area ((from clog-pixmap) from-x from-y width height
                             (to clog-pixmap) to-x to-y)
  (%clog-blit (clog-pixmap-context from) from-x from-y width height
              (clog-pixmap-context to) to-x to-y))

;;; ------------------------------------------------------------------
;;; Text metrics (Phase 3: real browser metrics)
;;;
;;; CLIM asks for text-size synchronously during layout.  The browser holds
;;; the real answer, so we round-trip CLOG:MEASURE-TEXT over the websocket
;;; and cache it (browser metrics are stable for the life of a connection),
;;; keyed by the CSS-font string.  When no drawable context exists yet (e.g.
;;; space composition before the mirror's canvas is realized) we fall back to
;;; the conservative offline estimate so layout still works.
;;; ------------------------------------------------------------------

(defun %offline-font-px (text-style)
  "Conservative pixel size for TEXT-STYLE used by the offline metric fallback."
  (let ((size (nth-value 2 (clim:text-style-components text-style))))
    (etypecase size
      (number size)
      (null 12)
      (symbol (case size
                (:tiny 8) (:very-small 9) (:small 10)
                (:normal 12) (:large 16) (:very-large 20)
                (:huge 24) (t 12))))))

(defun %measuring-context (medium)
  "Return a CLOG context usable for MEASURE-TEXT, or NIL if none exists yet.
Prefers the medium's offscreen drawable; falls back to the port context."
  (let ((ctx (medium-drawable medium)))
    (and (typep ctx 'clog:clog-context2d) ctx)))

(defun %font-metrics (medium text-style)
  "Return (values ascent descent em-width) for TEXT-STYLE from the browser,
cached per CSS-font string on the port.  Falls back to an offline estimate
when no measuring context is available.

ASCENT/DESCENT come from MEASURE-TEXT's font-bounding-box values (font-level,
independent of the sample text); EM-WIDTH is the measured advance of \"m\"."
  (let* ((css   (text-style->css text-style))
         (cache (clog-port-text-metrics-cache (port medium)))
         (key   (cons :font css))
         (cached (gethash key cache)))
    (if cached
        (values-list cached)
        (let ((ctx (%measuring-context medium)))
          (if (null ctx)
              ;; No surface yet: estimate, and do NOT cache (so the real
              ;; metrics get measured once a context exists).
              (let ((px (%offline-font-px text-style)))
                (values (* 0.8 px) (* 0.2 px) (* 0.6 px)))
              (progn
                (setf (clog:font-style ctx) css)
                (let* ((tm (clog:measure-text ctx "m"))
                       (asc (clog:font-bounding-box-ascent tm))
                       (desc (clog:font-bounding-box-descent tm))
                       (emw (clog:width tm)))
                  (setf (gethash key cache) (list asc desc emw))
                  (values asc desc emw))))))))

(defun %char-width (medium text-style char)
  "Measured advance width of CHAR in TEXT-STYLE, cached per (css . char)."
  (let* ((css   (text-style->css text-style))
         (cache (clog-port-text-metrics-cache (port medium)))
         (key   (cons css char)))
    (or (gethash key cache)
        (let ((ctx (%measuring-context medium)))
          (if (null ctx)
              (* 0.6 (%offline-font-px text-style))
              (progn
                (setf (clog:font-style ctx) css)
                (let ((w (clog:width (clog:measure-text ctx (string char)))))
                  (setf (gethash key cache) w))))))))

(defmethod text-style-ascent (text-style (medium clog-medium))
  (values (%font-metrics medium text-style)))

(defmethod text-style-descent (text-style (medium clog-medium))
  (nth-value 1 (%font-metrics medium text-style)))

(defmethod text-style-height (text-style (medium clog-medium))
  (multiple-value-bind (asc desc) (%font-metrics medium text-style)
    (+ asc desc)))

(defmethod text-style-character-width (text-style (medium clog-medium) char)
  (%char-width medium text-style char))

(defmethod text-style-width (text-style (medium clog-medium))
  (nth-value 2 (%font-metrics medium text-style)))

(defun %line-width (medium text-style string start end)
  "Advance width of STRING[start:end] (no newlines) as the sum of cached
per-character widths.  This is the latency-critical path: TEXT-SIZE is called
on the input line on *every* keystroke during DREI redisplay.  Summing cached
per-glyph widths means each distinct character costs one MEASURE-TEXT
round-trip exactly once for the life of the connection, after which typing
needs no synchronous websocket round-trips at all -- whereas measuring the
whole substring would block on a round-trip per keystroke."
  (loop with w = 0
        for i from start below end
        do (incf w (%char-width medium text-style (char string i)))
        finally (return w)))

(defmethod text-size ((medium clog-medium) string &key text-style (start 0) end)
  (setf string (etypecase string
                 (character (string string))
                 (string string)))
  (let* ((ts    (or text-style (medium-text-style medium)))
         (end   (or end (length string))))
    (multiple-value-bind (asc desc) (%font-metrics medium ts)
      (let* ((line-height (+ asc desc)))
        ;; Honour embedded newlines the way the CLIM TEXT-SIZE contract
        ;; requires: WIDTH is the widest line, HEIGHT spans every line, and
        ;; the final cursor position is relative to the last line.
        (if (find #\Newline string :start start :end end)
            (loop with max-w = 0 and lines = 0 and last-w = 0
                  for ls = start then (1+ le)
                  for le = (or (position #\Newline string :start ls :end end) end)
                  do (let ((lw (%line-width medium ts string ls le)))
                       (setf max-w (max max-w lw) last-w lw)
                       (incf lines))
                  until (>= le end)
                  finally (return
                            (values max-w (* lines line-height)
                                    last-w (* (1- lines) line-height) asc)))
            (let ((w (%line-width medium ts string start end)))
              (values w line-height w 0 asc)))))))

;;; Bounding rectangle of STRING relative to the text origin (baseline at
;;; y=0): the box extends BASELINE above (negative y) and the remaining
;;; height below.  Required by the output-recording machinery during
;;; redisplay.  Estimated from our offline metrics for now.
(defmethod text-bounding-rectangle* ((medium clog-medium) string
                                     &key text-style (start 0) end)
  (multiple-value-bind (width height cursor-dx cursor-dy baseline)
      (text-size medium string :text-style text-style :start start :end end)
    (declare (ignore cursor-dx cursor-dy))
    (values 0 (- baseline) width (- height baseline))))

;;; ------------------------------------------------------------------
;;; Output flushing -- the double-buffer page-flip.
;;;
;;; The medium draws into the mirror's offscreen canvas; here we blit it to
;;; the visible canvas in a single DRAW-IMAGE.  This is what eliminates the
;;; redisplay flash: a full redisplay clears and repaints the offscreen
;;; buffer (invisible), and only the finished frame appears on screen.  The
;;; bare-medium Phase-0 harness has no mirror and draws straight to the
;;; visible canvas, so flushing is a no-op there.
;;; ------------------------------------------------------------------

(defun %medium-flush (medium)
  (let ((mirror (medium-clog-mirror medium)))
    (when mirror
      (clog-mirror-flush mirror))))

(defmethod medium-finish-output ((medium clog-medium)) (%medium-flush medium))
(defmethod medium-force-output ((medium clog-medium)) (%medium-flush medium))
(defmethod medium-beep ((medium clog-medium)) nil)
(defmethod medium-miter-limit ((medium clog-medium)) 0)
