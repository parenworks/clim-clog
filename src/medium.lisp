;;;; medium.lisp
;;;;
;;;; The MEDIUM maintains the drawing context for a sheet, and the actual
;;;; drawing operations (MEDIUM-DRAW-LINE*, MEDIUM-DRAW-TEXT*, ...) are
;;;; specialised on it.  This is where CLIM's drawing protocol is mapped
;;;; onto CLOG's HTML5 canvas API.
;;;;
;;;; The "device object" we draw on -- the mirror, returned by
;;;; MEDIUM-DRAWABLE -- is a CLOG-CONTEXT2D.  Where the McCLIM HTML5
;;;; tutorial emits JavaScript strings like "context.lineTo(x, y)", we
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
;;;; STATUS (v0.0.1): output-only spike.  Inks are handled for solid
;;;; colours; transformations are applied to coordinates; line dashes and
;;;; full clipping-region fidelity are Roadmap items (see DESIGN.md).

(in-package #:clim-clog)

(defclass clog-medium (basic-medium)
  ())

;;; The device object the medium draws on: the 2D context of the canvas
;;; mirroring this medium's sheet.  Child panes share their mirrored
;;; ancestor's (the top-level sheet's) canvas, so we walk up to it.  Falls
;;; back to the port-level context for the bare-medium Phase-0 harness,
;;; which has no sheet hierarchy.
(defmethod medium-drawable ((medium clog-medium))
  (let* ((sheet (medium-sheet medium))
         (ancestor (and sheet (sheet-mirrored-ancestor sheet)))
         (mirror (and ancestor (sheet-direct-mirror ancestor))))
    ;; A real frame's mirror is a CLOG-MIRROR; the bare-medium Phase-0
    ;; harness draws on a graft whose direct mirror is just T, so fall back
    ;; to the port-level context there.
    (if (typep mirror 'clog-mirror)
        (clog-mirror-context mirror)
        (clog-port-context (port medium)))))

;;; ------------------------------------------------------------------
;;; Helpers: CLIM -> CSS
;;; ------------------------------------------------------------------

(defun ink->css (ink)
  "Translate a CLIM INK to a CSS colour string.

Only solid colours are handled in the spike; anything else falls back to
black.  Patterns, gradients, and the indirect inks (+foreground-ink+ etc.)
are Roadmap items."
  (if (typep ink 'clim:color)
      (multiple-value-bind (r g b) (clim:color-rgb ink)
        (format nil "rgb(~d,~d,~d)"
                (round (* 255 r)) (round (* 255 g)) (round (* 255 b))))
      "black"))

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

(defmacro with-device-position ((dx dy medium x y) &body body)
  "Bind DX/DY to the device coordinates for X/Y, coerced JS-safe via %JS.

NOTE: McCLIM's standard medium drawing generics apply the medium
transformation via :around methods (transform-coordinates-mixin) *before*
dispatching to our primary methods, so the X/Y we receive are already in
device space.  We therefore pass them through unchanged -- transforming
again here would apply the transformation twice (cf. the warning in
Backends/Null/medium.lisp).  MEDIUM is accepted for symmetry/future use."
  (declare (ignore medium))
  `(let ((,dx (%js ,x)) (,dy (%js ,y)))
     ,@body))

(defun apply-stroke-state (ctx medium)
  "Push MEDIUM's ink and line thickness onto the context as stroke state."
  (setf (clog:stroke-style ctx) (ink->css (medium-ink medium)))
  (setf (clog:line-width ctx)
        (max 1 (round (line-style-thickness (medium-line-style medium))))))

(defun apply-fill-state (ctx medium)
  "Push MEDIUM's ink onto the context as fill state."
  (setf (clog:fill-style ctx) (ink->css (medium-ink medium))))

;;; ------------------------------------------------------------------
;;; Drawing operations
;;; ------------------------------------------------------------------

(defmethod medium-draw-point* ((medium clog-medium) x y)
  (let ((ctx (medium-drawable medium))
        (r (max 0.5 (/ (line-style-thickness (medium-line-style medium)) 2))))
    (with-device-position (dx dy medium x y)
      (apply-fill-state ctx medium)
      (clog:begin-path ctx)
      (clog:arc ctx dx dy (%js r) 0 (%js (* 2 pi)))
      (clog:path-fill ctx))))

(defmethod medium-draw-points* ((medium clog-medium) coord-seq)
  (loop for i from 0 below (length coord-seq) by 2
        do (medium-draw-point* medium (elt coord-seq i) (elt coord-seq (1+ i)))))

(defmethod medium-draw-line* ((medium clog-medium) x1 y1 x2 y2)
  (let ((ctx (medium-drawable medium)))
    (apply-stroke-state ctx medium)
    (clog:begin-path ctx)
    (with-device-position (dx dy medium x1 y1) (clog:move-to ctx dx dy))
    (with-device-position (dx dy medium x2 y2) (clog:line-to ctx dx dy))
    (clog:path-stroke ctx)))

(defmethod medium-draw-lines* ((medium clog-medium) coord-seq)
  (let ((ctx (medium-drawable medium)))
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
  (let ((ctx (medium-drawable medium)))
    (clog:begin-path ctx)
    (%trace-polygon ctx medium coord-seq (or closed filled))
    (cond (filled (apply-fill-state ctx medium)   (clog:path-fill ctx))
          (t      (apply-stroke-state ctx medium)  (clog:path-stroke ctx)))))

(defmethod medium-draw-rectangle* ((medium clog-medium) left top right bottom filled)
  (let ((ctx (medium-drawable medium)))
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
  ;; CLIM specifies an ellipse by a centre and two radius vectors.  We map
  ;; that onto the HTML5 canvas ellipse(): the two radius lengths give
  ;; radius-x / radius-y, and the angle of the first radius vector gives the
  ;; rotation.  This is exact for an axis-aligned-or-rotated ellipse (the
  ;; common case); a general skewed (non-orthogonal radii) ellipse is
  ;; approximated by it.
  (let* ((ctx (medium-drawable medium))
         (rx (sqrt (+ (* radius-1-dx radius-1-dx) (* radius-1-dy radius-1-dy))))
         (ry (sqrt (+ (* radius-2-dx radius-2-dx) (* radius-2-dy radius-2-dy))))
         (rotation (atan radius-1-dy radius-1-dx)))
    (with-device-position (cx cy medium center-x center-y)
      (clog:begin-path ctx)
      (clog:ellipse ctx cx cy (%js rx) (%js ry) (%js rotation)
                    (%js start-angle) (%js end-angle))
      (cond (filled (apply-fill-state ctx medium)  (clog:path-fill ctx))
            (t      (apply-stroke-state ctx medium) (clog:path-stroke ctx))))))

(defmethod medium-draw-text* ((medium clog-medium) string x y
                              start end align-x align-y
                              toward-x toward-y transform-glyphs)
  (declare (ignore align-x align-y toward-x toward-y transform-glyphs))
  (let ((ctx (medium-drawable medium))
        (str (subseq string (or start 0) (or end (length string)))))
    (setf (clog:font-style ctx) (text-style->css (medium-text-style medium)))
    (apply-fill-state ctx medium)
    (with-device-position (dx dy medium x y)
      (clog:fill-text ctx str dx dy))))

(defmethod medium-clear-area ((medium clog-medium) left top right bottom)
  (let ((ctx (medium-drawable medium)))
    (with-device-position (dl dt medium left top)
      (with-device-position (dr db medium right bottom)
        (clog:clear-rect ctx dl dt (- dr dl) (- db dt))))))

;;; ------------------------------------------------------------------
;;; Text metrics
;;;
;;; CLOG:MEASURE-TEXT returns a TextMetrics object; for the spike we use a
;;; conservative monospace-ish estimate so layout works offline, and leave
;;; round-tripping the real measurement to the browser as a Roadmap item
;;; (it requires a synchronous query over the websocket).
;;; ------------------------------------------------------------------

(defmethod text-style-ascent (text-style (medium clog-medium))
  (* 0.8 (text-style-height text-style medium)))

(defmethod text-style-descent (text-style (medium clog-medium))
  (* 0.2 (text-style-height text-style medium)))

(defmethod text-style-height (text-style (medium clog-medium))
  (let ((size (nth-value 2 (clim:text-style-components text-style))))
    (if (numberp size) size 12)))

(defmethod text-style-character-width (text-style (medium clog-medium) char)
  (declare (ignore char))
  (* 0.6 (text-style-height text-style medium)))

(defmethod text-style-width (text-style (medium clog-medium))
  (text-style-character-width text-style medium #\m))

(defmethod text-size ((medium clog-medium) string &key text-style (start 0) end)
  (setf string (etypecase string
                 (character (string string))
                 (string string)))
  (let* ((ts (or text-style (medium-text-style medium)))
         (height (text-style-height ts medium))
         (n (- (or end (length string)) start))
         (width (* n (text-style-character-width ts medium #\m)))
         (baseline (text-style-ascent ts medium)))
    (values width height width 0 baseline)))

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
;;; Output flushing -- CLOG sends eagerly, so these are no-ops.
;;; ------------------------------------------------------------------

(defmethod medium-finish-output ((medium clog-medium)) nil)
(defmethod medium-force-output ((medium clog-medium)) nil)
(defmethod medium-beep ((medium clog-medium)) nil)
(defmethod medium-miter-limit ((medium clog-medium)) 0)
