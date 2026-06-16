;;;; hello.lisp
;;;;
;;;; A runnable smoke test / differential-test harness for the clim-clog
;;;; backend, in the spirit of the turtleware McCLIM-backends tutorials
;;;; (https://turtleware.eu/tag/clim.html).
;;;;
;;;; It opens a CLOG window, creates a <canvas> + 2D context, wraps them in
;;;; a clim-clog PORT/GRAFT/MEDIUM, and exercises the MEDIUM-DRAW-* methods
;;;; directly.  If the shapes and text appear in the browser, the CLIM ->
;;;; CLOG drawing mapping works.
;;;;
;;;; Usage:
;;;;   (ql:quickload :clim-clog/examples)
;;;;   (clim-clog::start)            ; opens http://127.0.0.1:8080
;;;;
;;;; Beyond the Phase-0 bare-medium demo (START), this file also drives the
;;;; Phase-1 application frame (START-FRAME), Phase-2 pointer input
;;;; (START-INPUT-DEMO), and keyboard input (START-KEY-DEMO).  See DESIGN.org
;;;; for the roadmap.

(in-package #:clim-clog)

(defun make-clog-medium-for-context (body context &key (width 800) (height 600))
  "Build a clim-clog PORT/GRAFT/MEDIUM trio that renders onto CONTEXT, a
CLOG-CONTEXT2D created under the CLOG object BODY.  Returns the medium.

This bypasses the application-frame/pane machinery (Phase 1) and gives a
bare medium for testing the drawing protocol in isolation."
  (let ((port (find-port :server-path (list :clog :body body
                                            :width width :height height))))
    (setf (clog-port-context port) context)
    (let* ((graft (find-graft :port port))
           (medium (make-medium port graft)))
      medium)))

(defun draw-demo (medium)
  "Exercise every implemented MEDIUM-DRAW-* operation once."
  (medium-clear-area medium 0 0 800 600)
  ;; Filled + outlined rectangles.
  (setf (medium-ink medium) clim:+steel-blue+)
  (medium-draw-rectangle* medium 50 50 250 150 t)
  (setf (medium-ink medium) clim:+black+)
  (medium-draw-rectangle* medium 300 50 500 150 nil)
  ;; A thick line.
  (setf (medium-ink medium) clim:+red+
        (medium-line-style medium) (clim:make-line-style :thickness 3))
  (medium-draw-line* medium 50 200 500 200)
  ;; A filled polygon.
  (setf (medium-ink medium) clim:+forest-green+)
  (medium-draw-polygon* medium #(550 60 760 90 710 210 560 190) t t)
  ;; A filled ellipse (radius-x 90, radius-y 45 -- visibly not a circle).
  (setf (medium-ink medium) clim:+purple+)
  (medium-draw-ellipse* medium 200 360 90 0 0 45 0 (* 2 pi) t)
  ;; Some points.
  (setf (medium-ink medium) clim:+orange+
        (medium-line-style medium) (clim:make-line-style :thickness 8))
  (medium-draw-points* medium #(300 320 340 360 380 320))
  ;; Text.
  (setf (medium-ink medium) clim:+black+
        (medium-text-style medium) (clim:make-text-style :sans-serif :bold 28))
  (medium-draw-text* medium "Hello from CLIM via CLOG!"
                     50 470 0 nil 0 0 0 0 nil))

(defun on-new-window (body)
  "CLOG on-new-window handler: build the canvas and render the demo.

The drawing is wrapped so a runtime error is reported (to the page and the
server console) rather than aborting the CLOG worker thread -- this keeps
the server alive while iterating on the medium."
  (handler-case
      (let* ((canvas  (clog:create-canvas body :width 800 :height 600))
             (context (clog:create-context2d canvas))
             (medium  (make-clog-medium-for-context body context)))
        (setf (clog-port-canvas (port medium)) canvas)
        (draw-demo medium))
    (error (e)
      (format *error-output* "~&[clim-clog demo] draw error: ~a~%" e)
      (ignore-errors
       (clog:create-p body :content (format nil "clim-clog demo error: ~a" e))))))

(defun start (&key (port 8080))
  "Start the CLOG server and open a browser on the demo page."
  (clog:initialize #'on-new-window :port port)
  (clog:open-browser :url (format nil "http://127.0.0.1:~d/" port)))

(defun stop ()
  "Shut the CLOG server down."
  (clog:shutdown))

;;; ------------------------------------------------------------------
;;; Phase 1: a real CLIM application-frame on a CLOG page.
;;;
;;; Unlike the bare-medium demo above, this goes through the full McCLIM
;;; machinery: a frame, a frame-manager, a mirrored top-level sheet (a
;;; CLOG canvas), pane layout, and the repaint/redisplay protocol.  The
;;; display function uses high-level CLIM drawing operations, which McCLIM
;;; routes to our CLOG medium.
;;; ------------------------------------------------------------------

(defun display-canvas (frame pane)
  "Display function for the demo frame's application pane."
  (declare (ignore frame))
  (clim:draw-rectangle* pane 50 50 250 150 :ink clim:+steel-blue+)
  (clim:draw-rectangle* pane 300 50 500 150 :filled nil :ink clim:+black+)
  (clim:draw-line* pane 50 200 500 200 :ink clim:+red+ :line-thickness 3)
  (clim:draw-polygon* pane '(550 60 760 90 710 210 560 190)
                      :ink clim:+forest-green+ :closed t :filled t)
  (clim:draw-text* pane "Hello from a CLIM frame via CLOG!" 50 360
                   :ink clim:+black+
                   :text-style (clim:make-text-style :sans-serif :bold 24)))

(clim:define-application-frame clog-demo ()
  ()
  ;; No menu bar: this is the minimal static-output demo; gadget/menu
  ;; rendering is exercised by CLOG-GADGET-DEMO (START-GADGET-DEMO).
  (:menu-bar nil)
  (:panes (canvas :application
                  :display-function 'display-canvas
                  :scroll-bars nil))
  (:layouts (default canvas)))

(defun run-frame-in-window (frame-class body)
  "Run application frame FRAME-CLASS in this browser tab.  A fresh port is
bound to this connection's BODY so REALIZE-MIRROR creates the canvas in the
right document."
  (handler-case
      (let* ((port (find-port :server-path
                              (list :clog :body body :width 820 :height 640)))
             (fm   (find-frame-manager :port port))
             (frame (make-application-frame frame-class
                                            :frame-manager fm
                                            :width 800 :height 600)))
        (run-frame-top-level frame))
    (error (e)
      (format *error-output* "~&[clim-clog frame] error: ~a~%" e)
      (ignore-errors
       (uiop:print-condition-backtrace e :stream *error-output*))
      (ignore-errors
       (clog:create-p body :content (format nil "clim-clog frame error: ~a" e))))))

(defun frame-on-new-window (body)
  "CLOG handler that runs the static CLOG-DEMO frame."
  (run-frame-in-window 'clog-demo body))

(defun start-frame (&key (port 8080))
  "Start the CLOG server serving the static CLOG-DEMO application frame."
  (clog:initialize #'frame-on-new-window :port port)
  (clog:open-browser :url (format nil "http://127.0.0.1:~d/" port)))

;;; ------------------------------------------------------------------
;;; Phase 2: input.  A pane that draws a marker wherever you click,
;;; proving the path browser pointer event -> CLIM event queue ->
;;; DISTRIBUTE-EVENT -> HANDLE-EVENT on the sheet -> draw via the medium.
;;; ------------------------------------------------------------------

(defclass click-canvas (clim:clim-stream-pane) ()
  (:documentation "An interactive pane that marks pointer clicks."))

(defmethod clim:handle-event ((pane click-canvas)
                              (event clim:pointer-button-press-event))
  (let ((x (clim:pointer-event-x event))
        (y (clim:pointer-event-y event)))
    (clim:draw-circle* pane x y 8 :ink clim:+red+)
    (clim:draw-text* pane (format nil "(~d, ~d)" (round x) (round y))
                     (+ x 12) y
                     :ink clim:+black+
                     :text-style (clim:make-text-style :sans-serif :roman 12))
    (finish-output pane)))

(clim:define-application-frame clog-input-demo ()
  ()
  (:menu-bar nil)
  (:panes (canvas (clim:make-pane 'click-canvas :scroll-bars nil)))
  (:layouts (default canvas)))

(defun start-input-demo (&key (port 8080))
  "Start the CLOG server serving the interactive CLOG-INPUT-DEMO frame.
Click anywhere in the page and a red marker should appear at the click."
  (clog:initialize (lambda (body) (run-frame-in-window 'clog-input-demo body))
                   :port port)
  (clog:open-browser :url (format nil "http://127.0.0.1:~d/" port)))

;;; ------------------------------------------------------------------
;;; Keyboard input: a pane that echoes typed text, proving the path
;;; browser keydown -> CLIM key-press-event -> queue -> DISTRIBUTE-EVENT
;;; -> HANDLE-EVENT.  Printable keys append; Backspace deletes; Return
;;; starts a new line; Escape clears.
;;; ------------------------------------------------------------------

(defclass type-canvas (clim:clim-stream-pane)
  ((lines :initform (list "") :accessor type-canvas-lines))
  (:documentation "A pane that accumulates typed characters and shows them."))

(defun type-canvas-redraw (pane)
  (let ((w (clim:bounding-rectangle-width pane))
        (h (clim:bounding-rectangle-height pane))
        (prompt-style (clim:make-text-style :sans-serif :roman 14))
        (text-style   (clim:make-text-style :fix :roman 20)))
    (clim:draw-rectangle* pane 0 0 w h :ink clim:+white+)
    (clim:draw-text* pane "Click here, then type.  Backspace deletes, Return = newline, Escape clears."
                     20 28 :ink (clim:make-rgb-color 0.4 0.4 0.4)
                     :text-style prompt-style)
    (loop for line in (reverse (type-canvas-lines pane))
          for y from 70 by 28
          do (clim:draw-text* pane line 20 y :ink clim:+black+
                              :text-style text-style))
    (finish-output pane)))

(defun type-canvas-redraw-line (pane)
  "Repaint only the current (bottom-most) line's row, instead of clearing and
redrawing the whole canvas.  This is what keeps each keystroke from flashing
the entire drawing area -- a poor-man's incremental redisplay."
  (let* ((lines (type-canvas-lines pane))
         (y (+ 70 (* 28 (1- (length lines)))))
         (w (clim:bounding-rectangle-width pane)))
    (clim:draw-rectangle* pane 0 (- y 22) w (+ y 8) :ink clim:+white+)
    (clim:draw-text* pane (first lines) 20 y :ink clim:+black+
                     :text-style (clim:make-text-style :fix :roman 20))
    (finish-output pane)))

(defmethod clim:handle-repaint ((pane type-canvas) region)
  (declare (ignore region))
  (type-canvas-redraw pane))

(defmethod clim:handle-event ((pane type-canvas) (event clim:key-press-event))
  (let ((char (clim:keyboard-event-character event))
        (name (clim:keyboard-event-key-name event))
        (lines (type-canvas-lines pane))
        (full nil))
    (case name
      (:backspace
       (let ((cur (first lines)))
         (if (plusp (length cur))
             (setf (first lines) (subseq cur 0 (1- (length cur))))
             ;; Removing an empty line shifts every row up: needs a full repaint.
             (when (rest lines)
               (setf (type-canvas-lines pane) (rest lines) full t)))))
      (:return
       (push "" (type-canvas-lines pane)))
      (:escape
       (setf (type-canvas-lines pane) (list "") full t))
      (t
       (when (and char (graphic-char-p char))
         (setf (first lines)
               (concatenate 'string (first lines) (string char))))))
    ;; Common case (typing, backspace within a line, newline) only repaints the
    ;; current row; structural changes (clear, line removal) repaint fully.
    (if full
        (type-canvas-redraw pane)
        (type-canvas-redraw-line pane))))

(clim:define-application-frame clog-key-demo ()
  ()
  (:menu-bar nil)
  (:panes (canvas (clim:make-pane 'type-canvas :scroll-bars nil)))
  (:layouts (default canvas)))

(defun start-key-demo (&key (port 8080))
  "Start the CLOG server serving the interactive CLOG-KEY-DEMO frame.
Click the canvas to focus it, then type; the text echoes on the page."
  (clog:initialize (lambda (body) (run-frame-in-window 'clog-key-demo body))
                   :port port)
  (clog:open-browser :url (format nil "http://127.0.0.1:~d/" port)))

;;; ------------------------------------------------------------------
;;; Presentation / command loop: clicking a *presented* object runs a
;;; command.  This is the CLIM interaction model proper -- objects are
;;; drawn as presentations of a presentation-type, a command takes an
;;; argument of that type with a :gesture, and McCLIM's command loop turns
;;; a click on the presentation into an invocation of the command.  It
;;; proves the whole input-context machinery works over this backend.
;;; ------------------------------------------------------------------

(clim:define-presentation-type shape ()
  :description "a coloured shape")

(defun shape-color (shape)
  (ecase shape
    (:red-box    clim:+firebrick+)
    (:green-box  clim:+forest-green+)
    (:blue-box   clim:+steel-blue+)))

(defun display-shapes (frame pane)
  "Draw the three shapes, each as a clickable presentation of type SHAPE,
plus the current status message."
  (let ((w (clim:bounding-rectangle-width pane))
        (h (clim:bounding-rectangle-height pane)))
    (clim:draw-rectangle* pane 0 0 w h :ink clim:+white+))
  (clim:draw-text* pane (present-demo-message frame) 20 30
                   :ink clim:+black+
                   :text-style (clim:make-text-style :sans-serif :bold 18))
  (loop for shape in '(:red-box :green-box :blue-box)
        for x from 40 by 170
        do (clim:with-output-as-presentation (pane shape 'shape)
             (clim:draw-rectangle* pane x 70 (+ x 130) 190
                                   :ink (shape-color shape))
             (clim:draw-text* pane (string-downcase (symbol-name shape))
                              (+ x 10) 215 :ink clim:+black+
                              :text-style (clim:make-text-style :sans-serif :roman 14)))))

(clim:define-application-frame clog-present-demo ()
  ((message :initform "Click a shape to run the Pick-Shape command on it."
            :accessor present-demo-message))
  (:menu-bar nil)
  (:panes (app :application
               :display-function 'display-shapes
               :scroll-bars nil))
  (:layouts (default app)))

(define-clog-present-demo-command (com-pick-shape :name "Pick Shape")
    ((shape 'shape :gesture :select))
  (let ((frame clim:*application-frame*))
    (setf (present-demo-message frame)
          (format nil "You picked ~a." (string-downcase (symbol-name shape))))
    (clim:redisplay-frame-pane frame (clim:find-pane-named frame 'app)
                               :force-p t)))

(defun start-present-demo (&key (port 8080))
  "Start the CLOG server serving the CLOG-PRESENT-DEMO frame.  Click a
coloured box: the status line updates via a CLIM command invoked by the
presentation translator.  Hover a box to see the command in the pointer
documentation line."
  (clog:initialize (lambda (body) (run-frame-in-window 'clog-present-demo body))
                   :port port)
  (clog:open-browser :url (format nil "http://127.0.0.1:~d/" port)))

;;; ------------------------------------------------------------------
;;; Phase 3: drawing fidelity.  A static scene that exercises every
;;; fidelity feature added in Phase 3 so the results can be eyeballed in
;;; the browser:
;;;   - dashed lines / outlines (LINE-STYLE-DASHES -> setLineDash)
;;;   - a genuinely skewed ellipse (non-orthogonal radius vectors), drawn
;;;     via the unit-circle affine transform
;;;   - text laid out with real browser metrics (TEXT-SIZE round-trip)
;;;   - a pixmap rendered off-screen with WITH-OUTPUT-TO-PIXMAP and blitted
;;;     back with COPY-FROM-PIXMAP
;;;   - COPY-AREA duplicating a region of the canvas
;;; ------------------------------------------------------------------

(defun display-fidelity (frame pane)
  (declare (ignore frame))
  (let ((w (clim:bounding-rectangle-width pane))
        (h (clim:bounding-rectangle-height pane)))
    (clim:draw-rectangle* pane 0 0 w h :ink clim:+white+))
  (clim:draw-text* pane "Phase 3 drawing-fidelity check" 20 30
                   :ink clim:+black+
                   :text-style (clim:make-text-style :sans-serif :bold 20))
  ;; Dashed line + dashed rectangle outline.
  (clim:draw-line* pane 20 60 500 60
                   :ink clim:+firebrick+
                   :line-thickness 3
                   :line-dashes #(14 8))
  (clim:draw-rectangle* pane 20 80 220 180 :filled nil
                        :ink clim:+steel-blue+
                        :line-thickness 2
                        :line-dashes #(4 4))
  ;; A solid line for contrast (proves dashes are reset per stroke).
  (clim:draw-line* pane 20 200 500 200 :ink clim:+forest-green+ :line-thickness 2)
  ;; A skewed ellipse: radius vectors (110,20) and (-30,70) are not
  ;; orthogonal (dot product -2300), so canvas ellipse()'s rx/ry/rotation
  ;; cannot represent it -- the affine-transform path can.
  (clim:draw-ellipse* pane 360 320 110 20 -30 70
                      :ink clim:+dark-violet+ :filled t)
  (clim:draw-text* pane "skewed ellipse" 280 430
                   :ink clim:+black+
                   :text-style (clim:make-text-style :sans-serif :roman 14))
  ;; Pixmap: draw a small scene off-screen, then blit it onto the pane.
  (let ((pixmap (clim:with-output-to-pixmap (m pane :width 140 :height 100)
                  (clim:draw-rectangle* m 0 0 140 100 :ink clim:+light-goldenrod-yellow+)
                  (clim:draw-circle* m 70 50 38 :ink clim:+chocolate+)
                  (clim:draw-text* m "pixmap" 40 56 :ink clim:+black+
                                   :text-style (clim:make-text-style :sans-serif :bold 16)))))
    (unwind-protect
         (progn
           (clim:copy-from-pixmap pixmap 0 0 140 100 pane 560 80)
           ;; COPY-AREA: duplicate the just-blitted pixmap region lower down.
           (clim:copy-area pane 560 80 140 100 560 220))
      (clim:deallocate-pixmap pixmap)))
  (clim:draw-text* pane "pixmap (top) + copy-area (below)" 540 340
                   :ink clim:+black+
                   :text-style (clim:make-text-style :sans-serif :roman 14)))

(clim:define-application-frame clog-fidelity-demo ()
  ()
  (:menu-bar nil)
  (:panes (app :application
               :display-function 'display-fidelity
               :scroll-bars nil))
  (:layouts (default app)))

(defun start-fidelity-demo (&key (port 8080))
  "Start the CLOG server serving the CLOG-FIDELITY-DEMO frame, a static scene
exercising the Phase 3 fidelity features (dashes, skewed ellipse, real text
metrics, pixmaps, copy-area)."
  (clog:initialize (lambda (body) (run-frame-in-window 'clog-fidelity-demo body))
                   :port port)
  (clog:open-browser :url (format nil "http://127.0.0.1:~d/" port)))

;;; ==================================================================
;;; Showcase: one interactive frame exercising every phase at once --
;;; Phase 0/3 drawing (shapes, colours, dashes, skewed ellipse, text,
;;; pixmap + copy-area) and Phase 2 input (mouse clicks with modifier
;;; reporting, keyboard echo).  State lives in slots; each event redraws
;;; the whole pane and double buffering keeps that flicker-free.  (The
;;; presentation/command loop has its own demo, START-PRESENT-DEMO, as a
;;; custom HANDLE-EVENT pane and the command loop do not share gestures.)
;;; ==================================================================

(defparameter *showcase-shapes*
  '((:circle    . "circle")
    (:square    . "square")
    (:triangle  . "triangle")
    (:dashed    . "dashed")
    (:ellipse   . "skewed")))

(defparameter *showcase-palette*
  (list clim:+firebrick+ clim:+forest-green+ clim:+steel-blue+
        clim:+dark-violet+ clim:+chocolate+ clim:+goldenrod+
        clim:+purple+ clim:+orange-red+))

(defclass showcase-pane (clim:clim-stream-pane)
  ((status :initform "Click a shape, click anywhere (try Ctrl/Shift), or type."
           :accessor showcase-status)
   (typed  :initform "" :accessor showcase-typed)
   (marks  :initform '() :accessor showcase-marks))
  (:default-initargs :scroll-bars nil))

(defun showcase-modifier-string (state)
  (let ((mods '()))
    (unless (zerop (logand state clim:+shift-key+))   (push "Shift" mods))
    (unless (zerop (logand state clim:+control-key+)) (push "Ctrl" mods))
    (unless (zerop (logand state clim:+meta-key+))    (push "Meta" mods))
    (if mods (format nil "~{~a~^+~}" (nreverse mods)) "none")))

(defun showcase-button-string (event)
  (let ((b (clim:pointer-event-button event)))
    (cond ((eql b clim:+pointer-left-button+)   "left")
          ((eql b clim:+pointer-middle-button+) "middle")
          ((eql b clim:+pointer-right-button+)  "right")
          (t "?"))))

(defun draw-showcase-shape (pane kind x y color)
  "Draw shape KIND at top-left (X,Y) in COLOR, inside a ~120x120 cell."
  (ecase kind
    (:circle   (clim:draw-circle* pane (+ x 60) (+ y 60) 50 :ink color))
    (:square   (clim:draw-rectangle* pane x (+ y 10) (+ x 110) (+ y 110) :ink color))
    (:triangle (clim:draw-polygon* pane (list (+ x 60) y (+ x 115) (+ y 110) (+ x 5) (+ y 110))
                                   :ink color :closed t :filled t))
    (:dashed   (clim:draw-rectangle* pane x (+ y 10) (+ x 110) (+ y 110)
                                     :filled nil :ink color :line-thickness 3
                                     :line-dashes #(12 8)))
    (:ellipse  (clim:draw-ellipse* pane (+ x 60) (+ y 60) 55 14 -18 48
                                   :ink color :filled t))))

(defun draw-showcase (pane)
  (let ((w (clim:bounding-rectangle-width pane))
        (h (clim:bounding-rectangle-height pane))
        (title (clim:make-text-style :sans-serif :bold 22))
        (label (clim:make-text-style :sans-serif :roman 13))
        (mono  (clim:make-text-style :fix :roman 18)))
    (clim:draw-rectangle* pane 0 0 w h :ink clim:+white+)
    (clim:draw-text* pane "clim-clog showcase -- shapes, colours, mouse & keyboard"
                     20 32 :ink clim:+black+ :text-style title)
    ;; Row of shapes, each in a different colour.
    (loop for (kind . name) in *showcase-shapes*
          for color in *showcase-palette*
          for x from 20 by 150
          do (draw-showcase-shape pane kind x 60 color)
             (clim:draw-text* pane name (+ x 30) 195
                              :ink clim:+black+ :text-style label))
    ;; Pixmap + copy-area row (guarded so a failure can't blank the scene).
    (ignore-errors
     (let ((pixmap (clim:with-output-to-pixmap (m pane :width 130 :height 90)
                     (clim:draw-rectangle* m 0 0 130 90 :ink clim:+light-goldenrod-yellow+)
                     (clim:draw-circle* m 65 45 34 :ink clim:+chocolate+)
                     (clim:draw-text* m "pixmap" 38 52 :ink clim:+black+
                                      :text-style (clim:make-text-style :sans-serif :bold 16)))))
       (unwind-protect
            (progn
              (clim:copy-from-pixmap pixmap 0 0 130 90 pane 20 220)
              (clim:copy-area pane 20 220 130 90 170 220))
         (clim:deallocate-pixmap pixmap))))
    (clim:draw-text* pane "pixmap + copy-area ->" 320 270
                     :ink clim:+black+ :text-style label)
    ;; Status / typed-text readouts.
    (clim:draw-line* pane 20 330 (- w 20) 330 :ink clim:+gray50+ :line-dashes #(3 5))
    (clim:draw-text* pane (showcase-status pane) 20 360
                     :ink clim:+dark-blue+
                     :text-style (clim:make-text-style :sans-serif :bold 16))
    (clim:draw-text* pane "Typed (click pane first to focus):" 20 400
                     :ink clim:+black+ :text-style label)
    (clim:draw-text* pane (if (plusp (length (showcase-typed pane)))
                              (showcase-typed pane)
                              "<nothing yet>")
                     20 430 :ink clim:+black+ :text-style mono)
    ;; Replay click markers.
    (loop for (mx my mcolor) in (reverse (showcase-marks pane))
          do (clim:draw-circle* pane mx my 7 :ink mcolor)
             (clim:draw-circle* pane mx my 7 :filled nil :ink clim:+black+))
    (finish-output pane)))

;;; Incremental redraws.  Each keystroke / click repaints only the row it
;;; changes instead of the whole scene.  This matters for latency: a full
;;; DRAW-SHOWCASE re-runs the pixmap + copy-area block, and COPY-AREA /
;;; COPY-FROM-PIXMAP each issue a *synchronous* CLOG:GET-IMAGE-DATA websocket
;;; round-trip (plus an off-screen canvas alloc/free) -- doing that per
;;; keystroke is what made typing feel sluggish.  The ordinary draw ops below
;;; are fire-and-forget, so a single-row repaint is effectively instant.

(defun showcase-redraw-status (pane)
  (let ((w (clim:bounding-rectangle-width pane)))
    (clim:draw-rectangle* pane 0 342 w 374 :ink clim:+white+)
    (clim:draw-text* pane (showcase-status pane) 20 360
                     :ink clim:+dark-blue+
                     :text-style (clim:make-text-style :sans-serif :bold 16))
    (finish-output pane)))

(defun showcase-redraw-typed (pane)
  (let ((w (clim:bounding-rectangle-width pane)))
    (clim:draw-rectangle* pane 0 410 w 444 :ink clim:+white+)
    (clim:draw-text* pane (if (plusp (length (showcase-typed pane)))
                              (showcase-typed pane)
                              "<nothing yet>")
                     20 430 :ink clim:+black+
                     :text-style (clim:make-text-style :fix :roman 18))
    (finish-output pane)))

(defmethod clim:handle-event ((pane showcase-pane)
                              (event clim:pointer-button-press-event))
  (let* ((x (clim:pointer-event-x event))
         (y (clim:pointer-event-y event))
         (state (clim:event-modifier-state event))
         (color (if (zerop (logand state clim:+control-key+))
                    clim:+red+ clim:+blue+)))
    (push (list x y color) (showcase-marks pane))
    (setf (showcase-status pane)
          (format nil "~a-click at (~d,~d), modifiers: ~a"
                  (showcase-button-string event) (round x) (round y)
                  (showcase-modifier-string state)))
    ;; Repaint just the status row, then add the one new marker (the others
    ;; are already on screen) -- no full-scene redraw.
    (showcase-redraw-status pane)
    (clim:draw-circle* pane x y 7 :ink color)
    (clim:draw-circle* pane x y 7 :filled nil :ink clim:+black+)
    (finish-output pane)))

(defmethod clim:handle-repaint ((pane showcase-pane) region)
  (declare (ignore region))
  (draw-showcase pane))

(defmethod clim:handle-event ((pane showcase-pane) (event clim:key-press-event))
  (let ((char (clim:keyboard-event-character event))
        (name (clim:keyboard-event-key-name event)))
    (case name
      (:backspace (let ((s (showcase-typed pane)))
                    (when (plusp (length s))
                      (setf (showcase-typed pane) (subseq s 0 (1- (length s)))))))
      (:escape (setf (showcase-typed pane) ""))
      (t (when (and char (graphic-char-p char))
           (setf (showcase-typed pane)
                 (concatenate 'string (showcase-typed pane) (string char))))))
    ;; Only the typed-text row changes per keystroke.
    (showcase-redraw-typed pane)))

(clim:define-application-frame clog-showcase ()
  ()
  (:menu-bar nil)
  (:panes (app (clim:make-pane 'showcase-pane)))
  (:layouts (default app)))

(defun start-showcase (&key (port 8080) (open t))
  "Start the CLOG server serving CLOG-SHOWCASE: an interactive frame showing
shapes, colours, dashes, a skewed ellipse, a pixmap + copy-area, mouse clicks
with modifier reporting, and keyboard echo.  Pass :OPEN NIL to skip launching
a browser (headless)."
  (clog:initialize (lambda (body) (run-frame-in-window 'clog-showcase body))
                   :port port)
  (when open
    (clog:open-browser :url (format nil "http://127.0.0.1:~d/" port))))

;;; ==================================================================
;;; Demo 2 -- gadget smoke-test.  Does the standard McCLIM \"CLIM look\"
;;; gadget set render and respond through the CLOG medium?
;;; STANDARD-FRAME-MANAGER hands us the pane-drawn gadgets (push-button,
;;; toggle-button, slider, ...), which draw themselves via the ordinary
;;; MEDIUM-DRAW-* protocol and post their callbacks through the same event
;;; queue as pointer clicks.  The menu bar is enabled (:menu-bar t) so we
;;; also exercise command-menu rendering.  This is the concrete way to
;;; learn how much of the gadget layer already works over the websocket.
;;; ==================================================================

(defun gadget-demo-refresh (frame)
  (clim:redisplay-frame-pane frame (clim:find-pane-named frame 'status) :force-p t))

(defun display-gadget-status (frame pane)
  (clim:draw-text* pane "Gadget events appear here:" 10 22
                   :ink clim:+gray40+
                   :text-style (clim:make-text-style :sans-serif :roman 12))
  (clim:draw-text* pane (gadget-demo-message frame) 10 48
                   :ink clim:+dark-blue+
                   :text-style (clim:make-text-style :sans-serif :bold 16))
  (finish-output pane))

(clim:define-application-frame clog-gadget-demo ()
  ((message :initform "Click the button, flip the toggle, drag the slider, or use the menu."
            :accessor gadget-demo-message))
  (:menu-bar t)
  (:panes
   (greet :push-button
          :label "Greet"
          :activate-callback
          (lambda (gadget)
            (declare (ignore gadget))
            (let ((frame clim:*application-frame*))
              (setf (gadget-demo-message frame) "Push-button activated.")
              (gadget-demo-refresh frame))))
   (flag :toggle-button
         :label "Enable feature"
         :value nil
         :value-changed-callback
         (lambda (gadget value)
           (declare (ignore gadget))
           (let ((frame clim:*application-frame*))
             (setf (gadget-demo-message frame)
                   (format nil "Toggle-button is now ~:[OFF~;ON~]." value))
             (gadget-demo-refresh frame))))
   (level :slider
          :orientation :horizontal
          :min-value 0 :max-value 100 :value 25
          :show-value-p t :decimal-places 0
          :value-changed-callback
          (lambda (gadget value)
            (declare (ignore gadget))
            (let ((frame clim:*application-frame*))
              (setf (gadget-demo-message frame)
                    (format nil "Slider value: ~d" (round value)))
              (gadget-demo-refresh frame))))
   (status :application
           :display-function 'display-gadget-status
           :incremental-redisplay nil
           :scroll-bars nil
           :min-height 70 :max-height 70))
  (:layouts
   (default
    (clim:vertically (:spacing 8)
      (clim:horizontally (:spacing 8) greet flag)
      (clim:labelling (:label "Slider") level)
      status))))

(define-clog-gadget-demo-command (com-gadget-hello :name "Say Hello" :menu t) ()
  (setf (gadget-demo-message clim:*application-frame*) "Hello from the menu bar!")
  (gadget-demo-refresh clim:*application-frame*))

(define-clog-gadget-demo-command (com-gadget-clear :name "Clear Message" :menu t) ()
  (setf (gadget-demo-message clim:*application-frame*) "(cleared)")
  (gadget-demo-refresh clim:*application-frame*))

(defun start-gadget-demo (&key (port 8080) (open t))
  "Start the CLOG server serving CLOG-GADGET-DEMO: a smoke-test of the standard
McCLIM gadgets (push-button, toggle-button, slider) and the menu bar, each
reporting into a status pane.  Pass :OPEN NIL to skip launching a browser."
  (clog:initialize (lambda (body) (run-frame-in-window 'clog-gadget-demo body))
                   :port port)
  (when open
    (clog:open-browser :url (format nil "http://127.0.0.1:~d/" port))))

;;; ==================================================================
;;; Demo 3 -- the remaining interaction layer: scrolling, an interactor,
;;; and an ACCEPTING-VALUES dialog.  These are the pieces a real CLIM
;;; application (the ubik UI) leans on hardest and that the earlier demos
;;; deliberately avoided.  This demo is exploratory: it shows how much of
;;; the scroller-pane / scroll-bar / dialog machinery already works over
;;; CLOG, and flags what still needs backend work.
;;;   - CLIM:SCROLLING wraps a tall/wide application pane, so the vertical
;;;     and horizontal scroll-bar gadgets should appear and scroll it
;;;     (scrolling repaints the viewport; COPY-AREA could optimise this).
;;;   - An :interactor pane gives the frame a *query-io* stream.
;;;   - \"Edit Values\" (menu / type the command) opens an in-frame
;;;     ACCEPTING-VALUES dialog editing three slots.
;;; ==================================================================

(defun display-scroll-content (frame pane)
  (declare (ignore frame))
  (loop for i from 0 below 40
        for y = (+ 24 (* i 30))
        do (clim:draw-text* pane
                            (format nil "Row ~2,'0d -- the quick brown fox jumps over the lazy dog, ~
                                         and then keeps running well past the right edge of the pane."
                                    i)
                            10 y
                            :ink (if (evenp i) clim:+black+ clim:+steel-blue+)
                            :text-style (clim:make-text-style :sans-serif :roman 15)))
  (finish-output pane))

(clim:define-application-frame clog-controls-demo ()
  ((name    :initform "Glenn"  :accessor controls-name)
   (count   :initform 3        :accessor controls-count)
   (enabled :initform t        :accessor controls-enabled))
  (:menu-bar t)
  (:panes
   (content :application
            :display-function 'display-scroll-content
            :incremental-redisplay nil
            :scroll-bars nil)
   (prefs :application
          :display-function 'display-controls-prefs
          :incremental-redisplay nil
          :scroll-bars nil
          :min-height 60 :max-height 60)
   (doc :interactor :min-height 90 :max-height 90))
  (:layouts
   (default
    (clim:vertically (:spacing 6)
      prefs
      (clim:scrolling (:scroll-bars :both :height 280) content)
      doc))))

(defun display-controls-prefs (frame pane)
  (clim:draw-text* pane
                   (format nil "Prefs -- name: ~s  count: ~d  enabled: ~:[no~;yes~]   (run \"Edit Values\" from the menu)"
                           (controls-name frame) (controls-count frame)
                           (controls-enabled frame))
                   10 30 :ink clim:+black+
                   :text-style (clim:make-text-style :sans-serif :roman 15))
  (finish-output pane))

(define-clog-controls-demo-command (com-edit-values :name "Edit Values" :menu t) ()
  (let* ((frame clim:*application-frame*)
         (name (controls-name frame))
         (count (controls-count frame))
         (enabled (controls-enabled frame)))
    (clim:accepting-values (*query-io* :own-window nil :label "Edit Values")
      (setf name (clim:accept 'string :prompt "Name" :default name))
      (fresh-line *query-io*)
      (setf count (clim:accept 'integer :prompt "Count" :default count))
      (fresh-line *query-io*)
      (setf enabled (clim:accept 'boolean :prompt "Enabled" :default enabled)))
    (setf (controls-name frame) name
          (controls-count frame) count
          (controls-enabled frame) enabled)
    (clim:redisplay-frame-pane frame (clim:find-pane-named frame 'prefs) :force-p t)))

(defun start-controls-demo (&key (port 8080) (open t))
  "Start the CLOG server serving CLOG-CONTROLS-DEMO: scrolling content with
scroll-bars, an interactor pane, and an ACCEPTING-VALUES dialog (menu command
\"Edit Values\").  Exploratory test of the scroller / dialog layer."
  (clog:initialize (lambda (body) (run-frame-in-window 'clog-controls-demo body))
                   :port port)
  (when open
    (clog:open-browser :url (format nil "http://127.0.0.1:~d/" port))))
