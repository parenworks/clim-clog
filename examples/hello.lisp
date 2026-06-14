;;;; hello.lisp
;;;;
;;;; A runnable smoke test / differential-test harness for the clim-clog
;;;; backend, in the spirit of the McCLIM HTML5 backend tutorial.
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
;;;; This is a Phase-0 (output-only) harness.  There is no input handling
;;;; yet -- clicking the canvas does nothing.  See DESIGN.md for the
;;;; roadmap to interactivity.

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
  (:panes (canvas (clim:make-pane 'click-canvas :scroll-bars nil)))
  (:layouts (default canvas)))

(defun start-input-demo (&key (port 8080))
  "Start the CLOG server serving the interactive CLOG-INPUT-DEMO frame.
Click anywhere in the page and a red marker should appear at the click."
  (clog:initialize (lambda (body) (run-frame-in-window 'clog-input-demo body))
                   :port port)
  (clog:open-browser :url (format nil "http://127.0.0.1:~d/" port)))
