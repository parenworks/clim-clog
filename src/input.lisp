;;;; input.lisp
;;;;
;;;; Phase 2: input.  CLOG delivers browser pointer/keyboard events to
;;;; handlers that run in CLOG's own worker threads.  Those handlers
;;;; translate the event into a CLIM event object and push it onto the
;;;; port's event queue.  The port I/O loop (started by RESTART-PORT in
;;;; port.lisp) is the single consumer: PROCESS-NEXT-EVENT pops one event
;;;; and DISTRIBUTE-EVENTs it into the CLIM sheet hierarchy, which drives
;;;; the command loop, presentation highlighting, accept, and so on.
;;;;
;;;; Coordinate note: CLOG reports pointer x/y relative to the target
;;;; element (the canvas), which is exactly the native (mirror) coordinate
;;;; system of the top-level sheet.  CLIM derives sheet coordinates from
;;;; these via the sheet's native transformation, so we pass them through
;;;; as the event's :X/:Y (native) values.

(in-package #:clim-clog)

;;; ------------------------------------------------------------------
;;; Event queue (lock-protected FIFO)
;;; ------------------------------------------------------------------

(defun clog-enqueue-event (port event)
  (clim-sys:with-lock-held ((clog-port-event-lock port))
    (setf (clog-port-event-queue port)
          (nconc (clog-port-event-queue port) (list event)))))

(defun clog-dequeue-event (port)
  (clim-sys:with-lock-held ((clog-port-event-lock port))
    (when (clog-port-event-queue port)
      (pop (clog-port-event-queue port)))))

;;; ------------------------------------------------------------------
;;; CLOG event data -> CLIM
;;; ------------------------------------------------------------------

(defun clog-modifier-state (data)
  "Build a CLIM modifier-state bitmask from a CLOG mouse/keyboard plist.
The browser's Alt is mapped to CLIM's meta modifier."
  (logior (if (getf data :shift-key) +shift-key+   0)
          (if (getf data :ctrl-key)  +control-key+ 0)
          (if (getf data :meta-key)  +meta-key+    0)
          (if (getf data :alt-key)   +meta-key+    0)))

(defun clog-button (data)
  "Map CLOG's :WHICH-BUTTON (1/2/3) to a CLIM pointer button constant."
  (case (getf data :which-button)
    (1 +pointer-left-button+)
    (2 +pointer-middle-button+)
    (3 +pointer-right-button+)
    (t +pointer-left-button+)))

(defun make-clog-pointer-event (class sheet port data buttonp)
  "Construct a CLIM pointer event of CLASS for SHEET from a CLOG mouse
event plist DATA.  When BUTTONP, include the :BUTTON slot."
  (let ((args (list :sheet sheet
                    :pointer (climi::port-pointer port)
                    :x (getf data :x)
                    :y (getf data :y)
                    :modifier-state (clog-modifier-state data)
                    :timestamp (get-internal-real-time))))
    (when buttonp
      (setf args (list* :button (clog-button data) args)))
    (apply #'make-instance class args)))

;;; ------------------------------------------------------------------
;;; Handler installation
;;; ------------------------------------------------------------------

(defun install-clog-input-handlers (port sheet canvas)
  "Attach CLOG pointer handlers on CANVAS that enqueue CLIM events for
SHEET (the top-level mirrored sheet) onto PORT's queue."
  (flet ((handler (class buttonp)
           (lambda (obj data)
             (declare (ignore obj))
             (handler-case
                 (clog-enqueue-event
                  port (make-clog-pointer-event class sheet port data buttonp))
               (error (e)
                 (format *error-output* "~&[clim-clog input] ~a~%" e))))))
    (clog:set-on-mouse-down canvas (handler 'pointer-button-press-event   t))
    (clog:set-on-mouse-up   canvas (handler 'pointer-button-release-event t))
    (clog:set-on-mouse-move canvas (handler 'pointer-motion-event         nil)))
  (values))

;;; ------------------------------------------------------------------
;;; The port I/O loop's workhorse
;;; ------------------------------------------------------------------

(defun %clog-deliver (port event)
  "Distribute EVENT into the CLIM sheet hierarchy, logging (not signalling)
errors so a bad event cannot tear down the port I/O loop."
  (handler-case
      (climi::distribute-event (port (event-sheet event)) event)
    (error (e)
      (format *error-output* "~&[clim-clog distribute] ~a~%" e))))

(defmethod process-next-event ((port clog-port) &key wait-function timeout)
  ;; Called repeatedly by the port I/O loop (and, in single-process mode,
  ;; by the frame command loop).  Deliver one queued event if available;
  ;; otherwise honour the wait-function / timeout contract.
  (let ((event (clog-dequeue-event port)))
    (cond
      (event
       (%clog-deliver port event)
       (values t nil))
      ((maybe-funcall wait-function)
       (values nil :wait-function))
      ((and timeout (<= timeout 0))
       (values nil :timeout))
      (t
       (let ((deadline (and timeout
                            (+ (get-internal-real-time)
                               (round (* timeout internal-time-units-per-second))))))
         (loop
           (sleep 0.01)
           (let ((ev (clog-dequeue-event port)))
             (when ev
               (%clog-deliver port ev)
               (return (values t nil))))
           (when (maybe-funcall wait-function)
             (return (values nil :wait-function)))
           (when (and deadline (>= (get-internal-real-time) deadline))
             (return (values nil :timeout)))))))))
