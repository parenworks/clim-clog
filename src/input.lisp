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
  (let ((pointer (climi::port-pointer port))
        (x (getf data :x))
        (y (getf data :y)))
    ;; Cache the pointer's screen position.  Presentation highlighting and
    ;; pointer documentation synthesize a motion event at (POINTER-POSITION
    ;; pointer); CLOG reports canvas-relative coordinates, which are our
    ;; graft/screen coordinates, so we record them here on every event.
    (setf (pointer-position pointer) (values x y))
    (let ((args (list :sheet sheet
                      :pointer pointer
                      :x x
                      :y y
                      :modifier-state (clog-modifier-state data)
                      :timestamp (get-internal-real-time))))
      (when buttonp
        (setf args (list* :button (clog-button data) args)))
      (apply #'make-instance class args))))

;;; ------------------------------------------------------------------
;;; Keyboard: browser KeyboardEvent.key -> CLIM keysym name + character
;;; ------------------------------------------------------------------

(defparameter *clog-key-name-map*
  '(("Enter"      . :return)
    ("Backspace"  . :backspace)
    ("Tab"        . :tab)
    ("Escape"     . :escape)
    ("Delete"     . :delete)
    ("Insert"     . :insert)
    ("ArrowLeft"  . :left)
    ("ArrowRight" . :right)
    ("ArrowUp"    . :up)
    ("ArrowDown"  . :down)
    ("Home"       . :home)
    ("End"        . :end)
    ("PageUp"     . :prior)
    ("PageDown"   . :next)
    ("Shift"      . :shift-left)
    ("Control"    . :control-left)
    ("Alt"        . :meta-left)
    ("Meta"       . :meta-left)
    ("CapsLock"   . :caps-lock)
    ("F1" . :f1) ("F2" . :f2) ("F3" . :f3)  ("F4" . :f4)
    ("F5" . :f5) ("F6" . :f6) ("F7" . :f7)  ("F8" . :f8)
    ("F9" . :f9) ("F10" . :f10) ("F11" . :f11) ("F12" . :f12))
  "Map browser KeyboardEvent.key strings for non-printable keys to the CLIM
keysym-name keywords (the X11 keysym names McCLIM uses, e.g. :RETURN).")

;;; A few non-printable keys must carry an explicit character, because McCLIM
;;; matches the input-editor's editing commands against the event's *character*
;;; (e.g. forward-delete is bound to #\Rubout) and only derives that character
;;; from the keysym name when the keysym table supplies one.  The Delete key is
;;; the notable casualty: standard-keys.lisp defines :DELETE first with the
;;; #\Delete character (DEFEDIT) and then *re-defines* it with no character
;;; (DEFNAVI), so the derived character is lost and Delete matches nothing.  We
;;; supply #\Rubout (code 127, == #\Delete) ourselves so the keysym clobbering
;;; in McCLIM cannot strand the key.
(defparameter *clog-key-char-map*
  '(("Delete" . #\Rubout))
  "Browser KeyboardEvent.key strings that must deliver an explicit character
even though they are not printable, keyed for the gesture matcher.")

(defun clog-key->name+char (key)
  "Translate a browser KeyboardEvent.key string KEY to two values: the CLIM
keysym name (a keyword) and the character to attach (or NIL).

CLIM names the lower-case 'a' key :|a| and the upper-case key :|A|, so for a
single-character KEY the keysym name is simply that character interned into
the keyword package; multi-character names (\"Enter\", \"ArrowLeft\", ...) are
looked up in *CLOG-KEY-NAME-MAP*.  A handful of named keys (*CLOG-KEY-CHAR-MAP*)
additionally carry an explicit character so the gesture matcher can find their
editing command even when McCLIM's keysym table derives none."
  (cond
    ((or (null key) (zerop (length key)))
     (values :void-symbol nil))
    ((string= key " ")
     (values :space #\Space))
    ((= (length key) 1)
     (values (intern key :keyword) (char key 0)))
    (t
     (values (or (cdr (assoc key *clog-key-name-map* :test #'string=))
                 (intern (string-upcase key) :keyword))
             (cdr (assoc key *clog-key-char-map* :test #'string=))))))

(defun make-clog-key-event (class sheet data)
  "Construct a CLIM keyboard event of CLASS for SHEET from a CLOG keyboard
event plist DATA.  :KEY-CHARACTER is supplied only for printable keys; for
the rest McCLIM derives the character (if any) from the keysym name."
  (multiple-value-bind (key-name char)
      (clog-key->name+char (getf data :key))
    (let ((args (list :sheet sheet
                      :key-name key-name
                      :modifier-state (clog-modifier-state data)
                      :timestamp (get-internal-real-time))))
      (when char
        (setf args (list* :key-character char args)))
      (apply #'make-instance class args))))

;;; ------------------------------------------------------------------
;;; Handler installation
;;; ------------------------------------------------------------------

(defun install-clog-input-handlers (port sheet canvas)
  "Attach CLOG pointer and keyboard handlers on CANVAS that enqueue CLIM
events for SHEET (the top-level mirrored sheet) onto PORT's queue.

A <canvas> is not focusable by default, so it receives no keyboard events.
We give it tab-index 0 and focus it; the browser then routes keydown/keyup
to it (and re-focuses it on click)."
  (flet ((pointer-handler (class buttonp)
           (lambda (obj data)
             (declare (ignore obj))
             (handler-case
                 (clog-enqueue-event
                  port (make-clog-pointer-event class sheet port data buttonp))
               (error (e)
                 (format *error-output* "~&[clim-clog input] ~a~%" e)))))
         (key-handler (class)
           (lambda (obj data)
             (declare (ignore obj))
             (handler-case
                 (clog-enqueue-event port (make-clog-key-event class sheet data))
               (error (e)
                 (format *error-output* "~&[clim-clog input] ~a~%" e))))))
    (clog:set-on-mouse-down canvas (pointer-handler 'pointer-button-press-event   t))
    (clog:set-on-mouse-up   canvas (pointer-handler 'pointer-button-release-event t))
    (clog:set-on-mouse-move canvas (pointer-handler 'pointer-motion-event         nil))
    ;; Make the canvas keyboard-focusable and start capturing keystrokes.
    ;; DISABLE-DEFAULT keeps Tab/Backspace/arrow keys inside the app instead
    ;; of letting the browser act on them (focus traversal, history, scroll).
    (setf (clog:tab-index canvas) 0)
    (clog:set-on-key-down canvas (key-handler 'key-press-event) :disable-default t)
    (clog:set-on-key-up   canvas (key-handler 'key-release-event))
    (clog:focus canvas))
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

;;; ------------------------------------------------------------------
;;; Presentation highlighting on pointer motion
;;;
;;; McCLIM drives hover-highlighting from the command loop's input-context
;;; wait handler (CLIMI::FRAME-INPUT-CONTEXT-WAIT-HANDLER).  That handler
;;; fires only when a pointer-motion gesture lands in the *command-loop
;;; stream's* input buffer -- i.e. the stream READ-FRAME-COMMAND reads from,
;;; which is the frame's first application (or interactor) pane.  In a
;;; multi-pane frame the pointer usually sits over a *different* pane, so the
;;; command-loop stream never sees the motion, the wait handler is never
;;; reached, and nothing is ever highlighted.  (A single-pane frame happens
;;; to work because its one pane is also the command-loop stream.)
;;;
;;; Our port I/O loop delivers every event through DISTRIBUTE-EVENT; the
;;; standard command loop then pulls each event off the shared frame queue
;;; and runs HANDLE-EVENT on the sheet under the pointer *with*
;;; *INPUT-CONTEXT* and *APPLICATION-FRAME* already bound.  We exploit that
;;; here: on each motion event we re-run the wait handler against the pane
;;; actually under the pointer, which highlights the applicable presentation
;;; (CLIMI::FRAME-INPUT-CONTEXT-WAIT-HANDLER no-ops the redraw unless the
;;; presentation under the pointer changed) or unhighlights when there is
;;; none.  Leaving the pane clears any lingering highlight.
;;; ------------------------------------------------------------------

(defmethod handle-event :after ((sheet clim-stream-pane) (event pointer-motion-event))
  (let ((frame *application-frame*))
    (when (and frame *input-context*)
      (climi::frame-input-context-wait-handler frame sheet event))))

(defmethod handle-event :after ((sheet clim-stream-pane) (event pointer-exit-event))
  (unhighlight-highlighted-presentation sheet))
