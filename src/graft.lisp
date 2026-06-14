;;;; graft.lisp
;;;;
;;;; A GRAFT is a special sheet directly connected to the display server --
;;;; conceptually the "root window".  For clim-clog the graft represents
;;;; the drawing surface as a whole; its dimensions are the canvas size.

(in-package #:clim-clog)

(defclass clog-graft (graft)
  ())

(defmethod graft-width ((graft clog-graft) &key (units :device))
  (declare (ignore units))
  (clog-port-width (port graft)))

(defmethod graft-height ((graft clog-graft) &key (units :device))
  (declare (ignore units))
  (clog-port-height (port graft)))
