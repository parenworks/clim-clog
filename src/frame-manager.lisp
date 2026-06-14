;;;; frame-manager.lisp
;;;;
;;;; The FRAME-MANAGER realises application frames and their panes for a
;;;; port.  In a full backend it would lay out gadgets as mirrored sheets
;;;; (canvases / DOM nodes).  For the output-only spike we provide the
;;;; minimal subclass so a port can be constructed; ADOPT-FRAME is a no-op,
;;;; matching the Null backend.  Real frame/pane realisation is Phase 1 of
;;;; the Roadmap (see DESIGN.md).

(in-package #:clim-clog)

(defclass clog-frame-manager (standard-frame-manager)
  ())

(defmethod adopt-frame :after ((fm clog-frame-manager) (frame application-frame))
  ())

(defmethod note-space-requirements-changed :after ((graft clog-graft) pane)
  (declare (ignore pane))
  ())
