;;;; package.lisp
;;;;
;;;; Package definition for the clim-clog backend.
;;;;
;;;; We follow the same package recipe as the bundled McCLIM backends
;;;; (see Backends/Null/package.lisp): use CLIM, CLIM-LISP and
;;;; CLIM-BACKEND so the backend protocol symbols (PORT, GRAFT, MEDIUM,
;;;; MEDIUM-DRAW-* and friends) are available unqualified.
;;;;
;;;; We deliberately do NOT (:use :clog).  Many CLOG canvas operations
;;;; share names with CLIM or CL symbols (e.g. RECT, ARC, FILL-STYLE), so
;;;; CLOG functions are always referenced with the CLOG: prefix to keep
;;;; the mapping explicit and conflict-free.

(defpackage #:clim-clog
  (:use #:clim #:clim-lisp #:clim-backend)
  (:import-from #:climi
                #:maybe-funcall
                ;; Sheet/window mixins we specialise on (Phase 1).
                #:top-level-sheet-mixin
                #:unmanaged-sheet-mixin
                #:mirrored-sheet-mixin
                #:basic-sheet
                ;; Port internals.
                #:frame-managers
                #:port-grafts)
  (:export #:clog-port
           #:clog-graft
           #:clog-medium
           #:clog-mirror
           #:clog-frame-manager
           ;; Harness entry points (see examples/hello.lisp).
           #:make-clog-medium-for-context
           #:*clog-server-path*))
