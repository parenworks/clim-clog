;;;; clim-clog.asd
;;;;
;;;; ASDF system definitions for clim-clog: a McCLIM backend that renders
;;;; to an HTML5 <canvas> in a web browser, driven over CLOG's websocket
;;;; connection.
;;;;
;;;; See README.org for goals and status, and DESIGN.org for the
;;;; architecture and the CLIM -> CLOG mapping.

(in-package #:asdf-user)

(defsystem "clim-clog"
  :description "A McCLIM backend that draws to an HTML5 canvas via CLOG."
  :author "Glenn Thompson"
  :license "BSD-3-Clause"
  :version "0.0.1"
  ;; clim-core is the portable McCLIM core (same dependency mcclim-null
  ;; uses); clog supplies the browser connection and the canvas API.
  :depends-on ("clim-core" "clog")
  :pathname "src"
  :serial nil
  :components
  ((:file "package")
   (:file "port"          :depends-on ("package"))
   (:file "graft"         :depends-on ("package" "port"))
   (:file "medium"        :depends-on ("package" "port"))
   (:file "input"         :depends-on ("package" "port"))
   (:file "frame-manager" :depends-on ("package" "port" "medium"))))

(defsystem "clim-clog/examples"
  :description "Runnable differential-test harness for the clim-clog backend."
  :depends-on ("clim-clog" "clog")
  :pathname "examples"
  :components ((:file "hello")))
