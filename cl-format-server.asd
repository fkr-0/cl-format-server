(in-package :asdf-user)

(defsystem "cl-format-server"
  :author "cbadger <cbadger@systemli.org>"
  :version "0.0.2"
  :license "MIT"
  :description "Respond to code format/lint requests"
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")

  ;; Dependencies.
  :depends-on (:usocket :bordeaux-threads :sblint :cl-indentify
                :trivial-formatter :log4cl :fiveam)

  ;; Project stucture.
  :serial t
  :components ((:module "src"
                        :serial t
                        :components ((:file "packages")
                                      (:file "util")
                                      (:file "core")
                                      (:file "client")
                                      (:file "handler")
                                     (:file "cl-format-server"))))

  ;; Build a binary:
  ;; don't change this line.
  :build-operation "program-op"
  ;; binary name: adapt.
  :build-pathname "cl-format-server"
  ;; entry point: here "main" is an exported symbol. Otherwise, use a double ::
  :entry-point "cl-format-server:main")
