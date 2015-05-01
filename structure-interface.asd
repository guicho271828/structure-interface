#|
  This file is a part of structure-interface project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  Non-CLOS, compile-time, inlined, fast method dispatching system

  Author: Masataro Asai (guicho2.71828@gmail.com)
|#



(in-package :cl-user)
(defpackage structure-interface-asd
  (:use :cl :asdf))
(in-package :structure-interface-asd)


(defsystem structure-interface
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:lisp-namespace :immutable-struct :trivia :alexandria)
  :components ((:module "src"
                :components
                ((:file "package"))))
  :description "Non-CLOS, compile-time, inlined, fast method dispatching system"
  :in-order-to ((test-op (load-op :structure-interface.test))))
