#|
  This file is a part of structure-interface project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage structure-interface.test-asd
  (:use :cl :asdf))
(in-package :structure-interface.test-asd)


(defsystem structure-interface.test
  :author "Masataro Asai"
  :license "LLGPL"
  :depends-on (:structure-interface
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (load-op :after (op c) (eval (read-from-string "(every #'fiveam::TEST-PASSED-P (5am:run! :structure-interface))"))
))
