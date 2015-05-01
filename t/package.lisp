#|
  This file is a part of structure-interface project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :structure-interface.test
  (:use :cl
        :structure-interface
        :fiveam
        :lisp-namespace :trivia :alexandria))
(in-package :structure-interface.test)



(def-suite :structure-interface)
(in-suite :structure-interface)

;; run test with (run! test-name) 

(test structure-interface

  )



