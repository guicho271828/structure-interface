#|
  This file is a part of structure-interface project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage structure-interface
  (:use :cl :trivia :alexandria)
  (:shadowing-import-from  :immutable-struct :ftype :defstruct)
  (:export
   #:define-interface
   #:implement-interface
   #:import-interface
   #:shadowing-import-interface))
(in-package :structure-interface)

;;; define-interface

(defstruct interface
  (typevars (error "no typevars") :type list) ;of symbols
  (methods (error "no methods") :type list)   ;of symbols
  (hash (make-hash-table :test 'equal) :type hash-table))

(defun expander-fn-name (name)
  (let ((*package* (symbol-package name)))
    (symbolicate name '-type)))

(lisp-namespace:define-namespace interface interface)

(defmacro define-interface (name typevars (&body methods) &key (export t) (documentation ""))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (symbol-interface ',name)
           (interface ',typevars ',(mapcar #'first methods)))
     ,@(mapcar (lambda-ematch
                 ((list name body)
                  (let ((expander (expander-fn-name name)))
                    `(progn
                       ,@(when export `((export ',name)))
                       (defun ,expander ,typevars ,body)
                       (deftype ,name ,typevars (,expander ,@typevars))))))
               methods)
     (eval '(define-generic-functions ',name))
     ,(dummy-form name typevars
                  (format nil "~a~2%The macro is a dummy macro for slime integration."
                          documentation))))

(defun dummy-form (name typevars string)
  `(defmacro ,name (,@typevars)
     ,string
     (declare (ignore ,@typevars))
     (error "dummy macro!")))

(defun check-impl (methods impl)
  (assert (= (length methods) (length impl))
          nil
          "mismatch in interface/implementation"))

(defun check-args (typevars typevals)
  (assert (= (length typevars) (length typevals))
          nil
          "mismatch in interface typevars"))

;;; implement-interface

(defmacro implement-interface ((name &rest typevals) &key (export t))
  (ematch (symbol-interface name)
    ((interface typevars methods hash)
     (let ((implementations
            (mapcar (lambda (x) (intern (string x)))
                    methods)))
       (check-impl methods implementations)
       (check-args typevars typevals)
       (setf (gethash typevals hash) implementations)
       `(eval-when (:compile-toplevel :load-toplevel :execute)
          ,(declaim-method-types methods implementations typevals)
          ,@(when export `((export ',implementations)))
          ,(define-generic-functions name))))))

(defun declaim-method-types (methods implementations typevals)
  `(declaim ,@(mapcar (lambda (method impl)
                        `(cl:ftype (,method ,@typevals) ,impl))
                      methods
                      implementations)))

(deftype lambda-keyword () 'symbol)
(defun lambda-keywordp (obj)
  (match obj
    ((symbol (name (string* #\&))) t)))

(defun /lk (list)
  (remove-if #'lambda-keywordp list))

(defun define-generic-functions (name)
  ;; recompile the generic version of the function.
  ;; dispatch is implemented with pattern matcher.
  ;; always inlined and dispatch is done in compile time as much as possible
  ;; FIXME: dirty handling of lambda keywords
  (ematch (symbol-interface name)
    ((interface typevars methods hash)
     `(progn
        ,@(mapcar (lambda (m i)
                    ;; for each method, redefine a new generic function
                    (let* ((expander (symbol-function (expander-fn-name m)))
                           (args (mapcar (lambda (x)
                                           (if (lambda-keywordp x) x (gensym)))
                                         (second (apply expander typevars))))
                           (args/lk (/lk args))
                           arg-type-list
                           result-type-list
                           (body (let (clauses)
                                   (maphash
                                    (lambda (typevals impl-function-names)
                                      ;; run type-expand and get the arguments types and result types
                                      (push (ematch (apply expander typevals)
                                              ((list 'function arg-type result-type)
                                               (push arg-type arg-type-list)
                                               (push result-type result-type-list)
                                               (let ((arg-type/lk (/lk arg-type)))
                                                 `(,(mapcar (lambda (type)
                                                              (match type
                                                                ((list* 'function _)
                                                                 `(type function))
                                                                (_
                                                                 `(type ,type))))
                                                            arg-type/lk)
                                                    (,(elt impl-function-names i) ,@args/lk)))))
                                            clauses))
                                    hash)
                                   (nreverse clauses))))
                      `(progn
                         ,@(when arg-type-list
                             `((ftype ,m ,@(apply #'mapcar (lambda (&rest args)
                                                             (if (lambda-keywordp (car args))
                                                                 (car args)
                                                                 `(or ,@args)))
                                                  arg-type-list) t)))
                         (declaim (inline ,m))
                         (defun-ematch* ,m ,args ,@body))))
                  methods (iota (length methods)))))))



;;; import/shadowing-import-interface

(defmacro import-interface (name)
  `(import ',(interface-methods (symbol-interface name))))

(defmacro shadowing-import-interface (name)
  `(shadowing-import ',(interface-methods (symbol-interface name))))

