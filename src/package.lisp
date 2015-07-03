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

(defun expander-fn (name)
  (symbol-function (expander-fn-name name)))

(lisp-namespace:define-namespace interface interface)

(defmacro define-interface (name typevars
                            (&whole methods (method ftype &key) &rest rest)
                            &key (documentation ""))
  "
FIXME: Bad interface design!!

 (options per method)

 (options per interface)
"
  (declare (ignore method ftype rest))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (symbol-interface ',name)
           (interface ',typevars
                      ',(mapcar #'first methods)))
     ,@(mapcar (lambda-ematch
                 ((list* name body keys)
                  (let ((expander (expander-fn-name name)))
                    `(progn
                       (defun ,expander ,typevars ,body)
                       (deftype ,name ,typevars (,expander ,@typevars))))))
               methods)
     (eval (define-generic-functions ',name))
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
          "Assertion (= (length ~a) (length ~a)) failed: ~
           mismatch in interface/implementation!" methods impl))

(defun check-args (typevars typevals)
  (assert (= (length typevars) (length typevals))
          nil
          "Assertion (= (length ~a) (length ~a)) failed: ~
           mismatch in interface typevars"  typevars typevals))

;;; implement-interface

(defmacro implement-interface ((name &rest typevals)
                               &key
                                 inherit)
  (ematch (symbol-interface name)
    ((interface typevars methods hash)
     (let ((implementations
            (mapcar (lambda (x) (intern (string x)))
                    methods)))
       (check-impl methods implementations)
       (check-args typevars typevals)
       `(eval-when (:compile-toplevel :load-toplevel :execute)
          ,(declaim-method-types methods implementations typevals)
          ,(when inherit
             (check-args typevars inherit)
             (define-specialied-functions
                 methods
                 implementations (or (gethash inherit hash)
                                     (error "implementation of ~s is not defined for ~s.~% ~s"
                                            name inherit
                                            (hash-table-plist hash)))
                 typevals inherit))
          (setf (gethash ',typevals (interface-hash (symbol-interface ',name)))
                ',implementations)
          (eval (define-generic-functions ',name)))))))

(defun declaim-method-types (methods implementations typevals)
  `(declaim ,@(mapcar (lambda (method impl)
                        `(cl:ftype (,method ,@typevals) ,impl))
                      methods
                      implementations)))
(defun declaim-inline (implementations)
  `(declaim ,@(mapcar (lambda (impl) `(inline ,impl)) implementations)))
(defun declaim-notinline (implementations)
  `(declaim ,@(mapcar (lambda (impl) `(notinline ,impl)) implementations)))

(defun define-specialied-functions (methods
                                    impls inherited-impls
                                    typevals inherited-typevals)
  (mapc (lambda (t1 t2)
          (assert (subtypep t1 t2) nil
                  "~a does not specializes ~a: ~a is not the subtype of ~a"
                  typevals inherited-typevals
                  t1 t2))
        typevals inherited-typevals)
  `(progn 
     ,@(mapcar (lambda (m i1 i2)
                 (let ((args (mapcar (lambda (x)
                                       (if (lambda-keywordp x) x (gensym)))
                                     (second (apply (expander-fn m) typevals)))))
                   `(defun ,i1 ,args
                      ,(format nil "inherited from ~s" i2)
                      (declare (inline ,i2))
                      (,i2 ,@args))))
               methods impls inherited-impls)))


(deftype lambda-keyword () 'symbol)
(defun lambda-keywordp (obj)
  (match obj
    ((symbol (name (string* #\&))) t)))

(defun /lk (list)
  (remove-if #'lambda-keywordp list))

(defun define-generic-functions (name &optional)
  ;; recompile the generic version of the function.
  ;; dispatching is implemented with pattern matcher.
  ;; always inlined and dispatch is done in compile time as much as possible
  ;; FIXME: dirty handling of lambda keywords
  (ematch (symbol-interface name)
    ((interface typevars methods hash)
     `(progn
        ,@(mapcar (lambda (m i)
                    ;; for each method, redefine a new generic function
                    (let* ((expander (expander-fn m))
                           (args (mapcar (lambda (x)
                                           (if (lambda-keywordp x) x (gensym)))
                                         (second (apply expander typevars))))
                           triples)
                      (maphash
                       (lambda (typevals impl-function-names)
                         ;; run type-expand and get the arguments types and result types
                         (ematch (apply expander typevals)
                           ((list 'function arg-type result-type)
                            (push (list arg-type result-type (elt impl-function-names i))
                                  triples))))
                       hash)
                      (setf triples (apply-precedence-order triples))
                      `(progn
                         ,@(when triples
                             `((ftype ,m ,@(apply #'mapcar (lambda (&rest args)
                                                             (if (lambda-keywordp (car args))
                                                                 (car args)
                                                                 `(or ,@args)))
                                                  (mapcar #'first triples)) t)))
                         (defun ,m ,args ,(make-body (/lk args) triples)))))
                  methods (iota (length methods)))))))

(defun apply-precedence-order (triples)
  (sort (copy-list triples)
        #'precedence-order
        :key #'first))

(defun-ematch* precedence-order (types1 types2)
  ((nil nil) t)
  (((list* t1 r1) (list* t2 r2))
   (cond
     ((subtypep t1 t2) nil)
     ((subtypep t2 t1) t)
     (t (precedence-order r1 r2)))))

(defun make-body (args triples)
  `(ematch* ,args
     ,@(mapcar (lambda-ematch
                 ((list argtype _ method)
                  (let ((argtype/lk (/lk argtype)))
                    `(,(mapcar (lambda (type)
                                 (match type
                                   ((list* 'function _)
                                    `(type function))
                                   (_
                                    `(type ,type))))
                               argtype/lk)
                       (,method ,@args)))))
               triples)))


;;; import/shadowing-import-interface

(defmacro import-interface (name)
  `(import ',(interface-methods (symbol-interface name))))

(defmacro shadowing-import-interface (name)
  `(shadowing-import ',(interface-methods (symbol-interface name))))

;;; specialize-interface

;; is it necessary? inline and specialize...

