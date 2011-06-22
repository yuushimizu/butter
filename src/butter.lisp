(in-package :cl-user)
(defpackage :butter
  (:use :cl :butter.util)
  (:export :test-context
           :test-context-name
           :test-context-parent
           :test-condition
           :test-condition-context
	   :test-passed
	   :test-failed
	   :test-failed-message

           :pass-test
           :fail-test
           :ignore-test

           :pass
           :fail

           :in-test-context
           :with-test-result-handlers
           :in-assertion-context
	   :assertion-form-expand
	   :define-assertion-type

	   :ok
           :ok-each
           :ok-call
           :<-

	   :test-not-found-error
           :named-test-not-found-error
           :package-test-not-found-error
	   :test-functions
	   :test-function
	   :deftest
	   :run-test
	   :test-names))
(in-package :butter)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass test-context ()
    ((name :initarg :name :reader test-context-name)
     (parent :initarg :parent :initform nil :reader test-context-parent)))
  (define-condition test-condition (condition)
    ((context :initarg :context :reader test-condition-context)))
  (define-condition test-passed (test-condition) ())
  (define-condition test-failed (test-condition)
    ((message :initarg :message :reader test-failed-message))
    (:report (lambda (condition stream)
               (format stream "The test ~A was failed with the message ~S." (test-context-name (test-condition-context condition)) (test-failed-message condition)))))

  (define-condition test-context-required (condition) ())
  (defun call-with-test-restarts (name function)
    (let* ((parent-context (restart-case
                               (progn (signal 'test-context-required)
                                      nil)
                             (continue-with-context (context) context)))
           (current-context (make-instance 'test-context :name name :parent parent-context)))
      (restart-case
          (restart-case
              (handler-bind ((test-context-required (lambda (condition)
                                                      (declare (ignore condition))
                                                      (invoke-restart 'continue-with-context current-context))))
                (funcall function))
            (pass-test ()
              (signal 'test-passed :context current-context)
              t)
            (fail-test (&optional (message ""))
              (signal (make-condition 'test-failed :context current-context :message message))
              nil))
        (ignore-test () nil))))
  (defun pass ()
    (invoke-restart 'pass-test))
  (defun fail (message)
    (invoke-restart 'fail-test message))
  (defmacro in-test-context (name &body body)
    `(call-with-test-restarts ',name (lambda () ,@body)))
  (defun call-with-test-result-handlers (function &key passed failed)
    (handler-bind ((test-passed (or passed #'identity))
                   (test-failed (or failed #'identity)))
      (funcall function)))
  (defmacro with-test-result-handlers (form &rest handlers &key passed failed)
    (declare (ignore passed failed))
    `(call-with-test-result-handlers (lambda () ,form)
                                        ,@handlers))
  (defun call-with-assertion-handlers (function)
    (with-test-result-handlers
        (funcall function)
      :passed (lambda (condition)
                (declare (ignore condition))
                (invoke-restart 'ignore-test))
      :failed (lambda (condition)
                (invoke-restart 'fail-test (test-failed-message condition)))))
  (defmacro with-assertion-handlers (&body body)
    `(call-with-assertion-handlers (lambda () ,@body)))
  (defmacro in-assertion-context (name &body body)
    `(in-test-context ,name (with-assertion-handlers ,@body) (pass)))

  (defgeneric assertion-form-expand (assertion-type arguments))
  (defmacro define-standard-assertion-form-expand ((type-variable type-specializer) (&rest arguments-lambda-list) &body body)
    (with-gensyms (arguments%)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (defmethod assertion-form-expand ((,type-variable ,type-specializer) ,arguments%)
           (destructuring-bind ,arguments-lambda-list ,arguments%
             `(in-assertion-context (,,type-variable ,@,arguments%) ,,@body))))))
  (defmacro define-assertion-type (type (&rest lambda-list) &body body)
    (with-gensyms (type%)
      `(define-standard-assertion-form-expand (,type% (eql ',type)) ,lambda-list ,@body)))
  (defmacro ok (type &rest arguments)
    (assertion-form-expand type arguments))
  
  (define-assertion-type t (expression)
    `(unless ,expression (fail ,(format nil "~S is nil." expression))))
  (define-standard-assertion-form-expand (type cons) (&rest arguments)
    `(ok ,@type ,@arguments))
  (define-standard-assertion-form-expand (type null) (&rest arguments)
    `(ok ,@arguments))
  (define-standard-assertion-form-expand (function symbol) (&rest arguments)
    (let ((argument-variables (mapcar (lambda (argument) (if (keywordp argument) argument (gensym (princ-to-string argument))))
                                      arguments)))
      `(let ,(mapcan (lambda (variable argument) `((,variable ,argument))) argument-variables arguments)
         (with-test-result-handlers (ok t (,function ,@argument-variables))
           :failed (lambda (condition)
                     (declare (ignore condition))
                     (fail (format nil "(~A~{ ~S~}) is nil." ',function (list ,@argument-variables))))))))
  (defmethod assertion-form-expand ((name string) arguments)
    `(in-assertion-context ,(format nil "~A: ~S" name arguments)
                           (ok ,@arguments)))
  (defmacro ok-each (assertion-type &rest argument-lists)
    `(progn ,@(mapcar (lambda (arguments) `(ok ,@(cons-or-append assertion-type arguments)))
                      argument-lists)))
  (defmacro ok-call (function assertion-type &rest argument-lists)
    (labels ((split-arguments (rest &optional (left ()))
               (if (or (not rest) (eq '<- (car rest)))
                   (values (reverse left) (cdr rest))
                   (split-arguments (cdr rest) (cons (car rest) left)))))
      `(progn ,@(mapcar (lambda (arguments)
                          (multiple-value-bind (assertion-arguments function-arguments) (split-arguments arguments)
                            `(ok ,@(cons-or-append assertion-type assertion-arguments)
                                 (,function ,@function-arguments))))
                        argument-lists))))
  (define-assertion-type :type (type value) `(ok typep ,value ',type))
  (define-assertion-type :condition (condition-type &body body)
    `(handler-case (progn ,@body (fail ,(format nil "An expected condition ~A was not signalled." condition-type)))
       (,condition-type ())))
  
  (define-condition test-not-found-error (error) ())
  (define-condition named-test-not-found-error (test-not-found-error)
    ((test-name :initarg :test-name :reader named-test-not-found-error-test-name))
    (:report (lambda (condition stream)
               (format stream "The test ~S is not found." (named-test-not-found-error-test-name condition)))))
  (define-condition package-test-not-found-error (test-not-found-error)
    ((package :initarg :package :reader package-test-not-found-error-package))
    (:report (lambda (condition stream)
               (format stream "No tests found in package ~A." (package-name (package-test-not-found-error-package condition))))))
  (let ((test-functions-in-packages (make-hash-table :test 'equal)))
    (defun test-functions (package)
      (or (gethash (package-name package) test-functions-in-packages)
          (error 'package-test-not-found-error :package package)))
    (defun test-function (name package)
      (or (gethash name (test-functions package))
          (error 'named-test-not-found-error :test-name name)))
    (defun (setf test-function) (function name package)
      (let ((test-functions (or (gethash (package-name package) test-functions-in-packages)
                                (setf (gethash (package-name package) test-functions-in-packages) (make-hash-table)))))
        (setf (gethash name test-functions) function))))
  (defmacro deftest (name &body body)
    `(progn
       (setf (test-function ',name *package*) (lambda () (in-test-context ,name ,@body)))
       ',name))
  (defun run-test (name package)
    (funcall (test-function name package)))
  (defun test-names (package)
    (loop for name being each hash-key in (test-functions package) collect name)))
