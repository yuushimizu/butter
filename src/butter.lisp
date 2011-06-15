(in-package :cl-user)
(defpackage :butter
  (:use :common-lisp)
  (:export :test-context
           :test-context-name
           :test-context-parent
           :test-condition
           :test-condition-context
	   :test-succeeded
	   :test-failed
	   :test-failed-message
           :test-condition-succeeded-p

           :success-test
           :fail-test

           :in-test
	   :test-form-expand
	   :define-test-type
	   :<-

	   :ok
	   :tests
           :ok-each
           :ok-call

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
  (defmacro with-gensyms ((&rest vars) &rest body)
    `(let ,(loop for var in vars collect `(,var (gensym ,(princ-to-string var)))) ,@body))
  (defclass test-context ()
    ((name :initarg :name :reader test-context-name)
     (parent :initarg :parent :initform nil :reader test-context-parent)))
  (define-condition test-condition (condition)
    ((context :initarg :context :reader test-condition-context)))
  (define-condition test-succeeded (test-condition) ())
  (define-condition test-failed (test-condition)
    ((message :initarg :message :reader test-failed-message))
    (:report (lambda (condition stream)
               (format stream "The test ~A was failed with the message ~S." (test-context-name (test-condition-context condition)) (test-failed-message condition)))))
  (defgeneric test-condition-succeeded-p (condition)
    (:method (condition) nil)
    (:method ((condition test-succeeded)) t))

  (define-condition test-context-required (condition) ())
  (defun call-with-test-handler (test-name function)
    (flet ((make-context ()
             (make-instance 'test-context
                            :name (princ-to-string test-name)
                            :parent (restart-case
                                        (progn (signal 'test-context-required)
                                               nil)
                                      (continue-with-context (context) context)))))
      (restart-case
          (handler-bind ((test-context-required (lambda (condition)
                                                  (declare (ignore condition))
                                                  (invoke-restart 'continue-with-context (make-context)))))
            (funcall function))
        (success-test ()
          (signal 'test-succeeded :context (make-context))
          nil)
        (fail-test (&optional (message ""))
          (signal (make-condition 'test-failed :context (make-context) :message message))
          nil))))
  (defmacro with-test-handler (test-name &body body)
    `(call-with-test-handler ',test-name (lambda () ,@body)))
  (defmacro in-test (test-name &body body)
    (with-gensyms (message%)
      `(with-test-handler ,test-name
         (flet ((success () (invoke-restart 'success-test))
                (fail (,message%) (invoke-restart 'fail-test ,message%)))
           (declare (ignorable (function success) (function fail)))
           ,@body))))
  
  (defgeneric test-form-expand (type arguments))
  (defmacro define-test-type (type (&rest lambda-list) &body body)
    (with-gensyms (type% arguments%)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (defmethod test-form-expand ((,type% (eql ',type)) ,arguments%)
           (destructuring-bind ,lambda-list ,arguments%
             `(in-test (,,type% ,@,arguments%)
                       ,,@body
                       (success)))))))
  (defmacro ok (type &rest arguments)
    (test-form-expand type arguments))
  (defmacro tests (&rest argument-lists)
    `(progn ,@(mapcar (lambda (arguments) `(ok ,@arguments)) argument-lists)))
  
  (define-test-type t (expression)
    `(unless ,expression (fail ,(format nil "~S is nil." expression))))
  (defmethod test-form-expand ((type cons) arguments)
    (funcall #'test-form-expand (car type) (append (cdr type) arguments)))
  (defmethod test-form-expand ((type null) arguments)
    `(ok ,@arguments))
  (defun calling-form (name arguments)
    (if (symbolp name)
        `(,name ,@arguments)
        `(funcall ,name ,@arguments)))
  (defun call-with-override-test-handler (function &key succeeded failed)
    (handler-bind ((test-succeeded (or succeeded #'identity))
                   (test-failed (or failed #'identity)))
      (funcall function)))
  (defun call-with-function-test-handler (function fail-function function-name arguments)
    (call-with-override-test-handler function
                                     :failed (lambda (condition)
                                               (declare (ignore condition))
                                               (funcall fail-function (format nil "(~A~{ ~S~}) is nil." function-name arguments)))))
  (define-test-type :function (function-name &rest arguments)
    (let ((argument-variables (mapcar (lambda (argument) (if (keywordp argument) argument (gensym (princ-to-string argument))))
                                      arguments)))
      `(let ,(mapcan (lambda (variable argument) `((,variable ,argument))) argument-variables arguments)
         (call-with-function-test-handler (lambda () (ok t ,(calling-form function-name argument-variables)))
                                          #'fail ',function-name (list ,@argument-variables)))))
  (defun call-with-resignal-test-handler (function &key success-function fail-function)
    (call-with-override-test-handler function
                                     :succeeded (and success-function
                                                     (lambda (condition)
                                                       (declare (ignore condition))
                                                       (funcall success-function)))
                                     :failed (and fail-function
                                                  (lambda (condition)
                                                    (funcall fail-function (test-failed-message condition))))))
  (defmethod test-form-expand ((type symbol) arguments)
    `(in-test (,type ,@arguments)
              (call-with-resignal-test-handler (lambda () (ok :function ,type ,@arguments))
                                               :success-function #'success
                                               :fail-function #'fail)))
  (defmethod test-form-expand ((type string) arguments)
    `(in-test ,type
              (call-with-resignal-test-handler (lambda () (ok ,@arguments))
                                               :success-function #'success
                                               :fail-function #'fail)))
  (defmacro ok-each (test-type &rest argument-lists)
    `(tests ,@(mapcar (lambda (arguments) (cons test-type arguments)) argument-lists)))
  (defmacro ok-call (function test-type &rest argument-lists)
    (labels ((split-arguments (rest &optional (left ()))
               (if (or (not rest) (eq '<- (car rest)))
                   (values (reverse left) (cdr rest))
                   (split-arguments (cdr rest) (cons (car rest) left)))))
      `(tests ,@(mapcar (lambda (arguments)
                          (multiple-value-bind (test-arguments function-arguments) (split-arguments arguments)
                            `(,test-type ,@test-arguments ,(calling-form function function-arguments))))
                        argument-lists))))

  (define-test-type :type (type value) `(ok typep ,value ',type))
  (define-test-type :condition (condition-type &body body)
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
       (setf (test-function ',name *package*) (lambda () (in-test ,name ,@body)))
       ',name))
  (defun run-test (name package)
    (funcall (test-function name package)))
  (defun test-names (package)
    (loop for name being each hash-key in (test-functions package) collect name)))
