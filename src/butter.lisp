(in-package :butter)

(defmacro with-gensyms ((&rest vars) &rest body)
  `(let ,(loop for var in vars collect `(,var (gensym ,(princ-to-string var)))) ,@body))
(defmacro do-in-package (package &body body)
  `(let ((*package* (find-package ,package)))
     ,@body))

(define-condition test-condition (condition)
  ((test-name :initarg :test-name :reader test-name)))
(define-condition test-succeeded (test-condition) ())
(define-condition test-aborted (test-condition)
  ((inner-condition :initarg :inner-condition :initform nil :reader inner-condition)))
(defun recursive-reader (test-condition slot)
  (or (and (slot-boundp test-condition slot) (slot-value test-condition slot))
      (and (inner-condition test-condition) (recursive-reader (inner-condition test-condition) slot))))
(define-condition test-failed (test-aborted)
  ((message :initarg :message :reader message)))
(defmethod message ((condition test-failed))
  (recursive-reader condition 'message))
(define-condition test-error-occurred (test-aborted)
  ((occurred-error :initarg :occurred-error :reader occurred-error))
  (:report (lambda (condition stream)
	     (format stream "Error ~A was occurred in test ~A." (occurred-error condition) (test-name condition)))))
(defmethod occurred-error ((condition test-error-occurred))
  (recursive-reader condition 'occurred-error))
(defgeneric test-condition-result (condition)
  (:method (condition) (declare (ignore condition)) t)
  (:method ((condition test-aborted)) (declare (ignore condition)) nil))

(defun call-as-test (name function)
  (handler-case
      (funcall function)
    (test-error-occurred (condition) (signal 'test-error-occurred :test-name name :inner-condition condition) nil)
    (error (condition) (signal 'test-error-occurred :test-name name :occurred-error condition) nil)))
(defun call-with-default-fail-handler (function fail-function)
  (handler-case (funcall function)
    (test-failed (condition) (funcall fail-function nil condition))))
(defmacro with-default-fail-handler (&body body)
  `(call-with-default-fail-handler (lambda () ,@body) #'fail))
(defmacro in-test (name &body body)
  (with-gensyms (name% result% fail-message% inner-condition% condition%)
    `(let ((,name% ,name))
       (call-as-test ,name%
	   (lambda ()
	     (let ((,result% (catch ',result%
			       (flet ((success () (throw ',result% (make-condition 'test-succeeded :test-name ,name%)))
				      (fail (,fail-message% &optional ,inner-condition%)
					(throw ',result% (make-condition 'test-failed :test-name ,name% :message ,fail-message% :inner-condition ,inner-condition%))))
				 (declare (ignorable (function success) (function fail)))
				 (with-default-fail-handler ,@body)
				 nil))))
	       (when ,result% (signal ,result%))
	       (test-condition-result ,result%)))))))
  
(defgeneric test-form-expand (type arguments))
(defmacro define-test-type-without-success (name (&rest lambda-list) &body body)
  (with-gensyms (type% arguments%)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (progn
	 (defmethod test-form-expand ((,type% (eql ',name)) ,arguments%)
	   (declare (ignore ,type%))
	   (destructuring-bind ,lambda-list ,arguments%
	     ,@body))
	 ',name))))
(defun call-with-default-success-handler (function success-function)
  (handler-case (funcall function)
    (test-succeeded ()))
  (funcall success-function))
(defmacro with-default-success-handler (&body body)
  `(call-with-default-success-handler (lambda () ,@body) #'success))
(defmacro define-test-type (name (&rest lambda-list) &body body)
  `(define-test-type-without-success ,name ,lambda-list
     `(with-default-success-handler ,(progn ,@body))))

(defmacro test (type &rest arguments)
  `(in-test '(,type ,@arguments) ,(test-form-expand type arguments)))
(defmacro tests (&rest argument-lists)
  `(progn ,@(mapcar (lambda (arguments) `(test ,@arguments)) argument-lists)))
  
(define-test-type t (expression)
  `(unless ,expression (fail ,(format nil "~S is nil." expression))))
(defmethod test-form-expand ((type cons) arguments)
  (funcall #'test-form-expand (car type) (append (cdr type) arguments)))
(define-test-type-without-success nil (&rest arguments)
  `(test ,@arguments))
(defun call-form (name arguments)
  (if (symbolp name)
      `(,name ,@arguments)
      `(funcall ,name ,@arguments)))
(defun call-with-function-fail-handler (function fail-function function-name arguments)
  (handler-case (funcall function)
    (test-failed () (funcall fail-function (format nil "(~A~{ ~S~}) is nil." function-name arguments)))))
(define-test-type :function (function-name &rest arguments)
  (let ((argument-vars (mapcar (lambda (argument)
				 (declare (ignore argument))
				 (if (keywordp argument) argument (gensym (princ-to-string argument))))
			       arguments)))
    `(let ,(remove nil (mapcar (lambda (var argument) (unless (keywordp var) (list var argument)))
			       argument-vars arguments))
       (call-with-function-fail-handler (lambda () (test t ,(call-form function-name argument-vars)))
					#'fail ',function-name (list ,@argument-vars)))))
(defun call-with-no-inner-condition-fail-handler (function fail-function)
  (handler-case (funcall function)
    (test-failed (condition) (funcall fail-function (message condition)))))
(defmacro with-no-inner-condition-fail-handler (&body body)
  `(call-with-no-inner-condition-fail-handler (lambda () ,@body) #'fail))
(defmethod test-form-expand ((type symbol) arguments)
  (with-gensyms (condition%)
    `(with-default-success-handler
	 (with-no-inner-condition-fail-handler (test :function ,type ,@arguments)))))

(define-test-type-without-success :each (test-type &rest argument-lists)
  `(tests ,@(mapcar (lambda (arguments) (cons test-type arguments)) argument-lists)))
(define-test-type-without-success call-single (function &rest arguments)
  (labels ((split-arguments (&optional (rest arguments) (left ()))
	     (if (or (not rest) (eq '<- (car rest)))
		 (values (reverse left) (cdr rest))
		 (split-arguments (cdr rest) (cons (car rest) left)))))
    (multiple-value-bind (test-arguments function-arguments) (split-arguments arguments)
      `(test ,@test-arguments ,(call-form function function-arguments)))))
(define-test-type-without-success :call (function test-type &rest argument-lists)
  `(test :each (call-single ,function ,test-type) ,@argument-lists))
(define-test-type-without-success macro-single ((&rest lambda-list) expansion &rest arguments)
  (with-gensyms (macro%)
    `(macrolet ((,macro% ,lambda-list (cons 'test ,expansion))) (,macro% ,@arguments))))
(define-test-type-without-success :macro ((&rest lambda-list) expansion &rest argument-lists)
  `(test :each (macro-single ,lambda-list ,expansion) ,@argument-lists))
(define-test-type :type (type value) `(test typep ,value ',type))
(define-test-type :condition (condition-type &body body)
  `(handler-case (progn ,@body (fail ,(format nil "An expected condition ~A was not signalled." condition-type)))
     (,condition-type ())))


(define-condition no-test-found (error)
  ((test-name :initarg :test-name :reader test-name)))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((test-functions-in-packages (make-hash-table :test 'equal)))
    (defun test-functions (package)
      (or (gethash (package-name package) test-functions-in-packages)
	  (setf (gethash (package-name package) test-functions-in-packages) (make-hash-table))))))
(defun test-function (name &optional (package *package*))
  (or (gethash name (test-functions package))
      (error 'no-test-found :test-name name)))
(defun (setf test-function) (function name &optional (package *package*))
  (let ((test-functions (test-functions package)))
    (setf (gethash name test-functions) function)))
(defmacro deftest (name &body body)
  `(progn
     (setf (test-function ',name) (lambda () (in-test ',name ,@body)))
     ',name))
(defun run-test (name package &key (show-details t))
  (funcall (test-function name package)))
(defun test-names (&optional (package *package*))
  (loop for name being each hash-key in (test-functions package) collect name))
