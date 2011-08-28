(butter.util:namespace :butter.assertion
  (:use :cl)
  (:import-from :butter.util
                :eval-always
                :with-gensyms
                :capture-conditions)
  (:import-from :butter.core
                :test
                :name
                :test-result
                :do-test
                :start-test)
  (:export :assertion
           :expected
           :message
           :assertion-result
           :actual
           :signalled-conditions
           :assertion-passed
           :assertion-failed
           :exit-as-passed
           :exit-as-failed
           :pass
           :fail
           :assertion-expand
           :cons-assertion-expand
           :is
           :are))

(defclass assertion (test)
  ((expected :initarg :expected :reader expected)
   (message :initarg :message :reader message)
   (assertion-function :initarg :assertion-function)))
(defmethod name ((assertion assertion))
  (or (message assertion) (expected assertion)))
(defclass assertion-result (test-result)
  ((actual :initarg :actual :reader actual)
   (signalled-conditions :initarg :signalled-conditions :reader signalled-conditions)))
(defclass assertion-passed (assertion-result) ())
(defclass assertion-failed (assertion-result) ())
(defmethod do-test ((assertion assertion))
  (multiple-value-bind (result-class/actual signalled-conditions)
      (capture-conditions
       (restart-case (progn (funcall (slot-value assertion 'assertion-function))
                            (error (format nil "The assertion ~A does not test anything." (name assertion))))
         (exit-as-failed (&optional actual)
           :report (lambda (stream) (format stream "Exit the assertion ~A as failed." (name assertion)))
           (list 'assertion-failed actual))
         (exit-as-passed (&optional actual)
           :report (lambda (stream) (format stream "Exit the assertion ~A as passed." (name assertion)))
           (list 'assertion-passed actual))))
    (destructuring-bind (result-class actual) result-class/actual
      (list result-class
            :signalled-conditions signalled-conditions
            :actual actual))))
(defun pass (actual)
  (invoke-restart 'exit-as-passed actual))
(defun fail (actual)
  (invoke-restart 'exit-as-failed actual))
(eval-always
 (defgeneric assertion-expand (expected environment))
 (defun standard-assertion-expand (expected)
   (with-gensyms (actual%)
     `(let ((,actual% ,expected))
        (funcall (if ,actual% #'pass #'fail) ,actual%))))
 (defmethod assertion-expand (expected environment)
   (declare (ignore environment))
   (standard-assertion-expand expected))
 (defgeneric cons-assertion-expand (car cdr environment)
   (:method (name arguments environment)
     (if (or (special-operator-p name) (macro-function name environment))
         (standard-assertion-expand (cons name arguments))
         (flet ((argument-to-gensym (argument)
                  (if (keywordp argument) argument (gensym (princ-to-string argument)))))
           (let ((argument-variables (mapcar #'argument-to-gensym arguments)))
             `(let ,(mapcan (lambda (variable argument)
                              (if (keywordp argument) () `((,variable ,argument))))
                            argument-variables arguments)
                (if (,name ,@argument-variables)
                    (pass (list ',name ,@argument-variables))
                    (fail `(not ,(list ',name ,@argument-variables))))))))))
 (defmethod assertion-expand ((expected cons) environment)
   (cons-assertion-expand (car expected) (cdr expected) environment))
 (defmethod cons-assertion-expand ((assertion-type (eql :signalled)) condition-type/forms environment)
   (declare (ignore environment))
   (destructuring-bind (condition-type &rest forms) condition-type/forms
     (with-gensyms (condition%)
       `(handler-bind ((,condition-type (lambda (,condition%)
                                          (pass ,condition%))))
          ,@forms
          (fail nil))))))
(defun %is (expected message assertion-function)
  (start-test (make-instance 'assertion
                             :expected expected
                             :message message
                             :assertion-function assertion-function)))
(defmacro is (&environment environment expected &optional message)
  `(%is ',expected ,message (lambda () ,(assertion-expand expected environment))))
(defmacro are (lambda-list assertion-body &rest argument-lists)
  (with-gensyms (expand-assertion%)
    `(macrolet ((,expand-assertion% ,lambda-list `(is ,,assertion-body)))
       ,@(mapcar (lambda (arguments) `(,expand-assertion% ,@arguments)) argument-lists))))