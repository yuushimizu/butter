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
           :exit-as-failed
           :assertion-expand
           :cons-assertion-expand
           :define-special-assertion
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
  (multiple-value-bind (passed-p/actual signalled-conditions)
      (capture-conditions
       (multiple-value-bind (passed-p actual)
           (restart-case (funcall (slot-value assertion 'assertion-function))
             (exit-as-failed (&optional actual)
               :report (lambda (stream) (format stream "Exit the assertion ~A as failed." (name assertion)))
               (values nil actual)))
         (list passed-p actual)))
    (destructuring-bind (passed-p actual) passed-p/actual
      (list (if passed-p 'assertion-passed 'assertion-failed)
            :signalled-conditions signalled-conditions
            :actual actual))))
(eval-always
 (defgeneric assertion-expand (expected environment))
 (defun standard-assertion-expand (expected)
   (with-gensyms (actual%)
     `(let ((,actual% ,expected))
        (values ,actual% ,actual%))))
 (defmethod assertion-expand (expected environment)
   (declare (ignore environment))
   (standard-assertion-expand expected))
 (defgeneric cons-assertion-expand (car cdr environment)
   (:method (name arguments environment)
     (if (or (special-operator-p name) (macro-function name environment))
         (standard-assertion-expand (cons name arguments))
         (flet ((argument-to-gensym (argument)
                  (if (keywordp argument) argument (gensym (princ-to-string argument)))))
           (let ((argument-variables (mapcar #'argument-to-gensym arguments))
                 (passed-p% (gensym "PASSED-P")))
             `(let ,(mapcan (lambda (variable argument)
                              (if (keywordp argument) () `((,variable ,argument))))
                            argument-variables arguments)
                (let ((,passed-p% (,name ,@argument-variables)))
                  (values ,passed-p%
                          (if ,passed-p%
                              (list ',name ,@argument-variables)
                              `(not ,(list ',name ,@argument-variables)))))))))))
 (defmethod assertion-expand ((expected cons) environment)
   (cons-assertion-expand (car expected) (cdr expected) environment))
 (defmacro define-special-assertion (name lambda-list &rest forms)
   (with-gensyms (assertion-type% arguments% environment%)
     (let ((environment-variable (second (member '&environment lambda-list))))
       `(eval-always
         (defmethod cons-assertion-expand ((,assertion-type% (eql ',name)) ,arguments% ,environment%)
           (declare (ignore ,assertion-type%)
                    (ignorable ,environment%))
           (destructuring-bind ,lambda-list ,arguments%
             ,@(if environment-variable
                   `((declare (ignore ,environment-variable))
                     (let ((,environment-variable ,environment%)) ,@forms))
                   forms))))))))
(define-special-assertion :signalled (condition-type &rest forms)
  (with-gensyms (condition%)
    `(handler-case (progn
                     ,@forms
                     (values nil nil))
       (,condition-type (,condition%) (values t ,condition%)))))
(define-special-assertion :print (stream-variable expected &rest forms)
  (with-gensyms (stream% output%)
    `(let ((,output% (let* ((,stream% (make-string-output-stream))
                            (,stream-variable ,stream%))
                       ,@forms
                       (get-output-stream-string ,stream%))))
       (values (string= ,expected ,output%) ,output%))))
(define-special-assertion typep (form type)
  (with-gensyms (value%)
    `(let ((,value% ,form))
       (values (typep ,value% ,type) (list ,value% (type-of ,value%))))))
(defun %is (expected message assertion-function)
  (typep (start-test (make-instance 'assertion
                                    :expected expected
                                    :message message
                                    :assertion-function assertion-function))
         'assertion-passed))
(defmacro is (&environment environment expected &optional message)
  `(%is ',expected
        ,message
        (lambda () ,(assertion-expand expected environment))))
(defmacro are (lambda-list assertion-body &rest argument-lists)
  (with-gensyms (expand-assertion%)
    `(macrolet ((,expand-assertion% ,lambda-list `(is ,,assertion-body)))
       ,@(mapcar (lambda (arguments) `(,expand-assertion% ,@arguments)) argument-lists))))
