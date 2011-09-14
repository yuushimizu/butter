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
           :pass
           :fail
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
  (multiple-value-bind (result-class/actual signalled-conditions)
      (capture-conditions
       (let ((exit-tag (gensym "EXIT-TAG")))
         (catch exit-tag
           (flet ((pass (actual) (throw exit-tag (list 'assertion-passed actual)))
                  (fail (actual) (throw exit-tag (list 'assertion-failed actual))))
             (restart-case (progn (funcall (slot-value assertion 'assertion-function)
                                           :pass #'pass :fail #'fail)
                                  (error (format nil "The assertion ~A does not test anything." (name assertion))))
               (exit-as-failed (&optional actual)
                 :report (lambda (stream) (format stream "Exit the assertion ~A as failed." (name assertion)))
                 (fail actual)))))))
    (destructuring-bind (result-class actual) result-class/actual
      (list result-class
            :signalled-conditions signalled-conditions
            :actual actual))))
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
  `(handler-bind ((,condition-type #'pass))
     ,@forms
     (fail nil)))
(define-special-assertion :print (stream-variable expected &rest forms)
  (with-gensyms (stream% output%)
    `(let ((,output% (let* ((,stream% (make-string-output-stream))
                            (,stream-variable ,stream%))
                       ,@forms
                       (get-output-stream-string ,stream%))))
       (funcall (if (string= ,expected ,output%) #'pass #'fail)
                ,output%))))
(define-special-assertion typep (form type)
  (with-gensyms (value%)
    `(let ((,value% ,form))
       (funcall (if (typep ,value% ,type) #'pass #'fail)
                (list ,value% (type-of ,value%))))))
(defun %is (expected message assertion-function)
  (start-test (make-instance 'assertion
                             :expected expected
                             :message message
                             :assertion-function assertion-function)))
(defmacro is (&environment environment expected &optional message)
  (with-gensyms (pass% fail% actual%)
    `(%is ',expected
          ,message
          (lambda (&key ((:pass ,pass%)) ((:fail ,fail%)))
            (flet ((pass (,actual%) (funcall ,pass% ,actual%))
                   (fail (,actual%) (funcall ,fail% ,actual%)))
              ,(assertion-expand expected environment))))))
(defmacro are (lambda-list assertion-body &rest argument-lists)
  (with-gensyms (expand-assertion%)
    `(macrolet ((,expand-assertion% ,lambda-list `(is ,,assertion-body)))
       ,@(mapcar (lambda (arguments) `(,expand-assertion% ,@arguments)) argument-lists))))
