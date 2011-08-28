(butter.util:namespace :butter.test-case
  (:use :cl)
  (:import-from :butter.core
                :test
                :name
                :test-result
                :do-test)
  (:export :test-case
           :test-case-result
           :test-case-completed
           :test-case-aborted
           :abort-test-case
           :cause
           :make-test-case))

(defclass test-case (test)
  ((name :type symbol :initarg :name :reader name)
   (function :initarg :function)))
(defclass test-case-result (test-result) ())
(defclass test-case-completed (test-case-result) ())
(defclass test-case-aborted (test-case-result)
  ((cause :initarg :cause :reader cause)))
(defmethod do-test ((test-case test-case))
  (restart-case
      (progn (funcall (slot-value test-case 'function))
             '(test-case-completed))
    (abort-test-case (&optional cause)
      :report (lambda (stream) (format stream "Exit the test case ~A." (name test-case)))
      (list 'test-case-aborted :cause cause))))
(defmacro make-test-case (name &body forms)
  `(make-instance 'test-case :name ',name :function (lambda () ,@forms)))
