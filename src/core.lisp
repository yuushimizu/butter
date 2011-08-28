(butter.util:namespace :butter.core
  (:use :cl)
  (:export :test
           :name
           :test-result
           :target
           :parent-tests
           :subresults
           :do-test
           :run-test
           :*reporter*
           :start-test
           :with-reporter))

(defclass test () ())
(defgeneric name (test))
(defclass test-result ()
  ((target :type test :initarg :target :reader target)
   (parent-tests :initarg :parent-tests :reader parent-tests)
   (subresults :initarg :subresults :reader subresults)))

(defvar *subresult-hook* #'identity)
(defgeneric do-test (test))
(defgeneric run-test (test reporter &rest options &key parent-tests &allow-other-keys)
  (:method (test reporter &key parent-tests &allow-other-keys)
    (declare (ignore reporter))
    (let ((subresults ()))
      (destructuring-bind (result-class &rest additional-arguments)
          (let ((*subresult-hook* (lambda (subresult) (push subresult subresults)))) (do-test test))
        (let ((result (apply #'make-instance
                             result-class
                             :target test
                             :parent-tests parent-tests
                             :subresults subresults
                             additional-arguments)))
          (funcall *subresult-hook* result)
          result)))))

(defvar *running-tests* ())
(defparameter *reporter* nil)
(defun start-test (test &optional (reporter *reporter*))
  (let* ((parent-tests (copy-list *running-tests*))
         (*running-tests* (cons test *running-tests*)))
    (run-test test reporter :parent-tests parent-tests)))
(defun %with-reporter (reporter function)
  (let ((*reporter* reporter))
    (funcall function)))
(defmacro with-reporter (reporter &body forms)
  `(%with-reporter ,reporter (lambda () ,@forms)))
