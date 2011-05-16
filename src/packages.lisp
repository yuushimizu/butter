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

	   :test-form-expand
	   :define-test-type-without-success
	   :with-default-success-handler
	   :define-test-type
	   :<-

	   :test
	   :tests

	   :make-test-condition-message
	   :print-test-condition
	   :call-with-default-test-handler
	   
	   :no-test-found
	   :test-functions
	   :test-function
	   :deftest
	   :run-test
	   :test-names))
(defpackage :butter/cui
  (:use :common-lisp :butter)
  (:export :begin :run))
