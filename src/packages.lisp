(in-package :cl-user)
(defpackage :butter
  (:use :common-lisp)
  (:export :test-condition
	   :test-name
	   :test-succeeded
	   :test-aborted
	   :inner-condition
	   :test-failed
	   :message
	   :test-error-occurred
	   :occurred-error
	   :test-condition-result

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
  (:export :run))
