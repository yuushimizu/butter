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
           :define-macro-test-type
	   :define-test-type
	   :<-

	   :ok
	   :tests

	   :test-not-found-error
           :named-test-not-found-error
           :package-test-not-found-error
	   :test-functions
	   :test-function
	   :deftest
	   :run-test
	   :test-names))
(defpackage :butter.cui
  (:use :common-lisp :butter)
  (:export :begin :run))
