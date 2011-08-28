(butter.util:namespace :butter.extending
  (:use :cl
        :butter.core
        :butter.assertion
        :butter.test-case
        :butter.test-suite)
  (:export :test
           :name
           :test-result
           :target
           :parent-tests
           :subresults
           :do-test
           :run-test
           :start-test
           :with-reporter
           :assertion
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
           :are
           :test-case
           :test-case-result
           :test-case-completed
           :test-case-aborted
           :cause
           :make-test-case
           :test-suite
           :test-suite-result
           :package-test-suite
           :test-cases
           :deftest
           :run-tests))
