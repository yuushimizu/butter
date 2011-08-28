(butter.util:namespace :butter
  (:use :cl)
  (:import-from :butter.util
                :eval-always)
  (:import-from :butter.core
                :*reporter*
                :with-reporter)
  (:import-from :butter.cui
                :cui-reporter)
  (:import-from :butter.assertion
                :is
                :are)
  (:import-from :butter.test-suite
                :deftest
                :run-tests)
  (:export :with-reporter
           :is
           :are
           :deftest
           :run-tests))

(eval-always
 (defparameter *reporter* (make-instance 'cui-reporter)))
