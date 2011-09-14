(butter.util:namespace :butter.test-suite
  (:use :cl)
  (:import-from :butter.util
                :with-gensyms
                :eval-always
                :add-or-replace)
  (:import-from :butter.core
                :test
                :name
                :test-result
                :do-test
                :start-test)
  (:import-from :butter.test-case
                :make-test-case)
  (:export :test-suite
           :test-suite-result
           :package-test-suite
           :test-cases
           :deftest
           :run-tests))

(defclass test-suite (test)
  ((package :initarg :package)))
(defmethod name ((test-suite test-suite))
  (package-name (slot-value test-suite 'package)))
(defclass test-suite-result (test-result) ())
(defun package-test-suite (package)
  (make-instance 'test-suite :package package))
(eval-always
 (let ((package-test-cases (make-hash-table :test 'equalp)))
   (defun add-test-case (test-case)
     (let ((package-name (package-name (symbol-package (name test-case)))))
       (setf (gethash package-name package-test-cases)
             (add-or-replace test-case (gethash package-name package-test-cases)
                             :test (lambda (case1 case2) (eq (name case1) (name case2))))))
     test-case)
   (defun test-cases (test-suite)
     (copy-list (gethash (package-name (slot-value test-suite 'package)) package-test-cases)))))
(defmethod do-test ((test-suite test-suite))
  (mapc #'start-test (test-cases test-suite))
  '(test-suite-result))
(defmacro deftest (name &body forms)
  (with-gensyms (test-case%)
    `(eval-always
      (let ((,test-case% (make-test-case ,name ,@forms)))
        (add-test-case ,test-case%)
        ,test-case%))))
(defun run-tests (&optional (package *package*))
  (start-test (package-test-suite package)))
