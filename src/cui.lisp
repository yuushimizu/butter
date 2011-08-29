(butter.util:namespace :butter.cui
  (:use :cl)
  (:import-from :butter.util
                :plist-merge)
  (:import-from :butter.extending
                :run-test
                :parent-tests
                :name
                :target
                :signalled-conditions
                :subresults
                :assertion
                :message
                :expected
                :assertion-result
                :assertion-passed
                :assertion-failed
                :actual
                :fail
                :test-case
                :test-case-result
                :test-case-aborted
                :cause
                :abort-test-case
                :test-suite
                :test-suite-result)
  (:export :cui-reporter))

(defclass cui-reporter ()
  ((stream :initarg :stream :initform *standard-output* :reader cui-reporter-stream)
   (verbose :initarg :verbose :initform nil :reader cui-reporter-verbose)
   (invoke-debugger :initarg :invoke-debugger :initform nil :reader cui-reporter-invoke-debugger)))
(defgeneric print-result (reporter result)
  (:method (reporter result)
    (declare (ignore reporter result))
    nil))
(defmethod run-test (test (reporter cui-reporter) &rest options)
  (declare (ignore test options))
  (let ((result (call-next-method)))
    (print-result reporter result)
    result))
(defgeneric default-debugger-hook (test)
  (:method (test)
    (declare (ignore test))
    nil)
  (:method ((test assertion))
    (declare (ignore test))
    #'fail)
  (:method ((test test-case))
    (declare (ignore test))
    (lambda (condition)
      (invoke-restart 'abort-test-case condition))))
(defmethod run-test :around (test (reporter cui-reporter) &rest options)
  (declare (ignore options))
  (let ((*debugger-hook* (if (cui-reporter-invoke-debugger reporter)
                             *debugger-hook*
                             (lambda (condition hook)
                               (declare (ignore hook))
                               (let ((default-hook (default-debugger-hook test)))
                                 (when default-hook (funcall default-hook condition)))))))
    (call-next-method)))
(defun result-context-string (result)
  (let ((parent-tests (parent-tests result)))
    (if (null parent-tests)
        "TOP LEVEL"
        (labels ((parents-to-string (&optional (rest parent-tests))
                   (if (cdr rest)
                       (format nil "~A~%  in ~A" (name (car rest)) (parents-to-string (cdr rest)))
                       (princ-to-string (name (car rest))))))
          (parents-to-string)))))
(defgeneric expression-string (value)
  (:method (value)
    (prin1-to-string value))
  (:method ((value condition))
    (format nil "~S: ~:*~A" value)))
(defun print-signalled-conditions (stream result)
  (when (signalled-conditions result)
    (format stream "~&signalled conditions:~%~{  ~S: ~:*~A~%~}" (signalled-conditions result))))
(defun print-assertion-result (stream type result with-actual-p)
  (format stream "~%~&~A in ~A~%~:[~; message: ~:*~A~%~]expected: ~S~%"
          type
          (result-context-string result)
          (message (target result))
          (expected (target result)))
  (when with-actual-p (format stream "  actual: ~A~%" (expression-string (actual result))))
  (print-signalled-conditions stream result))
(defmethod print-result (reporter (result assertion-passed))
  (when (cui-reporter-verbose reporter)
    (print-assertion-result (cui-reporter-stream reporter) "PASS" result nil)))
(defmethod print-result (reporter (result assertion-failed))
  (print-assertion-result (cui-reporter-stream reporter) "FAIL" result t))
(defmethod run-test :before ((test test-case) (reporter cui-reporter) &rest options)
  (declare (ignore options))
  (when (cui-reporter-verbose reporter)
    (format (cui-reporter-stream reporter) "~%~&Testing ~A.~%" (name test))))
(defmethod print-result (reporter (result test-case-aborted))
  (format (cui-reporter-stream reporter) "~%~&ABORT ~A~%cause: ~A~%"
          (name (target result))
          (expression-string (cause result))))
(defun empty-result-count ()
  `(:assertions 0 :failed-assertions 0 :test-cases 0 :aborted-test-cases 0))
(defun merge-result-counts (&rest result-counts)
  (reduce (lambda (result count)
            (plist-merge #'+ result count))
          result-counts
          :initial-value (empty-result-count)))
(define-method-combination merge-result-counts :operator merge-result-counts)
(defgeneric result-count (result)
  (:method-combination merge-result-counts)
  (:method merge-result-counts (result)
    (apply #'merge-result-counts
           (mapcar #'result-count (subresults result))))
  (:method merge-result-counts ((result assertion-result))
    (declare (ignore result))
    '(:assertions 1))
  (:method merge-result-counts ((result assertion-failed))
    (declare (ignore result))
    '(:failed-assertions 1))
  (:method merge-result-counts ((result test-case-result))
    (declare (ignore result))
    '(:test-cases 1))
  (:method merge-result-counts ((result test-case-aborted))
    (declare (ignore result))
    '(:aborted-test-cases 1)))
(defmethod print-result (reporter (result test-suite-result))
  (let ((count (result-count result)))
    (format (cui-reporter-stream reporter) "~%~&Ran ~D tests containing ~D assertions, failed ~D assertions, and aborted ~D tests.~%"
            (getf count :test-cases)
            (getf count :assertions)
            (getf count :failed-assertions)
            (getf count :aborted-test-cases))
    result))
