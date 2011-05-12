(in-package :butter/cui)
(defmacro do-in-package (package &body body)
  `(let ((*package* (find-package ,package)))
     ,@body))
(defun print-condition-test-names (condition stream)
  (when (inner-condition condition) (print-condition-test-names (inner-condition condition) stream))
  (format stream "  in ~S~%" (test-name condition)))
(defgeneric aborted-condition-message (condition)
  (:method ((condition test-failed)) (message condition))
  (:method ((condition test-error-occurred)) (format nil "Error ~S was occurred.~%~A~%**~%"
						     (occurred-error condition)
						     (with-output-to-string (message)
						       (let ((*print-escape* nil)) (print-object condition message))))))
(defun call-with-test-counter (function)
  (let ((succeeded 0) (failed 0))
    (handler-bind ((test-succeeded (lambda (condition) (declare (ignore condition)) (incf succeeded)))
		   (test-aborted (lambda (condition) (declare (ignore condition)) (incf failed))))
      (values (funcall function) succeeded failed))))
(defun format-test-count (succeeded failed)
  (format nil "~D test~P succeeded, ~D tset~P failed" succeeded succeeded failed failed))
(defun call-with-default-test-printer (function &key (stream *standard-output*) (show-details t))
  (multiple-value-bind (result succeeded failed)
      (call-with-test-counter
       (lambda ()
	 (handler-bind ((test-succeeded (lambda (condition)
					  (when show-details (format stream "ok ~S~%" (test-name condition)))))
			(test-aborted (lambda (condition)
					(format stream "not ok ~S: ~A~%" (test-name condition) (aborted-condition-message condition))
					(print-condition-test-names condition stream))))
	   (funcall function))))
    result))
(defmacro begin (&body form)
  `(call-with-default-test-printer (lambda () ,@form)))
(defun call-with-default-suite-printer (function &optional (stream *standard-output*))
  (multiple-value-bind (result succeeded failed)
      (call-with-test-counter function)
    (format stream "# Failed ~A/~A tests.~%" failed (+ succeeded failed))
    result))
(defun run (package &key (stream *standard-output*) (show-details nil))
  (do-in-package package
    (call-with-default-suite-printer
     (lambda () (reduce (lambda (result test-name)
                          (and (call-with-default-test-printer (lambda ()
                                                                 (when show-details (format stream "# ~A~%" test-name))
                                                                 (run-test test-name package))
                                                               :stream stream
                                                               :show-details show-details)
                               result))
                        (test-names package)
                        :initial-value t)))))
