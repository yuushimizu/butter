(in-package :cl-user)
(defpackage :butter.sample.extending
  (:use :cl :butter.extending :butter.sample.stub))
(in-package :butter.sample.extending)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod cons-assertion-expand ((assertion-type (eql 'type?)) type/forms environment)
    (declare (ignore environment))
    (destructuring-bind (type &body forms) type/forms
      (let ((value% (gensym "VALUE")))
        `(let ((,value% (progn ,@forms)))
           (funcall (if (typep ,value% ',type) #'pass #'fail)
                    (list (type-of ,value%) ,value%)))))))

(deftest type?
  (is (type? integer (generic-plus 10 20)))
  (is (type? string (generic-plus "FOO" "BAR"))))
