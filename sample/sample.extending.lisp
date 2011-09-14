(in-package :cl-user)
(defpackage :butter.sample.extending
  (:use :cl :butter.extending :butter.sample.stub))
(in-package :butter.sample.extending)

(define-special-assertion type? (type &rest forms)
  (let ((value% (gensym "VALUE")))
    `(let ((,value% (progn ,@forms)))
       (values (typep ,value% ',type) (list ,value% (type-of ,value%))))))

(deftest type?
  (is (type? integer (generic-plus 10 20)))
  (is (type? string (generic-plus "FOO" "BAR"))))
