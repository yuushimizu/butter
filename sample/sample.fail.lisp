(in-package :cl-user)
(defpackage :butter.sample.fail
  (:use :cl :butter :butter.sample.stub))
(in-package :butter.sample.fail)

(deftest sample-equivalence
  (is (eq 'foo 'foo))
  (is (eq 'foo 'bar))
  (is (= 10 (+ 1 2 3 4)))
  (is (= 10 (+ 1 2 3)))
  (is (string= "foobar" (concatenate 'string "foo" "bar")))
  (is (string= "foobar" (concatenate 'string "a" "b"))))

(deftest sample-function
  (is (< 0 20))
  (is (< 20 0))
  (is (typep 10 'integer))
  (is (typep 10 'string))
  (is (positive-even-p 10))
  (is (positive-even-p 13)))

(deftest sample-signalled
  (is (:signalled error (error "ERROR")))
  (is (:signalled error (+ 10 20)))
  (is (:signalled sample-warning (warn 'sample-warning)))
  (is (:signalled sample-warning (+ 1 2)))
  (is (:signalled sample-error (error 'sample-error)))
  (is (:signalled sample-error (error "ERROR"))))

(deftest sample-with-message
  (is (= 10 (+ 1 2 3 4)) "1 + 2 + 3 + 4 = 10")
  (is (= 10 (+ 1 2 3)) "1 + 2 + 3 = 10")
  (is (equal '(1 2 3 4) (append '(1 2) '(3 4))) "(1 2) + (3 4) = (1 2 3 4)")
  (is (equal '(1 2 3 4 5) (append '(1 2) '(3 4))) "(1 2) + (3 4) = (1 2 3 4 5)"))
