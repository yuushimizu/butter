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

(deftest sample-signal
  (is (:signal error (error "ERROR")))
  (is (:signal error (+ 10 20)))
  (is (:signal sample-warning (warn 'sample-warning)))
  (is (:signal sample-warning (+ 1 2)))
  (is (:signal sample-error (error 'sample-error)))
  (is (:signal sample-error (error "ERROR"))))

(deftest sample-print
  (is (:print *standard-output* "helloworld" (princ "hello") (princ "world")))
  (is (:print *standard-output* "hello world" (princ "hello") (princ "world")))
  (is (:print *debug-io* "foobar" (princ "foobar" *debug-io*)))
  (is (:print *debug-io* "foobar" (princ "foo bar" *debug-io*))))

(deftest sample-typep
  (is (typep (generic-plus 10 20) 'integer))
  (is (typep (generic-plus "A" "B") 'integer))
  (is (typep (generic-plus "A" "B") 'string))
  (is (typep (generic-plus 10 20) 'string)))

(deftest sample-with-message
  (is (= 10 (+ 1 2 3 4)) "1 + 2 + 3 + 4 = 10")
  (is (= 10 (+ 1 2 3)) "1 + 2 + 3 = 10")
  (is (equal '(1 2 3 4) (append '(1 2) '(3 4))) "(1 2) + (3 4) = (1 2 3 4)")
  (is (equal '(1 2 3 4 5) (append '(1 2) '(3 4))) "(1 2) + (3 4) = (1 2 3 4 5)"))
