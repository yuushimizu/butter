(in-package :cl-user)
(defpackage :butter.failed-test
  (:use :common-lisp :butter))
(in-package :butter.failed-test)

(define-assertion-type between (min max value)
  (let ((value% (gensym)))
    `(let ((,value% ,value))
       (ok and (<= ,min ,value%) (<= ,value% ,max)))))

(deftest failed-1
  (ok t (string= "foo" "bar"))
  (ok between 10 20 30))

(deftest failed-2
  (ok :condition 'error
      (+ 10 20)))

(deftest failed-3
  (ok () eq 'foo 'bar)
  (ok () eq 'baz 'baz)
  (ok () eql 'foo 'bar))

(deftest with-message
  (ok "add two numbers" = 29 (+ 20 10))
  (ok "append two lists" equal '(1 2 3 4 5) (append '(1 2) '(3 4))))

(deftest ok-each
  (ok-each ()
           (eql 15 (+ 4 5 6 7))
           (string= "foobar" (concatenate 'string "foo" "bar" "baz"))))

(deftest ok-call
  (ok-call 1+ ()
           (eql 3 <- 10)
           (< 4 <- 8))
  (ok-call 1- eql
           (10 <- 11)
           (20 <- 20)))
