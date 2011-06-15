(in-package :cl-user)
(defpackage :butter.test
  (:use :common-lisp :butter))
(in-package :butter.test)

(define-test-type between (min max value)
  (let ((value% (gensym)))
    `(let ((,value% ,value))
       (ok and (<= ,min ,value%) (<= ,value% ,max)))))

(deftest t
  (ok t 10)
  (ok t (> 20 10))
  (ok t (string= "foo" (format nil "~A~A~A" #\f #\o #\o)))
  (ok :condition test-failed
      (ok t (> 10 20))))

(deftest nil
  (ok () t  10)
  (ok () eq 'foo 'foo)
  (ok () = 3 (+ 1 2)))

(deftest not
  (ok not nil)
  (ok not (< 20 10))
  (ok not (string= "bar" (format nil "~A~A~A" #\f #\o #\o)))
  (ok :condition test-failed
      (ok not (< 10 20))))

(deftest eq
  (ok eq 'foo 'foo)
  (let* ((object1 (make-hash-table))
	 (object2 object1))
    (ok eq object1 object2))
  (ok eq nil nil)
  (ok eq t t))

(deftest eql
  (ok eql 10 10)
  (ok eql 10 (+ 4 6)))

(deftest equal
  (ok equal "foo" "foo")
  (ok equal "foobar" (format nil "~A~A" "foo" "bar"))
  (ok equal '(1 2 3) (list 1 2 (+ 1 2))))

(deftest equalp
  (ok equalp "foo" "FoO")
  (ok equalp "BAR" (format nil "~A~A" "b" "AR"))
  (ok equalp "foo" "foo"))

(defun number-in (min max value)
  (and (<= min value) (<= value max)))

(deftest functions
  (ok = 9 (* 3 3))
  (ok string= "foobar" (concatenate 'string "foo" "bar"))
  (ok < 10 20)
  (ok typep 10 'integer)
  (ok evenp 20)
  (ok number-in 10 30 20))

(deftest function
  (ok :function = 9 (* 3 3))
  (ok :function string= "foobar" (concatenate 'string "foo" "bar"))
  (ok :function < 10 20)
  (ok :function typep 10 'integer)
  (ok :function evenp 20)
  (ok :function number-in 10 30 20))

(deftest with-message
  (ok "add two numbers" = 30 (+ 20 10))
  (ok "append two lists" equal '(1 2 3 4) (append '(1 2) '(3 4)))
  (ok "10 < 20" < 10 20))

(deftest type
  (ok :type integer 12)
  (ok :type string "foo")
  (ok :type (or integer list) '(a b c)))

(deftest each
  (ok :each ()
      (eql 15 (+ 4 5 6))
      (string= "foobar" (concatenate 'string "foo" "bar")))
  (ok :each eql
      (10 (+ 4 6))
      ('x 'x))
  (ok :each (eql 20)
      ((+ 14 6))
      ((* 2 10)))
  (ok :each :type
      (integer (+ 12 6))
      (string "foo"))
  (ok :each (:type integer)
      (30)
      (42))
  (ok :each eql))

(deftest tests
  (tests (eql 15 (+ 4 5 6))
	 (:type integer 13)
	 (:each eql
		(10 (* 2 5))
		(5 (- 6 1)))))

(defun 2* (n) (* n 2))

(deftest call
  (ok :call 1+ ()
      (eql 3 <- 2)
      (< 4 <- 4))
  (ok :call 2* eql
      (10 <- 5)
      (20 <- 10)
      (30 <- 15))
  (ok :call - (> 10)
      (<- 12 8)
      (<- 16 10))
  (ok :call (lambda (n) (* n 10)) eql
      (100 <- 10))
  (ok :call (lambda (x y) (* x y)) =
      (27 <- 3 9)
      (56 <- 7 8))
  (ok :call (lambda () 30) (eql 30)
      ())
  (ok :call (constantly 40) eql
      (40))
  (ok :call (constantly 40) eql
      (40 <-))
  (ok :call (constantly 40) (eql 40)
      ())
  (ok :call (constantly 40) (eql 40)
      (<-))
  (ok :call 2* eql))

(deftest define-test-type
  (ok between 10 20 13)
  (ok (between -30) 0 -24)
  (ok (:each (between 5 9)
             (7)
             (6)
             (9))))
