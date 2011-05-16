(in-package :butter-test)

(define-test-type between (min max value)
  (let ((value% (gensym)))
    `(let ((,value% ,value))
       (test and (<= ,min ,value%) (<= ,value% ,max)))))

(deftest t
  (test t 10)
  (test t (> 20 10))
  (test t (string= "foo" (format nil "~A~A~A" #\f #\o #\o)))
  (test :condition test-failed
        (test t (> 10 20))))

(deftest nil
  (test () t  10)
  (test () eq 'foo 'foo)
  (test () = 3 (+ 1 2)))

(deftest not
  (test not nil)
  (test not (< 20 10))
  (test not (string= "bar" (format nil "~A~A~A" #\f #\o #\o)))
  (test :condition test-failed
        (test not (< 10 20))))

(deftest eq
  (test eq 'foo 'foo)
  (let* ((object1 (make-hash-table))
	 (object2 object1))
    (test eq object1 object2))
  (test eq nil nil)
  (test eq t t))

(deftest eql
  (test eql 10 10)
  (test eql 10 (+ 4 6)))

(deftest equal
  (test equal "foo" "foo")
  (test equal "foobar" (format nil "~A~A" "foo" "bar"))
  (test equal '(1 2 3) (list 1 2 (+ 1 2))))

(deftest equalp
  (test equalp "foo" "FoO")
  (test equalp "BAR" (format nil "~A~A" "b" "AR"))
  (test equalp "foo" "foo"))

(defun number-in (min max value)
  (and (<= min value) (<= value max)))

(deftest functions
  (test = 9 (* 3 3))
  (test string= "foobar" (concatenate 'string "foo" "bar"))
  (test < 10 20)
  (test typep 10 'integer)
  (test evenp 20)
  (test number-in 10 30 20))

(deftest function
  (test :function = 9 (* 3 3))
  (test :function string= "foobar" (concatenate 'string "foo" "bar"))
  (test :function < 10 20)
  (test :function typep 10 'integer)
  (test :function evenp 20)
  (test :function number-in 10 30 20))

(deftest with-message
  (test "add two numbers" = 30 (+ 20 10))
  (test "append two lists" equal '(1 2 3 4) (append '(1 2) '(3 4)))
  (test "10 < 20" < 10 20))

(deftest type
  (test :type integer 12)
  (test :type string "foo")
  (test :type (or integer list) '(a b c)))

(deftest each
  (test :each ()
	(eql 15 (+ 4 5 6))
	(string= "foobar" (concatenate 'string "foo" "bar")))
  (test :each eql
	(10 (+ 4 6))
	('x 'x))
  (test :each (eql 20)
	((+ 14 6))
	((* 2 10)))
  (test :each :type
	(integer (+ 12 6))
	(string "foo"))
  (test :each (:type integer)
	(30)
	(42))
  (test :each eql))

(deftest tests
  (tests (eql 15 (+ 4 5 6))
	 (:type integer 13)
	 (:each eql
		(10 (* 2 5))
		(5 (- 6 1)))))

(defun 2* (n) (* n 2))

(deftest call
  (test :call 1+ ()
	(eql 3 <- 2)
	(< 4 <- 4))
  (test :call 2* eql
	(10 <- 5)
	(20 <- 10)
	(30 <- 15))
  (test :call - (> 10)
	(<- 12 8)
	(<- 16 10))
  (test :call (lambda (n) (* n 10)) eql
	(100 <- 10))
  (test :call (lambda (x y) (* x y)) =
	(27 <- 3 9)
	(56 <- 7 8))
  (test :call (lambda () 30) (eql 30)
	())
  (test :call (constantly 40) eql
	(40))
  (test :call (constantly 40) eql
	(40 <-))
  (test :call (constantly 40) (eql 40)
	())
  (test :call (constantly 40) (eql 40)
	(<-))
  (test :call 2* eql))

(deftest macro
  (test :macro (n m) `(= 10 (+ ,n ,m))
	(2 8)
	(4 (+ 3 3))
	((+ 2 3) (+ 1 4))))

(deftest define-test-type
  (test between 10 20 13)
  (test (between -30) 0 -24)
  (test (:each (between 5 9)
	       (7)
	       (6)
	       (9))))
