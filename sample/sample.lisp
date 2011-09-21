(in-package :cl-user)
(defpackage :butter.sample
  (:use :cl :butter :butter.sample.stub))
(in-package :butter.sample)

(deftest sample-equivalence
  (is (eq 'foo (identity 'foo)))
  (is (eql 10 (+ 8 2)))
  (is (= 10 (+ 2.7 7.3)))
  (is (char= #\F (elt "FOO" 0)))
  (is (string= "FOOBAR" (concatenate 'string "FOO" "BAR")))
  (is (equal '(1 2 3 4) (append '(1 2) '(3 4))))
  (is (equalp "foobar" "FOOBAR")))

(deftest sample-function
  (is (< 10 20))
  (is (typep 10 'integer))
  (is (evenp 20))
  (is (find 30 '(1 2 3 4) :key (lambda (n) (* 10 n))))
  (is (positive-even-p 10))
  (is (not (positive-even-p 13))))

(deftest sample-macro
  (is (and (= 10 (+ 2 8)) (positive-even-p 14)))
  (is (or (= 10 (+ 1 2)) (positive-even-p 20) (error "This expression is never evaluated.")))
  (is (aand (* 14 2) (positive-even-p it))))

(deftest sample-special-operator
  (is (if (positive-even-p 11) (error "This expression is never evaluated.") (= 10 (+ 1 2 3 4))))
  (is (let ((x (+ 8 2))) (= 10 x)))
  (is (flet ((f (n) (and (positive-even-p n) (> n 10)))) (f 20))))

(deftest sample-signal
  (is (:signal condition (signal "SIGNAL")))
  (is (:signal warning (warn "WARNING")))
  (is (:signal error (error "ERROR")))
  (is (:signal sample-condition (signal 'sample-condition)))
  (is (:signal sample-warning (warn 'sample-warning)))
  (is (:signal sample-error (error 'sample-error)))
  (is (:signal condition (signal 'sample-condition)))
  (is (:signal warning (warn 'sample-warning)))
  (is (:signal error (warn 'sample-error))))

(deftest sample-nested-signal
    "The :signal special assertion passes immediately when forms signals an expected condition. The following expression passes only the :signal assertion, the \"=\" assertion is ignored."
  (is (:signal condition
                  (is (= 10 (progn (signal "SIGNAL") 10))))))

(deftest sample-print
  (is (:print *standard-output* "helloworld" (princ "hello") (princ "world")))
  (is (:print *standard-output* "1 + 2 = 3" (format t "~D + ~D = ~D" 1 2 (+ 1 2))))
  (is (:print *debug-io* "debugstream" (princ "debugstream" *debug-io*))))

(deftest smaple-typep
  (is (typep (+ 10 20) 'integer))
  (is (typep (concatenate 'string "A" "B") 'string))
  (is (typep (concatenate 'string "A" "B") 'vector))
  (is (typep (generic-plus 10 20) 'integer))
  (is (typep (generic-plus "A" "B") 'string))
  (is (typep nil 'null)))

(deftest sample-with-message
  (is (= 10 (* 2 5)) "2 * 5 = 10")
  (is (string= "FOOBAR" (concatenate 'string "FOO" "BAR")) "FOO + BAR = FOOBAR")
  (is (equal '(1 2 3 4) (append '(1 2) '(3 4))) "(1 2) + (3 4) = (1 2 3 4)"))

(deftest sample-are
  (are (result x y) `(= ,result (+ ,x ,y))
       (10 3 7)
       (15 8 7)
       (9 6 3))
  (are (number) `(positive-even-p ,number)
       (10)
       (4)
       (22)
       (42))
  (are (type form) `(:signal ,type ,form)
       (condition (signal 'sample-condition))
       (warning (warn 'sample-warning))
       (error (error 'sample-error))))
