(in-package :cl-user)
(defpackage :butter.sample.stub
  (:use :cl)
  (:export :generic-plus
           :positive-even-p
           :aand
           :it
           :sample-condition
           :sample-warning
           :sample-error))
(in-package :butter.sample.stub)

(defgeneric generic-plus (x y)
  (:method ((x number) (y number))
    (+ x y))
  (:method ((x string) (y string))
    (concatenate 'string x y)))

(defun positive-even-p (n)
  (and (> n 0) (evenp n)))

(defmacro aand (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(let ((it ,(car args)))
              (declare (ignorable it))
              (if it (aand ,@(cdr args)))))))

(define-condition sample-condition (condition) ())
(define-condition sample-warning (warning) ())
(define-condition sample-error (error) ())
