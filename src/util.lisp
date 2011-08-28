(in-package :cl-user)
(defpackage :butter.util
  (:use :cl)
  (:export :eval-always
           :with-gensyms
           :capture-conditions
           :namespace
           :plist-reduce
           :plist-split
           :plist-merge))
(in-package :butter.util)

(defmacro eval-always (&body forms)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@forms))

(defmacro with-gensyms ((&rest vars) &body forms)
  `(let ,(loop for var in vars collect `(,var (gensym ,(princ-to-string var)))) ,@forms))

(defun add-or-replace (item list &key (key #'identity) (test #'eql))
  (labels ((rec (&optional (acc ()) (rest list))
             (cond ((null rest) (nreverse (push item acc)))
                   ((funcall test (funcall key item) (car rest)) (append (nreverse (push item acc)) (cdr rest)))
                   (t (rec (push (car rest) acc) (cdr rest))))))
    (rec)))

(defmacro capture-conditions (&body forms)
  "Execute the forms and return the result of the last form and conditions that are signalled from the forms. Discard all but the first values that are returned from the last form."
  (with-gensyms (conditions% condition%)
    `(let ((,conditions% ()))
       (values
        (handler-bind ((t (lambda (,condition%) (push ,condition% ,conditions%))))
          ,@forms)
        (nreverse ,conditions%)))))

(defmacro namespace (package-name &rest arguments-for-defpackage)
  `(progn
     (in-package :cl-user)
     (defpackage ,package-name
       ,@arguments-for-defpackage)
     (in-package ,package-name)
     *package*))

(defun plist-reduce (function plist &key (initial-value nil initial-value-supplied-p))
  (if (cdr plist)
      (plist-reduce function (cddr plist) :initial-value (if initial-value-supplied-p
                                                             (funcall function initial-value (first plist) (second plist))
                                                             (second plist)))
      (if initial-value-supplied-p initial-value (funcall function))))

(defun plist-split (predicate plist)
  (plist-reduce (lambda (matched/not-matched key value)
                  (if (funcall predicate key value)
                      (list (list* key value (first matched/not-matched)) (second matched/not-matched))
                      (list (first matched/not-matched) (list* key value (second matched/not-matched)))))
                plist
                :initial-value '(() ())))

(defun plist-merge (function plist1 plist2)
  (let ((notfound '#:notfound))
    (destructuring-bind (notfound-in-plist1 found-in-both)
        (plist-split (lambda (key value)
                       (declare (ignore value))
                       (eq notfound (getf plist1 key notfound)))
                     plist2)
      (revappend
       (plist-reduce (lambda (merged key value)
                       (list* (let ((value-in-plist2 (getf found-in-both key notfound)))
                                (if (eq notfound value-in-plist2)
                                    value
                                    (funcall function value value-in-plist2)))
                              key
                              merged))
                     plist1
                     :initial-value ())
       notfound-in-plist1))))
