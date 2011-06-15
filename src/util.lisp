(in-package :cl-user)
(defpackage :butter.util
  (:use :cl))
(in-package :butter.util)

(export 'with-gensyms)
(defmacro with-gensyms ((&rest vars) &rest body)
  `(let ,(loop for var in vars collect `(,var (gensym ,(princ-to-string var)))) ,@body))
