(in-package :cl-user)

(defpackage :butter.system
  (:use :cl :asdf))
(in-package :butter.system)

(defsystem butter
    :name "butter"
    :author "yuushimizu"
    :version "0.0.1"
    :description "An unit testing frame"
    :components ((:file "util")
                 (:file "butter" :depends-on ("util"))
                 (:file "cui" :depends-on ("butter"))))
