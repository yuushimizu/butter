(in-package :cl-user)
(defpackage :butter.system
  (:use :cl :asdf))
(in-package :butter.system)
(defsystem butter
    :version "1.0.0"
    :author "yuushimizu"
    :description "An unit testing frame"
    :components ((:file "util")
                 (:file "core" :depends-on ("util"))
                 (:file "assertion" :depends-on ("util" "core"))
                 (:file "test-case" :depends-on ("util" "core"))
                 (:file "test-suite" :depends-on ("util" "core" "test-case"))
                 (:file "extending" :depends-on ("util" "core" "assertion" "test-case" "test-suite"))
                 (:file "cui" :depends-on ("util" "extending"))
                 (:file "butter" :depends-on ("util" "core" "assertion" "test-case" "test-suite" "cui"))))
