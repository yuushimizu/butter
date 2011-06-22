(in-package :cl-user)

(defpackage :butter.test.system
  (:use :cl :asdf))
(in-package :butter.test.system)

(defsystem butter.test
    :name "butter.test"
    :author "yuushimizu"
    :version "0.0.1"
    :description "Test for butter"
    :depends-on (:butter)
    :components ((:file "butter.test")
                 (:file "butter.failed-test")))
