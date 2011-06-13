(in-package :cl-user)

(defpackage :butter-system
  (:use :cl :asdf))
(in-package :butter-system)

(defsystem butter
    :name "butter"
    :author "yuushimizu"
    :version "0.0.1"
    :description "An unit testing frame"
    :components ((:file "packages")
		 (:file "butter" :depends-on ("packages"))
                 (:file "butter.cui" :depends-on ("packages" "butter"))))
