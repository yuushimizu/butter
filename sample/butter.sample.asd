(in-package :cl-user)
(defpackage :butter.sample.system
  (:use :cl :asdf))
(in-package :butter.sample.system)

(defsystem butter.sample
    :version "1.0.0"
    :author "yuushimizu"
    :description "Samples of Butter"
    :depends-on (:butter)
    :components ((:file "sample.stub")
                 (:file "sample" :depends-on ("sample.stub"))
                 (:file "sample.fail" :depends-on ("sample.stub"))
                 (:file "sample.extending" :depends-on ("sample.stub")))
    :in-order-to ((test-op (load-op butter)))
    :perform (test-op :after (op c)
                      (funcall (intern (symbol-name '#:run-tests) :butter) :butter.sample)))
