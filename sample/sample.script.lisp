(require :asdf)
(require :butter)
(use-package :butter)

(is 10)
(is (= 5 (+ 2 3)))
(is (equal '(1 2 3 4) (append '(1 2) '(3 4))))
