(require :asdf)
(require :butter)
(use-package :butter)

(butter.cui:begin
 (test t 10)
 (test = 5 (+ 2 3))
 (test equal '(1 2 3 4) (append '(1 2) '(3 4))))
