#lang raqit

(require (only-in racket
                  sqr)
         (for-syntax racket/base))

(let (a 5
      b 3)
  (+ a b))

(let (v 3)
  (cond (< v 0) 'negative
        (> v 0) 'positive
        else 'zero))

(def (do-something v)
  (let (v (abs v))
    v))

(flow check-number
  (switch
    (< 0) 'negative
    (> 0) 'positive
    else 'zero))

(map check-number [1 -2 0])

(def v -3)
(map {~> sqr add1} [1 2 (abs v) 4 5])

(switch (3)
  (< 0) 'negative
  (> 0) 'positive
  else 'zero)

(defsyntax (where expr bindings)
  #'(let bindings expr))

(where (+ v 3)
       (v 5))

(defsyntax hello
  [(_ name) #'(displayln (format "hello ~a" name))]
  [(_) #'(displayln (format "hello ~a" "world"))])

(hello "friend")
