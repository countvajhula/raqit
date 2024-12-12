#lang raqit/3

(require (only-in racket
                  sqr)
         (for-syntax racket/base))

(def a 3)

(let a 3)

(def (a b) (values 1 2))

(let (b c) (values 3 5))

(let [: x xs] [1 2 3])

(displayln (+ a b c x))

(let w 3)
(cond [(< w 0) 'negative]
      [(> w 0) 'positive]
      [else 'zero])

(loop go (a 5)
  (when (> a 0)
    (displayln a)
    (go (sub1 a))))

(loop go ([[: x xs] [1 2 3 4 5]]
          [a 5])
  (displayln x)
  (unless (null? xs)
    (go xs a)))

(fun do-something (v)
  (def w (abs v))
  w)

(fun do-something2 (abc [: x xs])
  (displayln abc)
  (displayln x))

(do-something2 "hi" [1 2 3])

(flow check-number
  (switch
    [(< 0) 'negative]
    [(> 0) 'positive]
    [else 'zero]))

(map check-number [1 -2 0])

(def v -3)
(map {~> sqr add1} [1 2 (abs v) 4 5])
(map {switch [positive? add1] [else sub1]} [1 -2 (abs v) -4 5])

(~> (3) sqr add1)

(macro (where expr bindings)
  #'(let bindings expr))

(where (+ v 3)
       abc)

(switch (3)
  [(< 0) 'negative]
  [(> 0) 'positive]
  [else 'zero])

(macro hello
  [(_ name) #'(displayln (format "hello ~a" name))]
  [(_) #'(displayln (format "hello ~a" "world"))])

(hello "sid")

(class fish% (object%)
  (init size)
  (super-new)
  (define current-size size)
  (define/public (get-size)
    current-size)
  (define/public (grow amt)
    (set! current-size (+ amt current-size)))
  (define/public (eat other-fish)
    (grow (send other-fish get-size))))

(define charlie
  (new fish% [size 10]))

(send charlie get-size)
