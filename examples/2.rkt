#lang raqit/2

(require (only-in racket
                  sqr)
         (for-syntax racket/base)
         ;; TODO: why do we need this?
         raqit/2)

(let ([a 5]
      [b 3])
  (+ a b))

(let ([v 3])
  (cond [(< v 0) 'negative]
        [(> v 0) 'positive]
        [else 'zero]))

(fun do-something (v)
  (let ([v (abs v)])
    v))

(fun do-something2 (v)
  (def v (abs v))
  v)

(flow check-number
  (switch
    [(< 0) 'negative]
    [(> 0) 'positive]
    [else 'zero]))

(map check-number [1 -2 0])

(def v -3)
(map {~> sqr add1} [1 2 (abs v) 4 5])

(macro (where expr bindings)
  #'(let bindings expr))

(where (+ v 3)
       ([v 5]))

(switch (3)
  [(< 0) 'negative]
  [(> 0) 'positive]
  [else 'zero])

(macro hello
  [(_ name) #'(displayln (format "hello ~a" name))]
  [(_) #'(displayln (format "hello ~a" "world"))])

(hello "friend")

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

