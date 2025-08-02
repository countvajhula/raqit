#lang raqit

(use (only-in racket
              sqr)
     (for-syntax racket/base))

(protocol stack
  (push stack element)
  (pop stack)
  (top stack))

(struct dogg (name age)
  #:methods gen:stack
  [(fun push (this element)
     'hi)
   (fun pop (this)
     'hi)
   (fun top (this)
     'hi)])

(fun key-sum (a b c #:key k)
  (~> (a b c) (>< k) +))

(def a 3)

(def (p q) (values 1 2))

(let ([[: x xs] [1 2 3]])
  x)

(let ([b 3] [c 5])

  (def [: x xs] [1 2 3])

  (displayln (+ a b c x)))

(= #:key string-upcase "abc" "ABC")

(def w 3)

(struct dog (name age))

(dog "fido" 5)

(cond [(< w 0) 'negative]
      [(> w 0) 'positive]
      [else 'zero])

(fun do-something (v)
  (def w (abs v))
  w)

(fun eqwal (x y #:key k)
  (equal? (k x) (k y)))

(eqwal 3 5 #:key {(remainder 3)})

(~> ([1 2 3]) (esc (λ (xs) (def [: y ys] xs) y)))

(flow check-number
  (switch
    [(< 0) 'negative]
    [(> 0) 'positive]
    [else 'zero]))

(map check-number [1 -2 0])

(def v -3)
(map (~> sqr add1) [1 2 (abs v) 4 5])
(map (switch [positive? add1] [else sub1]) [1 -2 (abs v) -4 5])

(~> (3) sqr add1)

(macro (where expr bindings)
  #'(def bindings expr))

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

(flow-macro (rev f g)
  #'(~> g f))

(map (rev add1 sqr) [1 2 3])

(class fish% (object%)
  (init size)
  (super-new)
  (def current-size size)
  (method get-size ()
    current-size)
  (method grow (amt)
    (set! current-size (+ amt current-size)))
  (method eat (other-fish)
    (grow (send other-fish get-size))))

(define charlie
  (new fish% [size 10]))

(send charlie get-size)

(loop go (a 5)
  (when (> a 0)
    (displayln a)
    (go (sub1 a))))

(loop go ([[: x xs] [1 2 3 4 5]]
          [a 5])
  (displayln x)
  (unless (null? xs)
    (go xs a)))

(~ [1 2 3] [4 5 6] [7 8 9])

(~ "abc" "def" "ghijk")
