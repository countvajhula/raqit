#lang raqit

;; racket/base is included out of the box,
;; but you can include any other Racket collections
;; using `use`
(use (only-in racket
              sqr)
     (for-syntax racket/base))

;; basic syntax: () and [] are generally interchangeable
(let ([a 1]) a)
(let ((a 1)) a)

;; expressions: paren shape matters for data literals
[1 2 3]  ; list
{'a 1 'b 2 'c 3}  ; hash
#{1 2 3 1}  ; set
☯(~> sqr add1)  ; flow

;; use `def` for ordinary definitions
(def a 3)

;; def supports multiple values
(def (p q) (values 1 2))

;; use `fun` to define functions
(fun do-something (v)
  (def w (abs v))
  w)

;; fun supports specifying multiple arities ("case-lambda")
(fun do-something-too
  [(v) "v"]
  [(v w) "v and w"])

;; `flow` defines a Qi flow

(flow transform
  (~> sqr (* 2) add1))

(transform 3)

;; lists

;; use : to construct a list from existing lists
(: 1 2 [3 4 5])

;; let, def, and loop are all pattern-matching
(let ([[: x xs] [1 2 3]]
      [y 3])
  [x y])

(def [: 1 2 vs] [: 1 2 [3 4 5]])

(def #{x y} #{1 2})

(def {'a _ 'b val 'c _} {'a 1 'b 2 'c 3})

[vs x y val]

;; conditionals

;; In addition to `if` and `cond`, there's `switch`
;; which implicitly conditions on the inputs

(switch (3)
  [(< 0) 'negative]
  [(> 0) 'positive]
  [else 'zero])

;; loops

;; Use the loop form for recursion. Like def and let,
;; it exhibits pattern-matching bindings
(loop go ([a 5])
  (when (> a 0)
    (displayln a)
    (go (sub1 a))))

(loop go ([[: x xs] [1 2 3 4 5]]
          [a 5])
  (displayln x)
  (unless (null? xs)
    (go xs a)))

;; sequencing

;; sequence multiple ordinary expressions using `do`
(do
  (displayln "hello")
  (displayln "goodbye"))

;; sequence flows of values using `~>`
(~> (1 2 3) + sqr add1)

;; you can easily mix ordinary expressions and flows
(fun key-sum (a b c #:key k)
  (~> (a b c) (>< k) +))

(fun eqwal (x y #:key k)
  (equal? (k x) (k y)))

(eqwal 3 5 #:key ☯(remainder 3))

(~> ([1 2 3]) (esc (λ (xs) (def [: y ys] xs) y)))

(flow check-number
  (switch
    [(< 0) 'negative]
    [(> 0) 'positive]
    [else 'zero]))

(map check-number [1 -2 0])

(def v -3)
(map ☯(~> sqr add1) [1 2 (abs v) 4 5])
(map ☯(switch [positive? add1]
              [else sub1])
     [1 -2 (abs v) -4 5])

(~> (3) sqr add1)

;; Relations

;; equality is generic across types, and supports specifying a key
(= #:key string-upcase "abc" "ABC")

;; order relations are generic, too, and also support a key
(< 1 2 3)

(< "apple" "banana" "cherry")

(< "abc" "PqR")

(< #:key string-upcase "abc" "PqR")

(sort [2 3 1] <)

(sort ["abc" "AAB" "Def" "aaa"] <)

(sort ["abc" "AAB" "Def" "aaa"] ☯(< #:key string-upcase))

(~> (["abc" "AAB" "Def" "aaa"]) (sort <))

;; append values using ~, which is also generic across types

(~ [1 2 3] [4 5 6] [7 8 9])

(~ "abc" "def" "ghijk")

;; User-defined datatypes

;; struct is similar to Racket's
(struct dog (name age))

(dog "fido" 5)

;; but with simple syntax for implementing
;; "protocols" (generic interfaces)
(protocol stack
          (push stack element)
          (pop stack)
          (top stack))

(struct dogg (name age)
  (implements stack
    (fun push (this element)
      'hi)
    (fun pop (this)
      'hi)
    (fun top (this)
      'hi)))

;; You can easily define macros, for both ordinary
;; expressions as well DSL positions (e.g., Qi flows)
;; using a common `macro` form

;; simple pattern
(macro (where expr bindings)
  (def bindings expr))

(where (+ v 3)
  abc)

;; multiple patterns
(macro hello
  [(_ name) #'(displayln (format "hello ~a" name))]
  [(_) #'(displayln (format "hello ~a" "world"))])

(hello "sid")

;; you can write flow macros using the same macro form,
;; by prefacing with ☯
(macro ☯(<~ f g)
  (~> g f))

(map ☯(<~ add1 sqr) [1 2 3])

;; Classes are very similar to Racket classes
;; but support a more familiar method syntax
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
