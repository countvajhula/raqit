#lang racket/base

(provide tests)

(require (only-in raqit lambda λ)
         (only-in racket/function thunk)
         rackunit
         rackunit/text-ui)

(define tests
  (test-suite
   "lambda tests"

   (test-suite
    "simple lambda"
    (let ([f (λ () 3)])
      (test-equal? "no arguments" 3 (f)))
    (let ([f (λ (a) (+ a a))])
      (test-equal? "1 argument" 6 (f 3)))
    (let ([f (λ (a b) (+ a b))])
      (test-equal? "2 arguments" 5 (f 2 3)))
    (let ([f (λ (a b . args) args)])
      (test-equal? "dotted rest argument" (list 3 4 5) (f 1 2 3 4 5)))
    (let ([f (λ args args)])
      (test-equal? "rest argument only" (list 1 2 3) (f 1 2 3))))

   (test-suite
    "keyword and optional arguments"
    (test-case "no required arguments"
      (let ([f (λ ([b 3]) b)])
        (check-equal? 3 (f) "optional argument not specified")
        (check-equal? 5 (f 5) "optional argument specified")))
    (test-case "both required and optional arguments"
      (let ([f (λ (a [b 3]) b)])
        (check-exn exn:fail:contract:arity? (thunk (f)) "required argument not specified")
        (check-equal? 3 (f 5) "optional argument not specified")
        (check-equal? 5 (f 1 5) "optional argument specified")))
    (let ([f (λ (a #:k b) b)])
      (check-exn exn:fail:contract? (thunk (f 1)) "required keyword argument not specified")
      (check-equal? 2 (f 1 #:k 2) "required keyword argument specified"))
    (let ([f (λ (a #:k [b 1]) b)])
      (check-equal? 1 (f 1) "optional keyword argument not specified")
      (check-equal? 2 (f 1 #:k 2) "optional keyword argument specified")))

   (test-suite
    "case lambda"
    (let ([f (λ [() 3]
                [(x) (+ x x)])])
      (check-equal? 3 (f))
      (check-equal? 4 (f 2)))
    (let ([f (λ [(x) x]
                [(x y) (+ x y)])])
      (check-equal? 1 (f 1))
      (check-equal? 5 (f 2 3)))
    (let ([f (λ [() 3])])
      (check-equal? 3 (f)))
    (let ([f (λ [x x])])
      (check-equal? '(3) (f 3)))
    (let ([f (λ [(x) x]
                [args (apply + args)])])
      (check-equal? 1 (f 1))
      (check-equal? 5 (f 2 3)))
    (let ([f (λ [(x) x]
                [(x . args) args])])
      (check-equal? 1 (f 1))
      (check-equal? '(2 3) (f 1 2 3))))

   (test-suite
    "edge cases and ambiguous patterns"
    (let ([f (λ (a b) (+ a b))])
      (test-equal? "2 arguments or duplicate rest args pattern?" 5 (f 2 3)))
    (test-equal? "nested lambda" (((λ (x) (λ (y) (+ x y))) 10) 20) 30)
    (test-case "single- vs multi-clause ambiguous pattern"
      (let ([f (λ (a [b 3]) (+ a b))])
        (check-exn exn:fail:contract:arity? (thunk (f)) "required argument not specified")
        (check-equal? 8 (f 5) "optional argument not specified")
        (check-equal? 6 (f 1 5) "optional argument specified"))))))

(module+ main
  (void
   (run-tests tests)))
