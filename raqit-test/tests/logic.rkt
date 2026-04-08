#lang racket/base

(provide tests)

(require (only-in raqit/logic
                  the-any
                  the-all)
         (only-in raqit < <= >= >)
         (prefix-in b: racket/base)
         rackunit
         rackunit/text-ui)

(define tests
  (test-suite
   "logic tests"

   (test-suite
    "order"
    (test-suite
     "<"
     (test-false "any and any" (< the-any the-any))
     (test-true "any and one" (< the-any 23))
     (test-true "any and all" (< the-any the-all))
     (test-false "one and any" (< 23 the-any))
     (test-false "one and one" (< 23 23))
     (test-true "one and one" (< 22 23))
     (test-false "one and one" (< 23 22))
     (test-true "one and all" (< 23 the-all))
     (test-false "all and any" (< the-all the-any))
     (test-false "all and one" (< the-all 23))
     (test-false "all and all" (< the-all the-all)))
    (test-suite
     "<="
     (test-true "any and any" (<= the-any the-any))
     (test-true "any and one" (<= the-any 23))
     (test-true "any and all" (<= the-any the-all))
     (test-false "one and any" (<= 23 the-any))
     (test-true "one and one" (<= 23 23))
     (test-true "one and one" (<= 22 23))
     (test-false "one and one" (<= 23 22))
     (test-true "one and all" (<= 23 the-all))
     (test-false "all and any" (<= the-all the-any))
     (test-false "all and one" (<= the-all 23))
     (test-true "all and all" (<= the-all the-all)))
    (test-suite
     ">="
     (test-true "any and any" (>= the-any the-any))
     (test-false "any and one" (>= the-any 23))
     (test-false "any and all" (>= the-any the-all))
     (test-true "one and any" (>= 23 the-any))
     (test-true "one and one" (>= 23 23))
     (test-false "one and one" (>= 22 23))
     (test-true "one and one" (>= 23 22))
     (test-false "one and all" (>= 23 the-all))
     (test-true "all and any" (>= the-all the-any))
     (test-true "all and one" (>= the-all 23))
     (test-true "all and all" (>= the-all the-all)))
    (test-suite
     ">"
     (test-false "any and any" (> the-any the-any))
     (test-false "any and one" (> the-any 23))
     (test-false "any and all" (> the-any the-all))
     (test-true "one and any" (> 23 the-any))
     (test-false "one and one" (> 23 23))
     (test-false "one and one" (> 22 23))
     (test-true "one and one" (> 23 22))
     (test-false "one and all" (> 23 the-all))
     (test-true "all and any" (> the-all the-any))
     (test-true "all and one" (> the-all 23))
     (test-false "all and all" (> the-all the-all))))))

(module+ main
  (b:void
   (run-tests tests)))
