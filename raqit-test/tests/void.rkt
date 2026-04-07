#lang racket/base

(provide tests)

(require (only-in raqit void)
         (prefix-in b: racket/base)
         rackunit
         rackunit/text-ui)

(define tests
  (test-suite
   "void tests"

   (test-suite
    "basic"
    (test-equal? "literal void"
                 void
                 (b:void)))))

(module+ main
  (b:void
   (run-tests tests)))
