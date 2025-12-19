#lang racket/base

(require rackunit
         rackunit/text-ui
         (prefix-in lambda: "lambda.rkt"))

(define tests
  (test-suite
   "raqit tests"

   lambda:tests))

(module+ test
  (void
   (run-tests tests)))
