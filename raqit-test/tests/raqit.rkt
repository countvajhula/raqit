#lang racket/base

(require rackunit
         rackunit/text-ui
         (prefix-in lambda: "lambda.rkt")
         (prefix-in hash: "hash.rkt")
         (prefix-in void: "void.rkt"))

(define tests
  (test-suite
   "raqit tests"

   lambda:tests
   hash:tests
   void:tests))

(module+ test
  (void
   (run-tests tests)))
