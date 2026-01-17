#lang racket/base

(provide tests)

(require (only-in raqit #%hash)
         rackunit
         rackunit/text-ui)

(define tests
  (test-suite
   "hash tests"

   (test-suite
    "basic"
    (test-equal? "happy"
                 (#%hash ['a 1] ['b 2] ['c 3])
                 (hash 'a 1 'b 2 'c 3))
    (test-equal? "one key"
                 (#%hash ['a 1])
                 (hash 'a 1))
    (test-equal? "empty"
                 (#%hash)
                 (hash)))))

(module+ main
  (void
   (run-tests tests)))
