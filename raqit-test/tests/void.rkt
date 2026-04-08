#lang racket/base

(provide tests)

(require (only-in raqit void)
         (prefix-in b: racket/base)
         (only-in racket/function thunk)
         rackunit
         rackunit/text-ui
         syntax/macro-testing)

(define tests
  (test-suite
   "void tests"

   (test-suite
    "basic"
    (test-equal? "literal void"
                 void
                 (b:void))
    (test-true "void as a value in a list"
               (not (not (member void [void 1 2]))))
    (test-exn "void used as a function"
              exn:fail?
              (thunk (convert-compile-time-error
                      (void)))))))

(module+ main
  (b:void
   (run-tests tests)))
