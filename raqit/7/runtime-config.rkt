#lang racket/base

(provide configure)

(require (only-in "reader.rkt" make-raqit-readtable current-syntax-introducer make-intro)
         (only-in "printer.rkt" pr))

(define (configure data)
  (current-syntax-introducer (make-intro))
  (current-readtable (make-raqit-readtable))
  (current-print (make-print-proc (current-print))))

(struct raqit-pr-thing (v)
  #:property prop:custom-write
  (λ (this out mode)
    (pr (raqit-pr-thing-v this) #:out out)))

(define ((make-print-proc orig-print-proc) v)
  (orig-print-proc (raqit-pr-thing v)))
