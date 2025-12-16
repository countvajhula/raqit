#lang racket/base

(provide configure)

(require (only-in "reader.rkt" make-raqit-readtable current-syntax-introducer make-intro)
         (only-in "printer.rkt" pr))

(define (raqit-print-handler v)
  (unless (void? v)
    (pr v)))

(define (configure data)
  (current-syntax-introducer (make-intro))
  (current-readtable (make-raqit-readtable))
  (current-print raqit-print-handler))
