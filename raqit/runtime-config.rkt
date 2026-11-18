#lang racket/base

(provide configure)

(require (only-in "reader.rkt" make-raqit-readtable)
         (only-in "printer.rkt" pr))

(define (raqit-print-handler v)
  (unless (void? v)
    (pr v)))

(define (configure data)
  (current-readtable (make-raqit-readtable))
  (current-print raqit-print-handler))
