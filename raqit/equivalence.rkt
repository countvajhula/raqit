#lang racket/base

(provide ==)

(define (== #:key [key #f] a b)
  (if key
      (== (key a) (key b))
      (equal? a b)))
