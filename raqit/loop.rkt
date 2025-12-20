#lang racket/base

(provide loop)

(require racket/match
         syntax/parse/define
         (for-syntax racket/base))

(begin-for-syntax
  (define-syntax-class binding-pair
    #:description "binding pair (e.g., [x 5])"
    (pattern (pat val)))

  (define-syntax-class binding-list
    #:description "list of binding pairs (e.g., ([x 1] [y 2]))"
    (pattern (pair:binding-pair ...))))

(define-syntax-parser loop
  [(_ name:id bindings:binding-list body ...+)
   #'(match-let name bindings body ...)])
