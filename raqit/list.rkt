#lang racket/base

(provide :)

(require racket/match
         (for-syntax racket/base
                     syntax/parse))

(define-match-expander :
  (syntax-parser
    [(: v ... vs)
     #'(list* v ... vs)])
  (syntax-parser
    [(_ v ...)
     #'(list* v ...)]))
