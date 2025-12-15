#lang racket/base

(provide (rename-out [squarelist-app #%app])
         :)

(require racket/match
         (for-syntax racket/base
                     syntax/parse
                     syntax/parse/class/paren-shape)
         (only-in racket [#%app racket:app]))

(define-match-expander :
  (syntax-parser
    [(: v ... vs)
     #'(list* v ... vs)])
  (syntax-parser
    [(_ v ...)
     #'(list* v ...)]))

(define-syntax squarelist-app
  (syntax-parser
    [(~brackets _ x ...)
     (syntax/loc this-syntax
       (list x ...))]
    [(_ x ...)
     (syntax/loc this-syntax
       (racket:app x ...))]))
