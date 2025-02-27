#lang racket/base

(provide (rename-out [squarelist-app #%app]))

(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/class/paren-shape)
         (only-in racket [#%app racket:app]))

(define-syntax squarelist-app
  (syntax-parser
    [(~brackets _ x ...)
     (syntax/loc this-syntax
       (list x ...))]
    [(_ x ...)
     (syntax/loc this-syntax
       (racket:app x ...))]))
