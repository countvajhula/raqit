#lang racket/base

(provide #%datum)

(require (only-in racket/base
                  [#%datum racket:datum])
         syntax/parse/define
         (for-syntax racket/base))

(define-syntax-parser #%datum
  [(_ . #[e ...])
   (syntax/loc this-syntax (vector-immutable e ...))]
  [(_ . e)
   (syntax/loc this-syntax (racket:datum . e))])
