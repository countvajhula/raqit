#lang racket/base

(provide #%lang-datum)

(require syntax/parse/define
         (for-syntax racket/base))

(define-syntax-parser #%lang-datum
  [(_ . #[e ...])
   (syntax/loc this-syntax (vector-immutable e ...))]
  [(_ . e)
   (syntax/loc this-syntax (#%datum . e))])
