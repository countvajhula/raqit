#lang racket/base

(provide let)

(require racket/match
         syntax/parse/define
         (for-syntax racket/base))

(define-syntax-parser let
  [(_ name:id arg ...)
   (raise-syntax-error #f
                       "Named 'let' is not supported; use 'loop' for recursion."
                       this-syntax
                       #'name)]
  [(_ arg ...) #'(match-let arg ...)])
