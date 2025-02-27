#lang racket/base

(provide def)

(require syntax/parse/define
         (for-syntax racket/base))

(define-syntax-parser def
  [(_ (var ...) values-expr)
   #:with values-expr-user (datum->syntax this-syntax #'values-expr)
   #'(define-values (var ...) values-expr-user)]
  [(_ var val) #'(define var val)])
