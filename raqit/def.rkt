#lang racket/base

(provide def)

(require racket/match
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse/class/paren-shape))

(define-syntax-parser def
  [(_ (~parens pat ...) values-expr)
   #:with values-expr-user (datum->syntax this-syntax #'values-expr)
   #'(match-define-values (pat ...) values-expr-user)]
  [(_ pat val) #'(match-define pat val)])
