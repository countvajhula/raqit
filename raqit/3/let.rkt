#lang racket/base

(provide let
         :)

(require racket/match
         syntax/parse/define
         syntax/parse
         (for-syntax racket/base
                     syntax/parse/class/paren-shape))

(define-match-expander :
  (syntax-parser [(: v vs) #'(cons v vs)]))

(define-syntax-parser let
  [(_ (~parens pat ...) values-expr)
   #:with values-expr-user (datum->syntax this-syntax #'values-expr)
   #'(match-define-values (pat ...) values-expr-user)]
  [(_ pat val) #'(match-define pat val)])
