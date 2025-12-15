#lang racket/base

(provide #%set)

(require racket/set
         racket/match)

(require syntax/parse/define
         (for-syntax racket/base))

(define-match-expander #%set
  ;; use in pattern-matching context
  (syntax-parser
    [(_ v ...)
     #'(? set? (app set->list (list-no-order v ...)))])
  ;; use in expression context
  (syntax-parser
    [(_ v ...)
     #'(set v ...)]))
