#lang racket/base

(provide #%set)

(require racket/set
         racket/match)

(require syntax/parse/define
         (for-syntax racket/base))

(define-match-expander #%set
  ;; use in pattern-matching context (subset matching)
  (syntax-parser
    [(_ v ...)
     ;; The `_ ...` allows extra elements to exist in the set
     #'(? set? (app set->list (list-no-order v ... _ (... ...))))])
  ;; use in expression context
  (syntax-parser
    [(_ v ...)
     #'(set v ...)]))
