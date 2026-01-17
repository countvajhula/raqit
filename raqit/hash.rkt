#lang racket/base

(provide #%hash)

(require syntax/parse/define
         racket/match
         syntax/parse
         (for-syntax racket/base))

(define-match-expander #%hash
  ;; use in pattern-matching context
  (syntax-parser
    ;; if the pattern ends with `_ ...`
    ;; use subset matching
    [(_ [k v] ... (~datum _) (~datum ...))
     #'(hash (~@ k v) ... #:open)]
    ;; default to strict matching
    [(_ [k v] ...)
     #'(hash (~@ k v) ...)])

  ;; use in expression context
  (syntax-parser
    [(_ [k v] ...)
     #'(hash (~@ k v) ...)]))
