#lang racket/base

(provide #%hash)

(require syntax/parse/define
         racket/match
         (for-syntax racket/base))

(define-match-expander #%hash
  ;; use in pattern-matching context
  (syntax-parser
    [(_ v ...)
     ;; Use #:open to allow extra keys (subset matching)
     #'(hash v ... #:open)])

  ;; use in expression context
  (syntax-parser
    [(_ v ...)
     #'(hash v ...)]))
