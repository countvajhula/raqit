#lang racket/base

(provide #%hash)

(require syntax/parse/define
         (for-syntax racket/base))

(define-syntax-parse-rule (#%hash v ...)
  (hash v ...))
