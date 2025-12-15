#lang racket/base

(provide #%set)

(require racket/set)

(require syntax/parse/define
         (for-syntax racket/base))

(define-syntax-parse-rule (#%set v ...)
  (set v ...))
