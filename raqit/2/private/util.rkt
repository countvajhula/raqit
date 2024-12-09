#lang racket/base

(provide define-alias)

(require syntax/parse/define
         (for-syntax racket/base))

(define-syntax-parse-rule (define-alias alias:id name:id)
  (define-syntax alias (make-rename-transformer #'name)))
