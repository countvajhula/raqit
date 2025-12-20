#lang racket/base

(provide loop)

(require racket/match
         syntax/parse/define
         (for-syntax racket/base))

(define-syntax-parser loop
  [(_ name:id arg ...)
   #'(match-let name arg ...)])
