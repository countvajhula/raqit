#lang racket/base

(provide cond)

(require syntax/parse/define
         (for-syntax racket/base)
         (prefix-in b: racket/base))

(define-syntax-parser cond
  [(_ (~seq p c) ...)
   #'(b:cond [p c] ...)])
