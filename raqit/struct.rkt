#lang racket/base

(provide struct)

(require syntax/parse/define
         (prefix-in b: racket/base)
         (for-syntax racket/base))

(define-syntax-parser struct
  [(_ id (field ...) struct-option ...)
   #'(b:struct id (field ...) #:transparent struct-option ...)]
  [(_ id super (field ...) struct-option ...)
   #'(b:struct id super (field ...) #:transparent struct-option ...)])
