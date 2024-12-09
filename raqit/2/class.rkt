#lang racket/base

(provide (rename-out [class2 class])
         (except-out (all-from-out racket/class)
                     class))

(require syntax/parse/define
         (for-syntax racket/base)
         racket/class)

(define-syntax-parser class2
  [(_ name (parent) body ...)
   #'(define name (class parent body ...))])
