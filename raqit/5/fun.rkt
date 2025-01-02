#lang racket/base

(provide fun)

(require syntax/parse/define
         (for-syntax racket/base))

(define-syntax-parser fun
  [(_ name (arg ...) body ...)
   #'(define (name arg ...)
       body ...)]
  [(_ name args body ...)
   #'(define (name . args)
       body ...)])
