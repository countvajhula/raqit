#lang racket/base

(provide fun)

(require syntax/parse/define
         (prefix-in my: "let.rkt")
         (for-syntax racket/base
                     syntax/parse/class/paren-shape))

(define-syntax-parser fun
  [(_ name (~parens pat ...) body ...)
   #:with (arg ...) (generate-temporaries #'(pat ...))
   #'(define (name arg ...)
       (my:let (pat ...) (values arg ...))
       body ...)]
  [(_ name pat body ...)
   #'(define (name . args)
       (my:let pat args)
       body ...)])
