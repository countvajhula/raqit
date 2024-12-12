#lang racket/base

(provide loop)

(require racket/match
         syntax/parse/define
         syntax/parse
         (prefix-in my: "let.rkt")
         (for-syntax racket/base))

(define-syntax-parser loop
  [(_ name:id ((pat val) ...) body ...)
   #:with (arg ...) (generate-temporaries #'(pat ...))
   #'((letrec ([name (λ (arg ...)
                       (my:let (pat ...) (values arg ...))
                       body ...)])
        name) val ...)]
  [(_ name:id (pat val) body ...)
   #:with arg (car (generate-temporaries (list #'pat)))
   #'((letrec ([name (λ (arg)
                       (my:let pat arg)
                       body ...)])
        name) val)])
