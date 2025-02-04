#lang racket/base

(provide loop)

(require racket/match
         syntax/parse/define
         syntax/parse
         (prefix-in my: "def.rkt")
         (for-syntax racket/base))

(define-syntax-parser loop
  [(_ name:id ((pat val) ...) body ...)
   #:with (arg ...) (generate-temporaries #'(pat ...))
   #'((letrec ([name (λ (arg ...)
                       (my:def (pat ...) (values arg ...))
                       body ...)])
        name) val ...)]
  ;; TODO: maybe remove this special case.
  ;; there could be patterns like [a b] and then
  ;; it would match the first clause
  [(_ name:id (pat val) body ...)
   #:with arg (car (generate-temporaries (list #'pat)))
   #'((letrec ([name (λ (arg)
                       (my:def pat arg)
                       body ...)])
        name) val)])
