#lang racket/base

(provide defsyntax)

(require syntax/parse/define
         (for-syntax racket/base))

(define-syntax-parser defsyntax
  [(_ (name pattern ...) template)
   #'(define-syntax-parser name
       [(_ pattern ...) template])]
  [(_ name:id parse-option ... [pattern template] ...)
   #'(define-syntax-parser name
       parse-option ...
       [pattern template]
       ...)])
