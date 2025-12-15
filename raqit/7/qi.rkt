#lang racket/base

(provide (except-out (all-from-out qi)
                     flow)
         (rename-out [flow #%flow])
         (all-from-out qi/list)
         flow-macro)

(require qi/list
         qi
         syntax/parse/define
         (for-syntax racket/base))

(define-syntax-parser flow-macro
  [(_ (name pattern ...) template)
   #'(define-qi-syntax-parser name
       [(_ pattern ...) template])]
  [(_ name:id parse-option ... [pattern template] ...)
   #'(define-qi-syntax-parser name
       parse-option ...
       [pattern template]
       ...)])
