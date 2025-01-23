#lang racket/base

(provide (all-from-out qi/list)
         (rename-out [~map map])
         flow-macro)

(require qi/list
         qi
         syntax/parse/define
         (for-syntax racket/base))

(on ((list 1 2 3))
  (map add1))

(define-syntax-parse-rule (~map f v)
  (on (v) (map f)))

(define-syntax-parser flow-macro
  [(_ (name pattern ...) template)
   #'(define-qi-syntax-parser name
       [(_ pattern ...) template])]
  [(_ name:id parse-option ... [pattern template] ...)
   #'(define-qi-syntax-parser name
       parse-option ...
       [pattern template]
       ...)])
