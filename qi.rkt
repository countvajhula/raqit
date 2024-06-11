#lang racket/base

;; NOTE: many require/provide and subform combinations with binding
;; spaces (e.g. except-in/out, rename-in/out) didn't work. Investigate
;; as it may reflect a bug.
(provide (all-from-out qi)
         (rename-out [racket-switch switch])
         (for-space qi
                    (rename-out [qi-switch switch])))

(require (rename-in qi [switch qi:switch])
         syntax/parse/define
         (for-syntax racket/base))

(define-qi-syntax-parser qi-switch
  [(_ (~seq p c) ...)
   #'(qi:switch [p c] ...)])

(define-syntax-parser racket-switch
  [(_ (v ...) (~seq p c) ...)
   #'(qi:switch (v ...) [p c] ...)])
