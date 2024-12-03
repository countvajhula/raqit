#lang racket/base

(provide (except-out (all-from-out racket/base)
                     #%module-begin
                     #%app)
         (rename-out [this-module-begin #%module-begin])
         (all-from-out "app.rkt")
         (rename-out [raqit-app #%app])
         (all-from-out "let.rkt")
         (all-from-out "cond.rkt")
         (all-from-out "switch.rkt")
         (all-from-out "def.rkt")
         (all-from-out "flow.rkt")
         (all-from-out "macro.rkt"))

(require (rename-in "app.rkt"
                    [#%app raqit-app])
         "let.rkt"
         "cond.rkt"
         "switch.rkt"
         "def.rkt"
         "flow.rkt"
         "macro.rkt"
         syntax/parse/define
         (for-syntax racket/base))

(define-syntax-parser this-module-begin
  [(_ EXPR ...)
   #'(#%module-begin
      EXPR ...)])

