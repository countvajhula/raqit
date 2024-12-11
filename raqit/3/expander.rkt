#lang racket/base

(provide (except-out (all-from-out racket/base)
                     #%module-begin
                     #%app)
         (rename-out [this-module-begin #%module-begin])
         (all-from-out "app.rkt")
         (rename-out [raqit-app #%app])
         (all-from-out "def.rkt")
         (all-from-out "let.rkt")
         (all-from-out "fun.rkt")
         (all-from-out "class.rkt")
         (all-from-out "flow.rkt")
         (all-from-out "macro.rkt")
         (all-from-out qi))

(require (rename-in "app.rkt"
                    [#%app raqit-app])
         (only-in qi switch)
         "def.rkt"
         "let.rkt"
         "fun.rkt"
         "class.rkt"
         "flow.rkt"
         "macro.rkt"
         syntax/parse/define
         (for-syntax racket/base))

(define-syntax-parser this-module-begin
  [(_ EXPR ...)
   #'(#%module-begin
      EXPR ...)])
