#lang racket/base

(provide (except-out (all-from-out racket/base)
                     #%module-begin
                     #%app)
         (rename-out [this-module-begin #%module-begin])
         (all-from-out "app.rkt")
         (rename-out [raqit-app #%app])
         (all-from-out "do.rkt")
         (all-from-out "def.rkt")
         (all-from-out "let.rkt")
         (all-from-out "loop.rkt")
         (all-from-out "fun.rkt")
         (all-from-out "class.rkt")
         (all-from-out "flow.rkt")
         (all-from-out "macro.rkt")
         (all-from-out "equivalence.rkt")
         (all-from-out "struct.rkt")
         (all-from-out "generic.rkt")
         (all-from-out "append.rkt"))

(require (rename-in "app.rkt"
                    [#%app raqit-app])
         "do.rkt"
         "def.rkt"
         "let.rkt"
         "loop.rkt"
         "fun.rkt"
         "class.rkt"
         "flow.rkt"
         "macro.rkt"
         "equivalence.rkt"
         "struct.rkt"
         "generic.rkt"
         "append.rkt"
         syntax/parse/define
         (for-syntax racket/base))

(define-syntax-parser this-module-begin
  [(_ EXPR ...)
   #'(#%module-begin
      EXPR ...)])
