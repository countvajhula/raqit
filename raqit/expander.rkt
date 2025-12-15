#lang racket/base

(provide (except-out (all-from-out racket/base)
                     #%module-begin
                     #%app
                     #%datum)
         (rename-out [this-module-begin #%module-begin])
         (all-from-out "app.rkt")
         (rename-out [raqit-app #%app])
         (rename-out [raqit-datum #%datum])
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
         (all-from-out "append.rkt")
         (all-from-out "use.rkt")
         (all-from-out "qi.rkt"))

(require (rename-in "app.rkt"
                    [#%app raqit-app])
         (rename-in "datum.rkt"
                    [#%datum raqit-datum])
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
         "use.rkt"
         "qi.rkt"
         syntax/parse/define
         (for-syntax racket/base))

(define-syntax-parser this-module-begin
  [(_ EXPR ...)
   #'(#%module-begin
      EXPR ...)])
