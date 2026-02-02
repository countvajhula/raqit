#lang racket/base

(provide (except-out (all-from-out racket/base)
                     #%module-begin
                     #%app
                     #%datum)
         (rename-out [this-module-begin #%module-begin])
         (rename-out [#%lang-app #%app])
         (rename-out [raqit-datum #%datum])
         (all-from-out "hash.rkt")
         (all-from-out "set.rkt")
         (all-from-out "list.rkt")
         (all-from-out "do.rkt")
         (all-from-out "def.rkt")
         (all-from-out "let.rkt")
         (all-from-out "loop.rkt")
         (all-from-out "fun.rkt")
         (all-from-out "class.rkt")
         (all-from-out "flow.rkt")
         (all-from-out "lambda.rkt")
         (all-from-out "macro.rkt")
         (all-from-out "equivalence.rkt")
         (all-from-out "order.rkt")
         (all-from-out "struct.rkt")
         (all-from-out "generic.rkt")
         (all-from-out "append.rkt")
         (all-from-out "use.rkt")
         (all-from-out "qi.rkt"))

(require "app.rkt"
         (rename-in "datum.rkt"
                    [#%datum raqit-datum])
         "hash.rkt"
         "set.rkt"
         "list.rkt"
         "do.rkt"
         "def.rkt"
         "let.rkt"
         "loop.rkt"
         "fun.rkt"
         "class.rkt"
         "flow.rkt"
         "lambda.rkt"
         "macro.rkt"
         "equivalence.rkt"
         "order.rkt"
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
