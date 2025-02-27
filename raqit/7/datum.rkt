#lang racket/base

(provide #%datum)

(require (only-in racket/base
                  [#%datum racket:datum])
         syntax/parse/define
         (for-syntax racket/base)
         (for-syntax "strip-context.rkt")
         racket/set)

(define-syntax-parser #%datum
  [(-#%datum . #[e ...])
   (syntax/loc this-syntax (vector-immutable e ...))]
  [(#%datum . hsh)
   #:when (syntax-property #'hsh 'raqit-hash-map)
   #:with (e ...) (replace-context #'hsh (syntax-property #'hsh 'raqit-hash-map))
   (syntax/loc this-syntax (hash e ...))]
  [(#%datum . st)
   #:when (syntax-property #'st 'raqit-set)
   #:with (e:expr ...) (replace-context #'st (syntax-property #'st 'raqit-set))
   (syntax/loc this-syntax (set e ...))]
  [(#%datum . e)
   (syntax/loc this-syntax (racket:datum . e))])
