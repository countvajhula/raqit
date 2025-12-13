#lang racket/base

(provide #%datum)

(require (only-in racket/base
                  [#%datum racket:datum])
         syntax/parse/define
         (for-syntax racket/base)
         (for-syntax "strip-context.rkt")
         racket/set)

(define-syntax-parser #%datum
  [(_ . #[e ...])
   (syntax/loc this-syntax (vector-immutable e ...))]
  [(_ . hsh)
   #:when (syntax-property #'hsh 'raqit-hash-map)
   #:with (e ...) (replace-context #'hsh (syntax-property #'hsh 'raqit-hash-map))
   (syntax/loc this-syntax (hash e ...))]
  [(_ . st)
   #:when (syntax-property #'st 'raqit-set)
   #:with (e:expr ...) (replace-context #'st (syntax-property #'st 'raqit-set))
   (syntax/loc this-syntax (set e ...))]
  [(_ . e)
   (syntax/loc this-syntax (racket:datum . e))])
