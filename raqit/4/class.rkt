#lang racket/base

(provide (rename-out [class2 class])
         method
         (except-out (all-from-out racket/class)
                     class))

(require syntax/parse/define
         (for-syntax racket/base
                     racket/class)
         racket/class
         racket/stxparam
         syntax/parse
         "fun.rkt")

(define-syntax-parameter method
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "can only be used inside `class`")))

(begin-for-syntax
  (define-syntax-class method-type
    #:literals (public
                override
                augment
                pubment
                overment
                augride
                public-final
                override-final
                augment-final
                private)
   (pattern
       (~or* public
             override
             augment
             pubment
             overment
             augride
             public-final
             override-final
             augment-final
             private))))

;; TODO: how are `field`s different from ordinary
;; bindings within the class body?
(define-syntax-parser class2
  [(_ class-name (parent) body ...)
   #:with ooo (quote-syntax ...)
   #'(define class-name
       (syntax-parameterize
           ([method (syntax-parser
                      [(_ annotation:method-type method-name (arg ooo) method-body ooo)
                       #'(begin
                           (annotation method-name)
                           (fun method-name (arg ooo) method-body ooo))]
                      [(_ method-name arg ooo)
                       #'(begin
                           (public method-name)
                           (fun method-name arg ooo))])])
         (class parent body ...)))])
