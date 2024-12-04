#lang racket/base

(provide #%app
         (all-from-out "curlique.rkt")
         (all-from-out "list.rkt"))

(require syntax/parse/define
         (for-syntax syntax/parse/class/paren-shape
                     racket/base)
         (only-in racket
                  [#%app racket:app])
         (rename-in "curlique.rkt"
                    [#%app curlique:app])
         (rename-in "list.rkt"
                    [#%app list:app]))

(define-syntax #%app
  (syntax-parser
    [(~brackets _ x ...)
     (syntax/loc this-syntax
       (list:app x ...))]
    [(~braces _ x ...)
     (syntax/loc this-syntax
       (curlique:app x ...))]
    [(_ x ...)
     (syntax/loc this-syntax
       (racket:app x ...))]))
