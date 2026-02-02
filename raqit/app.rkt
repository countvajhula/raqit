#lang racket/base

(provide #%lang-app)

(require syntax/parse/define
         (only-in racket/list range)
         (for-syntax syntax/parse/class/paren-shape
                     racket/base))

;; delegates to Racket's #%app implicitly
(define-syntax #%lang-app
  (syntax-parser
    [(~brackets _ low next (~datum ..) high)
     (syntax/loc this-syntax
       (range low (add1 high) (- next low)))]
    [(~brackets _ low (~datum ..) high)
     (syntax/loc this-syntax
       (range low (add1 high)))]
    [(~brackets _ x ...)
     (syntax/loc this-syntax
       (list x ...))]
    [(_ x ...)
     (syntax/loc this-syntax
       (x ...))]))
