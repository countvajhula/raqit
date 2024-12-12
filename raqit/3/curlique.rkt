#lang racket/base

(provide (rename-out [curlique-app #%app]
                     [curlique~> ~>]
                     [curlique~>> ~>>]
                     [curlique-< -<]
                     [curlique-switch switch])
         (except-out (all-from-out qi)
                     ~>
                     ~>>
                     flow
                     switch))

(require (only-in racket [#%app racket:app])
         syntax/parse/define
         (for-syntax syntax/parse/class/paren-shape
                     racket/base)
         qi)

(define-syntax curlique-app
  (syntax-parser
    [{~braces _ x ...}
     (syntax/loc this-syntax
       (flow x ...))]
    [(_ x ...)
     (syntax/loc this-syntax
       (racket:app x ...))]))

(define-syntax-parse-rule (define-curlique-syntax name:id qi-form:id)
  #:with ooo #'(... ...)
  (define-syntax name
    (syntax-parser
      [{~braces _ x ooo}
       (syntax/loc this-syntax
         (flow (qi-form x ooo)))]
      [(_ x ooo)
       (syntax/loc this-syntax
         (qi-form x ooo))])))

(define-syntax-parse-rule (define-curlique-syntaxes [name:id qi-form:id] ...)
  (begin (define-curlique-syntax name qi-form) ...))

(define-curlique-syntaxes
  [curlique~> ~>]
  [curlique~>> ~>>]
  [curlique-< -<]
  ;; TODO: what about this?
  [curlique-switch switch])
