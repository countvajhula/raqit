#lang racket/base

(provide lambda λ)

(require syntax/parse/define
         (for-syntax racket/base
                     syntax/parse/class/paren-shape)
         (prefix-in b: racket/base)
         (only-in "private/util.rkt" define-alias))

(begin-for-syntax
  (define-syntax-class required-argument
    #:description "identifier or keyword"
    (pattern _:id)
    (pattern _:keyword))

  (define-syntax-class optional-argument
    #:description "keyword or default pair"
    (pattern (_:id _:expr))
    (pattern _:keyword))

  (define-syntax-class standard-formals
    #:attributes ()
    (pattern (req:required-argument ... opt:optional-argument ...))
    (pattern (req:required-argument ... opt:optional-argument ... . rest:id))
    (pattern rest:id))

  (define-syntax-class case-clause
    #:description "bracketed case-lambda clause"
    #:attributes (args (body 1))
    (pattern (~brackets args:standard-formals body ...+))))

(define-syntax-parser lambda
  ;; PRIORITY 1: Case Lambda
  [(_ clause:case-clause ...+)
   #'(case-lambda (clause.args clause.body ...) ...)]

  ;; PRIORITY 2: Standard Lambda
  [(_ args:standard-formals body ...+)
   #'(b:lambda args body ...)])

(define-alias λ lambda)
