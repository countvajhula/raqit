#lang racket/base

(provide fun)

(require syntax/parse/define
         (for-syntax racket/base)
         "lambda.rkt")

(define-syntax-parser fun
  ;; This handles both standard and case-lambda styles
  ;; by delegating to lambda.

  ;; Standard: (fun my-sqr (x) (* x x))
  ;; -> (define my-sqr (lambda (x) (* x x)))

  ;; Multi:    (fun my-add [(x) x] [(x y) (+ x y)])
  ;; -> (define my-add (lambda [(x) x] [(x y) (+ x y)])) -> case-lambda
  [(_ name:id args-or-clauses ...)
   #'(define name (lambda args-or-clauses ...))])
