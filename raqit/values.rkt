#lang racket/base

(provide values*)

(require syntax/parse/define
         (for-syntax racket/base
                     syntax/parse))

(define-syntax-parse-rule (values* v ... vs)
  (apply values v ... vs))
