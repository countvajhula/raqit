#lang racket/base

(provide let
         let*)

(require racket/match
         syntax/parse/define
         (for-syntax racket/base))

(begin-for-syntax
  ;; A syntax class to parse each individual binding pair
  (define-syntax-class let-clause
    #:attributes (normalized-clause)

    ;; Case 1: Multi-value binding
    ;; Matches: [(values pat1 pat2 ...) expr]
    ;; We use (~datum values) so it matches the word "values" exactly,
    ;; regardless of bindings.
    (pattern (((~datum values) pat ...) expr)
             #:with normalized-clause #'[(pat ...) expr])

    ;; Case 2: Single-value binding (Fallback)
    ;; Matches: [pat expr]
    ;; We wrap the single pattern in parens to satisfy match-let-values.
    (pattern (pat expr)
             #:with normalized-clause #'[(pat) expr])))

(define-syntax-parser let
  [(_ name:id arg ...)
   (raise-syntax-error #f "Named 'let' is not supported; use 'loop' for recursion." #'name)]

  [(_ (clause:let-clause ...) body ...+)
   ;; Compile everything down to match-let-values using our normalized clauses
   #'(match-let-values (clause.normalized-clause ...)
       body ...)])

(define-syntax-parser let*
  [(_ (clause:let-clause ...) body ...+)
   #'(match-let*-values (clause.normalized-clause ...)
       body ...)])
