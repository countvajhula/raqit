#lang racket/base

(provide lambda λ)

(require syntax/parse/define
         (for-syntax racket/base)
         (prefix-in b: racket/base)
         (only-in "private/util.rkt" define-alias))

(begin-for-syntax
  ;; 1. Define what a single "Standard Argument" looks like.
  ;;    It can be an ID, a Keyword, or a Default Pair [id val].
  (define-syntax-class required-argument
    #:description "identifier or keyword"
    (pattern _:id)
    (pattern _:keyword))

  (define-syntax-class optional-argument
    #:description "keyword or default pair"
    (pattern (_:id _:expr))
    (pattern _:keyword))

  ;; 2. Define the argument LIST composed of those items.
  (define-syntax-class standard-formals
    #:attributes () ;; Suppress attribute exports to avoid consistency errors
    ;; Case A: Proper list (e.g. (x y [z 1]))
    (pattern (req-arg:required-argument ... opt-arg:optional-argument ...))
    ;; Case B: Improper list (e.g. (x . rest))
    (pattern (req-arg:required-argument ... opt-arg:optional-argument ... . rest:id))
    ;; Case C: Single Rest ID (e.g. args)
    (pattern rest:id))

  ;; 3. Define what a Case Lambda clause looks like.
  (define-syntax-class case-clause
    #:description "case-lambda clause"
    (pattern (args body ...+))))

(define-syntax-parser lambda
  ;; PRIORITY 1: Standard Lambda
  ;; Matches if the first argument looks like a valid Standard Argument List
  ;; (i.e. strictly IDs, keywords, or default pairs).
  ;; This prevents ambiguity with case-lambda clauses like [(x) y] because
  ;; '(x)' is neither an ID, Keyword, nor Default Pair (length 1 vs 2).
  [(_ args:standard-formals body ...+)
   #'(b:lambda args body ...)]

  ;; PRIORITY 2: Case Lambda
  ;; Matches if P1 failed. This handles multi-clause definitions.
  [(_ clause:case-clause ...+)
   #'(case-lambda clause ...)])

(define-alias λ lambda)
