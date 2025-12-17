#lang racket/base

(provide macro)

(require syntax/parse/define
         (for-syntax racket/base)
         qi)

(define-syntax-parser macro
  ;; ---------------------------------------------------------
  ;; CASE 1: Qi Macros (Tagged with ☯)
  ;; Reader produces: (macro (#%flow HEAD) . BODY)
  ;; ---------------------------------------------------------

  ;; 1A. Single Clause Shorthand: ☯(name args ...)
  ;; The template is implicitly quoted, like Racket's
  ;; single-clause macro-defining forms
  [(_ ((~datum #%flow) (name:id args ...)) body ...)
   #'(define-qi-syntax-rule (name args ...)
       body ...)]

  ;; 1B. Full Form: ☯name
  ;; The templates require explicit #'.
  [(_ ((~datum #%flow) name:id) body ...)
   #'(define-qi-syntax-parser name
       body ...)]

  ;; ---------------------------------------------------------
  ;; CASE 2: Host-language Macros (Untagged)
  ;; Reader produces: (macro HEAD . BODY)
  ;; ---------------------------------------------------------

  ;; 2A. Single Clause Shorthand: (name args ...)
  ;; The template is implicitly quoted
  [(_ (name:id args ...) body ...)
   #'(define-syntax-parse-rule (name args ...)
       body ...)]

  ;; 2B. Full Form: name
  ;; The templates require explicit #'.
  [(_ name:id body ...)
   #'(define-syntax-parser name
       body ...)])
