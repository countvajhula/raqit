#lang racket/base

(provide raqit-syntax)

(require syntax/parse/define
         (for-syntax racket/base))

(define-syntax-parser raqit-syntax
  [(_ (name pattern ...) template)
   #'(define-syntax-parser name
       [(_ pattern ...) template])])
