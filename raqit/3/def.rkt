#lang racket/base

(provide def)

(require syntax/parse/define
         (for-syntax racket/base))

;; TODO: behaves differently
;; than define; doesn't allow
;; redefining values in REPL
;; "cannot re-define a constant"
(define-syntax-parser def
  [(_ (var ...) vals)
   (datum->syntax this-syntax
     #'(define-values (var ...) vals))]
  [(_ var val)
   (datum->syntax this-syntax
     #'(define var val))])
