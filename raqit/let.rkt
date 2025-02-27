#lang racket/base

(provide let
         let*)

(require syntax/parse/define
         (for-syntax racket/base)
         (prefix-in b: racket/base))

(define-syntax-parser let
  [(_ ((~seq v e) ...)
      body ...)
   #'(b:let ([v e] ...)
            body ...)])

(define-syntax-parser let*
  [(_ ((~seq v e) ...)
      body ...)
   #'(b:let* ([v e] ...)
             body ...)])
