#lang racket/base

(provide let
         :)

(require racket/match
         "private/util.rkt")

(define-match-expander :
  (syntax-rules () [(: v vs) (cons v vs)]))

(define-alias let match-define)
