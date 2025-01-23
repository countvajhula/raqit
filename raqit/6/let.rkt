#lang racket/base

(provide let)

(require racket/match
         syntax/parse/define
         syntax/parse
         (for-syntax racket/base
                     syntax/parse/class/paren-shape)
         "private/util.rkt")

(define-alias let match-let)
