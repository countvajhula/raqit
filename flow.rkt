#lang racket/base

(provide flow)

(require (rename-in "qi.rkt" [flow qi:flow])
         "private/util.rkt")

(define-alias flow define-flow)
