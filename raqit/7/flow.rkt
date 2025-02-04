#lang racket/base

(provide flow)

(require (only-in qi define-flow)
         "private/util.rkt")

(define-alias flow define-flow)
