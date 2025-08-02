#lang racket/base

(provide protocol)

(require racket/generic
         "private/util.rkt")

(define-alias protocol define-generics)
