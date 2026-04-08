#lang racket/base

(provide the-any the-all
         any-val? all-val?)

(struct any-type ()
  #:property prop:custom-write
  (lambda (v port mode)
    (write-string "#any" port)))

(struct all-type ()
  #:property prop:custom-write
  (lambda (v port mode)
    (write-string "#all" port)))

;; Singleton instances that are provided
(define the-any (any-type))
(define the-all (all-type))

(define (any-val? x) (eq? x the-any))
(define (all-val? x) (eq? x the-all))
