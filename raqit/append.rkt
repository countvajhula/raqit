#lang racket/base

(provide ~
         append
         identity
         null?
         null
         gen:appendable)

(require "generic.rkt"
         "struct.rkt"
         racket/set
         racket/stream
         racket/vector
         (prefix-in b: racket/base))

(protocol appendable
  (append appendable other)
  (identity appendable)
  #:defaults ([procedure?
               (define (append appendable other)
                 (if (eq? other null)
                     appendable
                     (compose appendable other)))
               (define (identity appendable)
                 values)])
  #:fast-defaults ([string?
                    (define (append appendable other)
                      (if (eq? other null)
                          appendable
                          (string-append appendable other)))
                    (define (identity appendable)
                      "")]
                   [bytes?
                    (define (append appendable other)
                      (if (eq? other null)
                          appendable
                          (bytes-append appendable other)))
                    (define (identity appendable)
                      #"")]
                   [list?
                    (define (append appendable other)
                      (if (eq? other null)
                          appendable
                          (b:append appendable other)))
                    (define (identity appendable)
                      (list))]
                   [vector?
                    (define (append appendable other)
                      (if (eq? other null)
                          appendable
                          (vector-append appendable other)))
                    (define (identity appendable)
                      #())]
                   [set?
                    (define (append appendable other)
                      (if (eq? other null)
                          appendable
                          (set-union appendable other)))
                    (define (identity appendable)
                      (set))]
                   [hash?
                    (define (append appendable other)
                      (if (eq? other null)
                          appendable
                          (for*/hash ([h (in-list (list appendable other))]
                                      [(k v) (in-hash h)])
                            (values k v))))
                    (define (identity appendable)
                      (hash))]
                   [number?
                    (define (append appendable other)
                      (if (eq? other null)
                          appendable
                          (+ appendable other)))
                    (define (identity appendable)
                      0)]
                   [stream?
                    (define (append appendable other)
                      (if (eq? other null)
                          appendable
                          (stream-append appendable other)))
                    (define (identity appendable)
                      empty-stream)]))

(struct composition-identity ()
  #:methods gen:appendable
  [(define (append appendable other)
     other)
   (define (identity appendable)
     appendable)])

(define null (composition-identity))

(define (null? v)
  (equal? v (identity v)))

(define (~ . vs)
  (if (b:null? vs)
      null
      (foldr append null vs)))
