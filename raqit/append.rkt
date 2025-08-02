#lang racket/base

(provide ~
         append
         identity
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
                 (if (eq? other ID)
                     appendable
                     (compose appendable other)))
               (define (identity appendable)
                 values)])
  #:fast-defaults ([string?
                    (define (append appendable other)
                      (if (eq? other ID)
                          appendable
                          (string-append appendable other)))
                    (define (identity appendable)
                      "")]
                   [bytes?
                    (define (append appendable other)
                      (if (eq? other ID)
                          appendable
                          (bytes-append appendable other)))
                    (define (identity appendable)
                      #"")]
                   [list?
                    (define (append appendable other)
                      (if (eq? other ID)
                          appendable
                          (b:append appendable other)))
                    (define (identity appendable)
                      (list))]
                   [vector?
                    (define (append appendable other)
                      (if (eq? other ID)
                          appendable
                          (vector-append appendable other)))
                    (define (identity appendable)
                      #())]
                   [set?
                    (define (append appendable other)
                      (if (eq? other ID)
                          appendable
                          (set-union appendable other)))
                    (define (identity appendable)
                      (set))]
                   [hash?
                    (define (append appendable other)
                      (if (eq? other ID)
                          appendable
                          (for*/hash ([h (in-list (list appendable other))]
                                      [(k v) (in-hash h)])
                            (values k v))))
                    (define (identity appendable)
                      (hash))]
                   [number?
                    (define (append appendable other)
                      (if (eq? other ID)
                          appendable
                          (+ appendable other)))
                    (define (identity appendable)
                      0)]
                   [stream?
                    (define (append appendable other)
                      (if (eq? other ID)
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

(define ID (composition-identity))

(define (~ . vs)
  (if (null? vs)
      ID
      (foldr append ID vs)))
