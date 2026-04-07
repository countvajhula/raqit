#lang racket/base

(provide print)

(require (prefix-in b: racket/base))

(define (print datum
               #:out [out (current-output-port)]
               #:quote-depth [quote-depth 0]
               . args)
  (b:print (apply format datum args)
           out
           quote-depth))
