#lang racket/base

(require (only-in racket/sequence sequence->list))

(provide
 (rename-out [this-read-syntax read-syntax]
             [this-read read]))

(define (this-read port)
  (this-read-syntax #f port))

(define (this-read-syntax path port)
  ;; this reads symexes from the source file
  (define src-stx-datums (sequence->list
                          (in-port (λ (v)
                                     (read-syntax path v))
                                   port)))
  (define module-datum `(module this-mod raqit/4/expander
                          ,@src-stx-datums))
  (datum->syntax #f module-datum))
