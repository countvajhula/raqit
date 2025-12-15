#lang racket/base

(require (only-in racket/sequence sequence->list)
         (only-in racket/set list->set)
         "reader-no-wrap.rkt"
         syntax/readerr)

(provide
 (rename-out [this-read-syntax read-syntax]
             [this-read read]
             [this-get-info get-info])
 current-syntax-introducer
 make-intro
 make-raqit-readtable)

(define (make-raqit-readtable)
  (make-readtable (current-readtable)
                  #\{ 'terminating-macro hash-proc
                  #\{ 'dispatch-macro set-proc
                  #\☯ 'terminating-macro flow-proc))

(define (flow-proc ch in src ln col pos)

  (define next-char (read-char in))

  (unless (memv next-char '(#\( #\[))
    (raise-read-error "expected '(' or '[' after '☯'" src ln col pos 1))

  (define lst-stx
    (parameterize ([read-accept-dot #f])
      (read-syntax/recursive src in next-char)))

  (datum->syntax lst-stx
    `(#%flow ,(syntax->list lst-stx))
    lst-stx
    lst-stx))

(define (read-nested-syntax-as-list src in ch)
  (parameterize ([read-accept-dot #f])
    (read-syntax/recursive src
                           in
                           ch
                           (make-readtable (current-readtable)
                                           ch
                                           #\{
                                           #f))))

(define (hash-proc ch in src ln col pos)
  (define lst-stx
    (read-nested-syntax-as-list src in ch))
  (datum->syntax lst-stx
    (cons '#%hash (syntax->list lst-stx))
    lst-stx
    lst-stx))

(define (set-proc ch in src ln col pos)
  (define lst-stx
    (read-nested-syntax-as-list src in ch))
  (datum->syntax lst-stx
    (cons '#%set (syntax->list lst-stx))
    lst-stx
    lst-stx))

(define current-syntax-introducer (make-parameter (λ (x) x)))

(define make-intro
  (λ () (make-syntax-introducer #t)))

(define (wrap-reader rd)
  (lambda args
    (define intro (make-intro))
    (parameterize ([current-readtable (make-raqit-readtable)]
                   [current-syntax-introducer intro])
      (define stx (apply rd args)) ; parametrized here
      (intro stx))))

(define this-read (wrap-reader read))
(define this-read-syntax (wrap-reader read-syntax))
(define this-get-info get-info)
