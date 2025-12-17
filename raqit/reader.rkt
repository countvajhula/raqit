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

;; Flow reader
;; Reads the immediate next datum and wraps it in #%flow.
;; e.g. ☯id    -> (#%flow id)
;;      ☯(a b) -> (#%flow (a b))
(define (flow-proc ch in src ln col pos)
  (define next-stx (read-syntax src in))
  (when (eof-object? next-stx)
    (raise-read-error "expected value after '☯'" src ln col pos 1))

  (datum->syntax next-stx
    `(#%flow ,next-stx)
    next-stx
    next-stx))

(define (read-nested-syntax-as-list src in ch)
  (parameterize ([read-accept-dot #f])
    (read-syntax/recursive src
                           in
                           ch
                           ;; Revert 'ch' back to its standard behavior for this recursive read.
                           ;; E.g., while parsing a hash literal {k v ...}, this indicates that
                           ;; { should be handled as { is ordinarily handled, viz., as an
                           ;; opening list delimiter. Thus, we read this syntax as a list and
                           ;; then wrap it as a hash in the outer reading context.
                           (make-readtable (current-readtable)
                                           ch ch #f))))

;; Hash reader
;; Reads { ... } as (#%hash ...)
(define (hash-proc ch in src ln col pos)
  (define lst-stx
    (read-nested-syntax-as-list src in ch))
  (datum->syntax lst-stx
    (cons '#%hash (syntax->list lst-stx))
    lst-stx
    lst-stx))

;; Set reader
;; Reads #{ ... } as (#%set ...)
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
