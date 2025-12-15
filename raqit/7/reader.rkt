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
  (unless (char=? (read-char in) #\{)
    (raise-read-error "expected '{' after '☯'" src ln col pos 1))

  (define lst-stx
    (parameterize ([read-accept-dot #f])
      (read-syntax/recursive src in #\{
                             (make-readtable (current-readtable)
                                             #\{ #\{ #f))))

  (datum->syntax lst-stx
    `(#%flow ,(syntax->list lst-stx))
    lst-stx
    lst-stx))

(define (hash-proc ch in src ln col pos)
  (define lst-stx
    (parameterize ([read-accept-dot #f])
      (read-syntax/recursive src in ch (make-readtable (current-readtable) ch #\{ #f))))
  (define lst (syntax->list lst-stx))
  (unless (even? (length lst))
    (raise-read-error "hash map literal must contain an even number of forms"
                      src ln col pos (syntax-span lst-stx)))
  (datum->syntax lst-stx (for/hash ([(k v) (in-hash (apply hash lst))]) ; need syntax property to
                           (values (syntax->datum k) v))                ; preserve order of evaluation
                 lst-stx                                                ; and source locations of keys
                 (syntax-property lst-stx 'raqit-hash-map lst-stx)))

(define (set-proc ch in src ln col pos)
  (define lst-stx
    (parameterize ([read-accept-dot #f])
      (read-syntax/recursive src in ch (make-readtable (current-readtable) ch #\{ #f))))
  (datum->syntax lst-stx (list->set (syntax->datum lst-stx))
                 lst-stx
                 (syntax-property lst-stx 'raqit-set lst-stx)))

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
