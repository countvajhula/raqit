#lang racket/base

(provide pr)

(require racket/match
         racket/list
         racket/set
         racket/format)

;; pr : Any ... [#:out Output-Port] -> Nil
;; analogous to write in racket
(define (pr #:out [out (current-output-port)] . args)
  (pr1-list/open-close args "" "" out pr1)
  (newline))

;; pr1 : Any #:out Output-Port -> Nil
(define (pr1 v #:out out)
  (cond
    [(list? v)
     (pr1-list/open-close v "[" "]" out pr1)]
    [(vector? v)
     (pr1-list/open-close (vector->list v) "#[" "]" out pr1)]
    [(hash? v)
     (pr1-list/open-close (append* (hash-map v list)) "{" "}" out pr1)]
    [(set? v)
     (pr1-list/open-close (set->list v) "#{" "}" out pr1)]
    [(char? v)
     (write-string (substring (~s v) 1) out)
     (void)]
    [(or (symbol? v) (number? v) (string? v))
     (write v out)
     (void)]
    [(boolean? v)
     (write-string (if v "#true" "#false") out)
     (void)]
    [else
     (print v out)
     (void)]))

;; pr1-list/open-close :
;; (Listof Any) String String Output-Port (Any #:out Output-Port -> Nil) -> Nil
(define (pr1-list/open-close lst open close out rec-pr)
  (match lst
    ['()
     (write-string open out)
     (write-string close out)
     (void)]
    [(cons fst rst)
     (write-string open out)
     (rec-pr fst #:out out)
     (for ([x (in-list rst)])
       (write-char #\space out)
       (rec-pr x #:out out))
     (write-string close out)
     (void)]))
