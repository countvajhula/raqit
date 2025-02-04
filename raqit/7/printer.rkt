#lang racket/base

(provide pr)

(require racket/match
         racket/list
         racket/set
         racket/format)

;; pr : Any ... [#:out Output-Port] -> Nil
;; analogous to write in racket
(define (pr #:out [out (current-output-port)] . args)
  (pr1-list/open-close args "" "" out pr1))

;; pr1 : Any #:out Output-Port -> Nil
(define (pr1 v #:out out)
  (cond
    [(list? v)
     (pr1-list/open-close v "(" ")" out pr1)]
    [(vector? v)
     (pr1-list/open-close (vector->list v) "[" "]" out pr1)]
    [(hash? v)
     (pr1-list/open-close (append* (hash-map v list)) "{" "}" out pr1)]
    [(set? v)
     (pr1-list/open-close (set->list v) "#{" "}" out pr1)]
    [(char? v)
     (write-string (substring (~s v) 1) out)]
    [(or (symbol? v) (number? v) (string? v))
     (write v out)]
    [else
     (write-string "#<rkt " out)
     (print v out)
     (write-string ">" out)]))

;; pr1-list/open-close :
;; (Listof Any) String String Output-Port (Any #:out Output-Port -> Nil) -> Nil
(define (pr1-list/open-close lst open close out rec-pr)
  (match lst
    ['()
     (write-string open out)
     (write-string close out)]
    [(cons fst rst)
     (write-string open out)
     (rec-pr fst #:out out)
     (for ([x (in-list rst)])
       (write-char #\space out)
       (rec-pr x #:out out))
     (write-string close out)]))
