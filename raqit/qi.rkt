#lang racket/base

(provide (all-from-out qi/list)
         (rename-out [~map map]
                     [~filter filter]
                     [~filter-map filter-map]
                     [~foldl foldl]
                     [~foldr foldr]
                     [~range range]
                     [~take take]
                     [~car car]
                     [~cadr cadr]
                     [~caddr caddr]
                     [~cadddr cadddr]
                     [~list-ref list-ref]
                     [~length length]
                     [~empty? empty?]
                     [~null? null?])
         flow-macro)

(require qi/list
         qi
         syntax/parse/define
         (for-syntax racket/base))

(define-syntax-parse-rule (~map f v)
  (on (v) (map f)))

(define-syntax-parse-rule (~filter f v)
  (on (v) (filter f)))

(define-syntax-parse-rule (~filter-map f v)
  (on (v) (filter-map f)))

(define-syntax-parse-rule (~foldl f init v)
  (on (v) (foldl f init)))

(define-syntax-parse-rule (~foldr f init v)
  (on (v) (foldr f init)))

(define-syntax-parse-rule (~range e ...)
  (on () (range e ...)))

(define-syntax-parse-rule (~take n v)
  (on (v) (take n)))

(define-syntax-parse-rule (~car v)
  (on (v) car))

(define-syntax-parse-rule (~cadr v)
  (on (v) cadr))

(define-syntax-parse-rule (~caddr v)
  (on (v) caddr))

(define-syntax-parse-rule (~cadddr v)
  (on (v) cadddr))

(define-syntax-parse-rule (~list-ref n)
  (on (v) (list-ref n)))

(define-syntax-parse-rule (~length v)
  (on (v) length))

(define-syntax-parse-rule (~empty? v)
  (on (v) empty?))

(define-syntax-parse-rule (~null? v)
  (on (v) null?))

(define-syntax-parser flow-macro
  [(_ (name pattern ...) template)
   #'(define-qi-syntax-parser name
       [(_ pattern ...) template])]
  [(_ name:id parse-option ... [pattern template] ...)
   #'(define-qi-syntax-parser name
       parse-option ...
       [pattern template]
       ...)])
