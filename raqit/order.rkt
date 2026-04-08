#lang racket/base

(provide gen:orderable
         orderable/c
         orderable?
         lt?
         lte?
         gt?
         gte?
         <
         ≤
         <=
         ≥
         >=
         >
         min
         max)

(require racket/generic
         (only-in racket/function disjoin
                  curryr
                  curry)
         racket/set
         (prefix-in b: racket/base)
         (prefix-in r: "equivalence.rkt")
         "logic.rkt")

(define || disjoin)

(define-generics orderable
  (lt? orderable other)
  (lte? orderable other)
  (gte? orderable other)
  (gt? orderable other)
  #:fallbacks [(define/generic generic-lt lt?)
               (define/generic generic-lte lte?)
               (define lte? (|| generic-lt
                                r:=))
               (define (gte? a b)
                 (generic-lte b a))
               (define (gt? a b)
                 (generic-lt b a))]
  #:fast-defaults ([number?
                    (define lt? b:<)
                    (define lte? b:<=)
                    (define gte? b:>=)
                    (define gt? b:>)]
                   [string?
                    (define lt? string<?)
                    (define lte? string<=?)
                    (define gte? string>=?)
                    (define gt? string>?)]
                   [bytes?
                    (define lt? bytes<?)
                    (define lte?
                      (|| bytes=?
                          bytes<?))
                    (define gte?
                      (|| bytes=?
                          bytes>?))
                    (define gt? bytes>?)]
                   [char?
                    (define lt? char<?)
                    (define lte? char<=?)
                    (define gte? char>=?)
                    (define gt? char>?)]
                   [set?
                    (define lt? proper-subset?)
                    (define lte? subset?)
                    (define (gte? orderable other)
                      (subset? other orderable))
                    (define (gt? orderable other)
                      (proper-subset? other orderable))]))

(define (check-pairwise check? vals)
  (or (null? vals)
      (let ([v (car vals)]
            [vs (cdr vals)])
        (or (null? vs)
            (and (check? v (car vs))
                 (check-pairwise check? vs))))))

(define (< #:key [key #f] . args)
  (if key
      (apply < (map key args))
      (check-pairwise (λ (a b)
                        (cond [(eq? a the-any) (if (eq? b the-any) #false #true)]
                              [(eq? a the-all) #false]
                              [(eq? b the-any) #false]
                              [(eq? b the-all) #true]
                              [else (lt? a b)]))
                      args)))

(define (<= #:key [key #f] . args)
  (if key
      (apply <= (map key args))
      (check-pairwise (λ (a b)
                        (cond [(eq? a the-any) #true]
                              [(eq? b the-any) #false]
                              [(eq? b the-all) #true]
                              [(eq? a the-all) #false]
                              [else (lte? a b)])) args)))

(define (>= #:key [key #f] . args)
  (not (apply < #:key key args)))

(define (> #:key [key #f] . args)
  (not (apply <= #:key key args)))

(define (min #:key [key #f] . args)
  (car (sort args (curry < #:key key))))

(define (max #:key [key #f] . args)
  (car (sort args (curry > #:key key))))

(define ≤ <=)
(define ≥ >=)
