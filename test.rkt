#lang racket

(require "expander.rkt")

(def (do-something v)
  (let (v (abs v))
    v))

(flow check-number
      (switch
        (< 0) 'negative
        (> 0) 'positive
        else 'zero))

(map check-number [1 -2 0])

(def v -3)
(map {~> sqr add1} [1 2 (abs v) 4 5])
