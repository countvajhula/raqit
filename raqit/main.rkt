#lang racket/base

(provide (all-from-out "base.rkt")
         sqr)

(require "base.rkt"
         (only-in racket/math sqr))

(module reader syntax/module-reader
  raqit
  #:wrapper1 wrap-reader
  #:language-info '#[raqit/language-info get-language-info #f]
  (require "reader.rkt"))
