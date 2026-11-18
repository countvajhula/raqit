#lang racket/base

(provide (all-from-out "expander.rkt"))

(require "expander.rkt")

(module reader syntax/module-reader
  raqit/base
  #:wrapper1 wrap-reader
  #:language-info '#[raqit/language-info get-language-info #f]
  (require "reader.rkt"))
