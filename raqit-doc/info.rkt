#lang info

(define version "0.0")
(define collection "raqit")
(define deps '("base"))
(define build-deps '("scribble-lib"
                     "scribble-abbrevs"
                     "scribble-math"
                     "racket-doc"
                     "qi-doc"
                     "sandbox-lib"
                     "raqit"))
(define scribblings '(("scribblings/raqit.scrbl" ())))
(define clean '("compiled" "doc" "doc/raqit"))
