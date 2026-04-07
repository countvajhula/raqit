#lang racket/base

(provide void)

(require syntax/parse/define
         (prefix-in b: racket/base)
         (for-syntax racket/base))

(define-syntax-parser void
  ;; Used as a literal value (e.g., `void`, `(def x void)`)
  ;; Expands to the actual Racket void value.
  [_:id #'(b:void)]

  ;; Show a helpful error if the user tries to call it like standard Racket
  ;; (e.g., `(void 1 2)`)
  [(_ args ...)
   (raise-syntax-error 'void
                       "In Raqit, 'void' is a literal value, not a function. Just use 'void'."
                       this-syntax)])
