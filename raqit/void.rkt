#lang racket/base

(provide void)

(require syntax/parse/define
         (prefix-in b: racket/base)
         (for-syntax racket/base
                     syntax/parse/class/paren-shape)
         "app.rkt")

(define-syntax-parser void
  ;; Used as a literal value (e.g., `void`, `(def x void)`)
  ;; Expands to the actual Racket void value.
  [_:id #'(b:void)]

  ;; Show a helpful error if the user tries to call it like standard Racket
  ;; (e.g., `(void 1 2)`)
  [(~parens _ args ...)
   (raise-syntax-error 'void
                       "In Raqit, 'void' is a literal value, not a function. Just use 'void'."
                       this-syntax)]

  ;; Delegate to #%app in case void is being used as a value
  ;; in a list like [void 1 2]
  [(_ args ...)
   (define shape (syntax-property this-syntax 'paren-shape))
   (syntax-property (syntax/loc this-syntax
                      (#%lang-app void args ...))
                    'paren-shape
                    shape)])
