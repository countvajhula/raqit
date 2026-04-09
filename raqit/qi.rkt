#lang racket/base

(provide (except-out (all-from-out qi) flow)
         (rename-out [raqit-flow #%flow])
         (all-from-out qi/list)
         (for-space qi
                    lambda
                    λ
                    void
                    the-any
                    the-all))

(require qi/list
         qi
         (prefix-in b: racket/base)
         (prefix-in l: "void.rkt")
         (prefix-in l: "logic.rkt")
         (prefix-in l: "lambda.rkt")
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse))


(define-qi-syntax-rule (lambda e ...)
  (esc (lambda e ...)))

(define-qi-syntax-rule (λ e ...)
  (lambda e ...))

(define-qi-syntax-parser void
  [_:id #'(gen l:void)])

(define-qi-syntax-parser the-any
  [_:id #'(gen l:the-any)])

(define-qi-syntax-parser the-all
  [_:id #'(gen l:the-all)])

(define-syntax-parser raqit-flow
  ;; Intercept the void literal, i.e., ☯void
  [(_ (~literal void)) #'(flow b:void)]

  [(_ expr)
   (define shape (syntax-property #'expr 'paren-shape))

   ;; If the paren shape is explicitly square brackets
   ;; or curly braces, wrap the expression in Qi's 'gen'
   ;; form before passing it on, so that it gets
   ;; handled by Raqit's #%app macro (e.g., [...] is a list)
   (if (memv shape '(#\[ #\{))
       #'(flow (gen expr))
       ;; Otherwise (round parens or no shape, like an identifier),
       ;; pass it through to Qi untouched.
       #'(flow expr))])
