#lang racket/base

(provide struct)

(require syntax/parse/define
         (prefix-in b: racket/base)
         (for-syntax racket/base
                     racket/syntax))

(begin-for-syntax
  (define-syntax-class struct-option
    ;; We specify depth 1 because 'expanded' will always hold a LIST of syntax objects.
    ;; This allows us to use '... ...' (double ellipsis) in the main macro to splice them flat.
    #:attributes ([expanded 1])

    ;; Case 1: The 'implements' clause
    ;; Syntax: (implements protocol-id method ...)
    ;; Rewrites to: (#:methods gen:protocol-id [method ...])
    (pattern ((~datum implements) interface:id method ...)
             ;; 1. Construct the "gen:interface" identifier.
             ;; We use #'interface as the 'context', ensuring we look for the binding
             ;; in the same scope where the user wrote the name.
             #:with gen-id (format-id #'interface "gen:~a" #'interface)

             ;; 2. Expand using the computed gen-id
             #:with (expanded ...) #'(#:methods gen-id [method ...]))

    ;; Case 2: Standard struct options (pass-through)
    ;; Syntax: #:mutable, #:property, #:guard, etc.
    ;; Rewrites to: (the-option-itself)
    ;; We wrap 'other' in a list so it matches the list-shape of Case 1.
    (pattern other
             #:with (expanded ...) #'(other))))

(define-syntax-parser struct
  ;; 1. Struct without superclass
  ;; Matches: (struct point (x y) #:mutable (implements gen:custom ...))
  [(_ id:id (field ...) opt:struct-option ...)
   ;; Use 'opt.expanded ... ...' to flatten the list-of-lists into a single sequence of options
   #'(b:struct id (field ...) #:transparent opt.expanded ... ...)]

  ;; 2. Struct with superclass
  ;; Matches: (struct 3d-point point (z) ...)
  [(_ id:id super:id (field ...) opt:struct-option ...)
   #'(b:struct id super (field ...) #:transparent opt.expanded ... ...)])
