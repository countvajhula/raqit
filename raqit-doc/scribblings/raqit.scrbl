#lang scribble/doc
@require[scribble/manual
         scribble-abbrevs/manual
         scribble/example
         racket/sandbox
         @for-label[raqit]]

@(define eval-for-docs
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit #f])
    (make-evaluator 'racket/base)))

@title{Raqit}

@defmodule[raqit]

Language experiments with Racket and Qi.

@section{Basic Syntax}

@subsection{Sequencing}

@defform[(do e ...)]{
  Identical to Racket's @racket[begin].

  @codeblock{
    (do
      (def x 10)
      x)
   }
}

@defform[(~> (arg ...) e ...)]{
  Identical to Qi's @racket[~>].

  @codeblock{
    (~> (3) sqr add1)
   }
}

@subsection{Application Syntax}

@racket[(...)] is an ordinary function application (and ordinary Lisp syntax, outside of application contexts).

@racket[[...]] is an unquoted list.

@racket[{...}] is a Qi flow.

@section{Relations}

@defproc[(= [#:key key (-> any/c any/c) #f] [v any/c] ...)
         boolean?]{
  Identical to Racket's @racket[equal?] except that it supports an optional @racket[key] function that is applied to the arguments prior to comparison, if provided.

@codeblock{
  (= 3 4)
  (= #:key string-upcase "abc" "ABC")
 }
}

@section{Generics}

@defform[(protocol stx ...)]{
  Alias for Racket's @racket[define-generics].

  @codeblock{
    (protocol stack
      (push stack element)
      (pop stack)
      (top stack))
   }
}

@section{Generic Operators}

@defproc[(append [a appendable?]
                 [b appendable?])
         appendable?]{

 A function taking two arguments that composes them using the implemention of @racket[gen:appendable] for the type of the arguments being composed.
}

@defproc[(~ [v appendable?]
            ...)
         appendable?]{
 Append the provided values together, using the canonical append operation on the data based on its type. The special value @racket[ID] serves as the generic identity value when the type of the operands is not known. In particular, this value is the result when no operands are provided.

@codeblock{
  (~ [1 2 3] [4 5 6] [7 8 9])
  (~ "abc" "def" "ghijk")
 }
}

@section{Lists}

@defproc[(:: [v any/c] ... [vs list?])
         list?]{
  Construct a list. Alias for Racket's @racket[list*].

  @codeblock{
    (:: 1 [2 3])
    (:: 1 2 3 [4 5 6])
  }
}

@defproc[(map [f (-> any/c any/c)] [vs list?])
         list?]{
  This is an embedding of @racket[map] from @racket[qi/list].

  @codeblock{
    (map (~> sqr add1) [1 2 3 4 5])
    (map (switch [positive? add1] [else sub1]) [1 -2 3 -4 5])
  }
}

Likewise, all other list operations in @racket[qi/list] are available in embedded form.

@section{User-defined Datatypes}

@defform[(struct stx ...)]{
  Identical to Racket's @racket[struct], except that it is @racket[#:transparent] by default.
}

@section{Modules}

@defform[(use stx ...)]{
  Alias for Racket's @racket[require].
}

@section{Loops}

@defform[(loop name bindings body ...)]{
  A basic looping form, analogous to Racket's named let, that uses pattern-matching bindings.

  @codeblock{
    (loop go (a 5)
      (when (> a 0)
        (displayln a)
        (go (sub1 a))))

    (loop go ([[: x xs] [1 2 3 4 5]]
              [a 5])
      (displayln x)
      (unless (null? xs)
        (go xs a)))
   }
}

@section{Classes and Objects}

@defform[(class stx ...)]{
  Identical to Racket's @racket[class], except that it supports the following syntax for methods:

  @codeblock{
    (method modifier name args
      body)
    (method name args
      body)
   }

  … where @racket[modifier] is one of the recognized method modifiers like @racket[public], @racket[augride], and so on.
}

@section{Definitions}

@deftogether[(
  @defform[(def id expr)]
  @defform[#:link-target? #f (def (id ...) expr)]
  )]{
  Identical to Racket's @racket[match-define] and @racket[match-define-values].
  @codeblock{
    (def a 3)
    (def (p q) (values 1 2))
    (def [: v vs] [1 2 3])
   }
}

@defform[(let stx ...)]{
  Identical to Racket's @racket[match-let].

  @codeblock{
    (let ([[: x xs] [1 2 3]])
      x)
    (let ([a 3]
          [b 5])
      (displayln (+ a b)))
   }
}

@deftogether[(
  @defform[(fun name (arg ...) body ...)]
  @defform[#:link-target? #f (fun name args body ...)]
  )]{
  Identical to Racket's @racket[define], but with different syntax.

  @codeblock{
    (fun do-something (v)
      (abs v))
    (fun do-something args
      (length args))
   }
}

@defform[(flow name body)]{
  Identical to Qi's @racket[define-flow].

  @codeblock{
    (flow check-number
      (switch
        [(< 0) 'negative]
        [(> 0) 'positive]
        [else 'zero]))

    (map check-number [1 -2 0])
   }
}

@defform[(: v ... vs)]{
  A match expander for @racket[::]. TODO: ideally we'd want both to have the same name, but they currently collide if both are named @racket[:].

  @codeblock{
    (def [: x xs] [1 2 3])
   }
}

@section{Macros}

@deftogether[(
  @defform[(macro (name pattern ...) template)]
  @defform[#:link-target? #f (macro name parse-option ... [pattern template] ...)]
  )]{
  Identical to either Racket's @racket[define-syntax-parse-rule] or @racket[define-syntax-parser], except that it expects an explicit syntax template in both cases.

  @codeblock{
    (macro (where expr bindings)
      #'(def bindings expr))
    (macro hello
      [(_ name) #'(displayln (format "hello ~a" name))]
      [(_) #'(displayln (format "hello ~a" "world"))])
   }
}


@deftogether[(
  @defform[(flow-macro (name pattern ...) template)]
  @defform[#:link-target? #f (flow-macro name parse-option ... [pattern template] ...)]
  )]{
  Identical to Qi's @racket[define-qi-syntax-rule] or @racket[define-qi-syntax-parser], except that it expects an explicit syntax template in both cases.

  @codeblock{
    (flow-macro (rev f g)
      #'(~> g f))

    (map (rev add1 sqr) [1 2 3])
   }
}
