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

A general purpose Scheme-like language designed for clarity and economy of expression, with the goal of weaving together intuitive, expressive DSLs for major language facilities, with an emphasis on Racket and @seclink["top" #:indirect? #t #:doc '(lib "qi/scribblings/qi.scrbl")]{Qi} and functional programming.

@bold{Note}: Raqit is a placeholder name for a language being developed as a @hyperlink["https://github.com/countvajhula/raqit"]{series of community experiments} on DSLs, to be synthesized into a cohesive language that @hyperlink["https://countvajhula.com/2024/12/09/langs-that-fit-in-your-head/"]{"fits in your head."} Join in on the fun and do your own experiments! The @hyperlink["https://github.com/countvajhula/raqit/blob/main/README.rst"]{project README} describes how.

These docs describe the latest incarnation of the language, representing a (one) synthesis of the best of these experiments. What do you think of it? What would you change?

@section{Basic Syntax}

@subsection{Delimiters}
@racket[(...)] and @racket[[...]] are generally interchangeable in the core syntax (like in Racket). They have different meanings when used in expressions, as described below.

@subsection{Expressions}

@racket[(...)] is ordinary function application.

@racket[[...]] is an unquoted list.

@subsection{Data Literals}

All of these literals are @emph{unquoted}, meaning that the arguments are evaluated prior to construction of the datatype.

@racket[[...]] is a list.

@racket[#(...)] is an immutable vector (the delimiters here could also be @racket[#[]]).

@racket[#{...}] is a set.

@racket[{...}] is a hash.

@racket[☯(...)] is a Qi flow.

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

@defproc[(: [v any/c] ... [vs list?])
         list?]{
  When used as an expression, construct a list; when used as a pattern, match a list. Alias for Racket's @racket[list*] both as an expression and as a pattern.

  @codeblock{
    (: 1 [2 3])
    (: 1 2 3 [4 5 6])
    (def [: x xs] [1 2 3])
  }
}

@defproc[(map [f (-> any/c any/c)] [vs list?])
         list?]{
  An alias for Racket's @racket[map].

  @codeblock{
    (map ☯(~> sqr add1) [1 2 3 4 5])
    (map ☯(switch [positive? add1] [else sub1]) [1 -2 3 -4 5])
  }
}

Likewise, many other standard list operations in @racket[racket/list] are available. However, for any nontrivial list processing, it's advisable to use Qi flows, as they use the operations from @racket[qi/list] which are more efficient (and more clear).

@codeblock{
  (~> ([1 2 3 4 5]) (filter odd?) (map sqr) (foldl +))
}

@section{User-defined Datatypes}

@defform[(struct stx ...)]{
  Identical to Racket's @racket[struct], except that it is @racket[#:transparent] by default.
}

@section{Modules}

@defform[(use stx ...)]{
  Alias for Racket's @racket[require].
}

@defform[(offer stx ...)]{
  Alias for Racket's @racket[provide].
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

  … where @racket[modifier] is one of the recognized @seclink["clmethoddefs" #:doc '(lib "scribblings/reference/reference.scrbl")]{method modifiers} like @racket[public], @racket[augride], and so on.
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
    (flow-macro (<~ f g)
      #'(~> g f))

    (map ☯(<~ add1 sqr) [1 2 3])
   }
}
