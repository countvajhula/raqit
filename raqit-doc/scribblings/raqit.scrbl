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

A general purpose Scheme-like language designed for clarity and economy of expression, with the goal of weaving together intuitive, expressive DSLs for major language facilities, and an emphasis on @seclink["top" #:indirect? #t #:doc '(lib "qi/scribblings/qi.scrbl")]{Qi} and functional programming.

@bold{Note}: Raqit is a placeholder name for a language being developed as a @hyperlink["https://github.com/countvajhula/raqit"]{series of community experiments} on DSLs in the @hyperlink["https://racket-lang.org/"]{Racket community}, to be synthesized into a cohesive language that @hyperlink["https://countvajhula.com/2024/12/09/langs-that-fit-in-your-head/"]{"fits in your head."} Join in on the fun and do your own experiments! The @hyperlink["https://github.com/countvajhula/raqit/blob/main/README.rst"]{project README} describes how.

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

@subsection{Lambdas}

@deftogether[(
  @defform[(lambda args body ...)]
  @defform[(λ args body ...)]
  @defform[#:link-target? #f (lambda [args body ...] ...)]
  @defform[#:link-target? #f (λ [args body ...] ...)]
  )]{
  Identical to either Racket's @racket[lambda] or @racket[case-lambda], depending on the syntax used.

  @codeblock{
    (λ (a) (+ a a))
    (λ [(a) (+ a a)]
       [(a b) (+ a b)]
       [args (apply + args)])
   }
}

Lambdas power Raqit's @racket[fun] but should rarely be used directly — favor using Qi flows via @racket[☯()], instead.

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

@section{Relations and Operators}

@defproc[(= [#:key key (-> any/c any/c) #f] [v any/c] ...)
         boolean?]{
  Identical to Racket's @racket[equal?] except that it supports an optional @racket[key] function that is applied to the arguments prior to comparison, if provided.

@codeblock{
  (= 3 4)
  (= #:key string-upcase "abc" "ABC")
 }
}

@deftogether[(
  @defproc[(< [#:key key (-> any/c orderable?) #f]
            [v any/c]
            ...)
         boolean?]
  @defproc[(<= [#:key key (-> any/c orderable?) #f]
            [v any/c]
            ...)
         boolean?]
  @defproc[(>= [#:key key (-> any/c orderable?) #f]
            [v any/c]
            ...)
         boolean?]
  @defproc[(> [#:key key (-> any/c orderable?) #f]
            [v any/c]
            ...)
         boolean?]
  @defproc[(min [#:key key (-> any/c orderable?) #f]
            [v any/c]
            ...)
         boolean?]
  @defproc[(max [#:key key (-> any/c orderable?) #f]
            [v any/c]
            ...)
         boolean?]
  )]{
  @emph{Generic} order relations supporting any orderable type, and extensible via @racket[(implements orderable ...)]. Identical to @racket[<], @racket[<=], @racket[>=], @racket[>], @racket[min], and @racket[max] from @seclink["Order_Relations" #:indirect? #t #:doc '(lib "relation/scribblings/relation.scrbl")]{@racket[relation/order]}.
}

@defproc[(~ [v appendable?]
            ...)
         appendable?]{
 Append the provided values together, using the type's implementation of the @racket[appendable] protocol. The special value @racket[ID] serves as the generic identity value when the type of the operands is not known. In particular, this value is the result when no operands are provided.

 Identical to @racket[~] from @seclink["Composing_Operations" #:indirect? #t #:doc '(lib "relation/scribblings/relation.scrbl")]{@racket[relation/composition]}.

@codeblock{
  (~ [1 2 3] [4 5 6] [7 8 9])
  (~ "abc" "def" "ghijk")
 }
}

@section{Lists}

Raqit includes @racket[racket/base], so many standard list-processing utilties are available.

@codeblock{
    (map ☯(~> sqr add1) [1 2 3 4 5])
    (map ☯(switch [positive? add1] [else sub1]) [1 -2 3 -4 5])
}

However, for any nontrivial list processing, it's advisable to use Qi flows, as they use the operations from @racket[qi/list] which are more efficient (and more clear).

@codeblock{
  (~> ([1 2 3 4 5]) (filter odd?) (map sqr) (foldl + 0))
}

@defproc[(: [v any/c] ... [vs list?])
         list?]{
  When used as an expression, construct a list; when used as a pattern, match a list. Alias for Racket's @racket[list*] both as an expression and as a pattern.

  @codeblock{
    (: 1 [2 3])
    (: 1 2 3 [4 5 6])
    (def [: x xs] [1 2 3])
  }
}

@section{Generics}

Many of Raqit's @seclink["Relations_and_Operators"]{relations and operators} are generic to any type. You can also define your own generic functions using @emph{protocols}.

@defform[(protocol stx ...)]{
  Alias for Racket's @racket[define-generics].

  @codeblock{
    (protocol stack
      (push stack element)
      (pop stack)
      (top stack))
   }

 Protocols may be implemented in user-defined types via, e.g., @racket[(implements stack method ...)].
}

@section{User-defined Datatypes}

@defform[(struct stx ...)]{
  Identical to Racket's @racket[struct], except that it is @racket[#:transparent] by default, and supports the syntax @racket[(implements protocol-name method ...)] instead of Racket's @racket[#:methods gen:interface-name [method ...]] for generic interfaces, where @racket[protocol-name] is the name of the protocol rather than its produced binding, e.g., @racket[(implements stack ...)] rather than @racket[(implements gen:stack ...)].
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
    (loop go ([a 5])
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
  @defform[(fun name args body ...)]
  @defform[#:link-target? #f (fun name [args body ...] ...)]
  )]{
  Similar to Racket's @racket[define] for function definitions, but also supports specifying multiple arities like Racket's @racket[case-lambda].

  @codeblock{
    (fun do-something (v)
      (abs v))
    (fun do-something args
      (length args))
    (fun do-something
      [(v) (abs v)]
      [args (length args)])
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

@section{Patterns}

Many of Raqit's binding forms, including @racket[def], @racket[let] and @racket[loop] bind by @emph{matching patterns}. In addition to @seclink["match" #:doc '(lib "scribblings/reference/reference.scrbl")]{Racket's patterns}, Raqit defines a few of its own.

@racket[[: v ...]] matches and destructures lists. It's equivalent to Racket's @racket[list*] pattern.

@racket[#{e ...}] matches and destructures sets. To match subsets, use the wildcard sequence pattern, e.g., @racket[#{e₁ e₂ _ ...}].

@racket[{k v ...}] matches and destructures hashes. To match only some keys (the most common case), use the wildcard sequence pattern, e.g., @racket[{k v _ ...}]. Note that the wildcard sequence pattern here @emph{must} come at the end.

@section{Macros}

@deftogether[(
  @defform[(macro (name pattern ...) directive ... template)]
  @defform[#:link-target? #f (macro name parse-option ... [pattern directive ... template] ...)]
  )]{
  A unified way to define macros, both for the host language as well as native DSLs.

  Ordinarily, this defines host language macros, and is identical to either Racket's @racket[define-syntax-parse-rule] or @racket[define-syntax-parser], depending on whether the simple rewrite pattern or the dispatch pattern is used.

  @codeblock{
    (macro (where expr bindings)
      (def bindings expr))
    (macro hello
      [(_ name) #'(displayln (format "hello ~a" name))]
      [(_) #'(displayln (format "hello ~a" "world"))])
   }

  If the pattern is prefixed with ☯, this defines @emph{Qi} macros, and is identical to Qi's @racket[define-qi-syntax-rule] or @racket[define-qi-syntax-parser], depending on the pattern used.

  @codeblock{
    (macro ☯(<~ f g)
      (~> g f))

    (map ☯(<~ add1 sqr) [1 2 3])
   }
}

@section{Interoperating with Racket}

Raqit includes all of @racket[racket/base]. Any installed Racket collection may be used in the usual way, e.g., @racket[(use racket/list)].
