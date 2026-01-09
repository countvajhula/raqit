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

@deftogether[(
  @defproc[(empty? [v any/c])
           boolean?]
  @defproc[(some? [v any/c])
           boolean?]
  )]{
  @racket[empty?] is a @emph{generic} predicate to check for an empty value of any type. @racket[some?] is the opposite of @racket[empty?].

  @codeblock{
    (empty? [1 2 3])
    (empty? [])
    (empty? "hello")
    (empty? "")
    (some? [1 2 3])
    (some? "")
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

@section{For Racket Programmers}

This section motivates some of the choices in Raqit.

@subsection{@racket[def] and @racket[fun]}

Racket has a single @racket[define] form for defining both values and functions. Raqit has separate @racket[def] and @racket[fun] forms. This is so that the former can also handle binding multiple values unambiguously, which in Racket requires the separate @racket[define-values] form. It also allows @racket[fun] to unambiguously handle specifying both single as well as multiple arities for the function, which in Racket requires reliance on a separate @racket[case-lambda].

The names are chosen to be familiar from other contexts: Clojure and Python use the name @racket[def], while Rhombus uses the name @racket[fun] for the function-defining form.

In general, Raqit borrows the idea from many popular languages that defining forms need not begin with the word "define." Thus, @racket[fun] defines a function, @racket[flow] defines a Qi flow, and @racket[class] defines a class. In Racket, all of these require a prefacing form of @racket[define], although this policy of Raqit's is consistent with the use of @racket[struct] in Racket to define a structure type. Likewise, @racket[protocol] defines a generic interface (the specific name is less important, but this one was chosen to match a similar facility in Clojure and in Python). An advantage of this approach, in addition to brevity, is that it makes it easier for the programmer to identify class definitions, function definitions, protocol definitions, etc., at a glance.

@subsection{Unquoted literals}

Racket literals are quoted, meaning that, e.g., @racket['(a b c)] does not evaluate its arguments, a point of surprise for many newcomers, and requiring the use of @racket[(list a b c)]. Raqit allows writing unquoted literals like @racket[[a b c]], supporting a practice followed by many programming languages, including by Rhombus. It is more convenient and more visually distinct.

@subsection{Interchangeable Delimiters}

At the same time, Racket programmers expect delimiter types to be interchangeable, and being able to preserve this in general syntax (aside from expression positions) is valuable. To accomplish this, Raqit handles @racket[[]] and @racket[()] parsing at the expansion stage, when the usage context (e.g., general syntax vs expression) is already known, allowing these to be distinguished only in expression contexts and not otherwise.

@subsection{Functional Programming}

Racket has adequate functional programming facilities but they are syntactically awkward (e.g., requiring the use of explicit @racket[conjoin], @racket[disjoin] and @racket[compose]) or conceptually gratuitous, requiring explicit use of currying, or entailing the frequent need to spell out explicit lambdas.

Raqit's design makes it easy to embed Qi flows anywhere in your programs and conveniently extend the syntax of such flows in the same way as any other language form. This enables easy specification of higher-order functions, including intuitive currying syntax (e.g., @racket[☯(f arg)]), effortless composition (e.g., @racket[~>], @racket[and] and @racket[or]), point-free dispatching (@racket[switch]) to express many common conditionals, functional optimizations not possible in Racket (deforestation), and more. Raqit strongly encourages the functional style, and with simple and elegant syntax.

@subsection{Host Language and Hosted Languages}

As Racket is a general ecosystem for languages, the use of any particular DSL requires a dedicated interface macro, and dedicated forms for macro-extension. Raqit aims to be a proof-of-concept of seamlessly weaving specialized hosted DSLs into the general-purpose host language by leveraging technologies such as @seclink["top" #:indirect? #t #:doc '(lib "syntax-spec-v3/scribblings/main.scrbl")]{Syntax Spec}, to gain the generality of the host as well as benefit from tailored, elegant syntax and performance from the specializations of each DSL. The ability to use and extend Qi flows using familiar host language patterns (e.g., the @racket[macro] form) is one example.

@subsection[#:tag "frp:rao"]{Relations and Operators}

Racket has special relations and operators for working with every type. Raqit provides generic operators where it makes sense, including order relations like @racket[<] that can sort any orderable type (e.g., numbers, strings, sets), and the @racket[~] append operator that can append any appendable type (e.g., lists, strings, vectors). Racket's type-specific relations are the most efficient, but the huge gain in economy of expression is worth the modest performance cost from generic dispatch in most cases.

@subsection{Unified @racket[macro] Form}

Racket provides a diverse range of macro-defining forms and at least two separate macro systems! In addition, DSLs provide their own macro-defining forms.

Raqit unifies both single and multi-clause pattern-based macros using the single @racket[macro] form. Additionally, it uses reader-level syntax to delegate macro definitions to DSL-specific macro-defining forms, once again, expecting only the same syntax common to all macro-defining forms, i.e., single-clause vs multi-clause patterns.

This unification greatly simplifies defining macros, and also supports DSLs being seamlessly part of the host language while still retaining clear formal separation.

@subsection{@racket[:] Instead of @racket[cons] and @racket[list*]}

@racket[cons] intuitively generalizes to @racket[list*], so Raqit handles both using the common and standard syntax, @racket[:] (borrowed from functional languages like Haskell), usable in both expression and pattern-matching contexts.

@subsection{Pervasive pattern-matching}

In many cases, ordinary binding forms are naturally generalized by pattern-matching ones. Raqit (like Rhombus) scrupulously uses the pattern-matching generalizations offered by Racket, exclusively, avoiding the dichotomy without any loss in expressiveness.

@subsection{@racket[protocol] and @racket[implements]}

Declaring that a particular Racket struct type implements a generic interface has a complicated and non-standard syntax, where the keyword argument @racket[#:methods] groups the following two otherwise ungrouped arguments, with the second one requiring an extra pair of delimiters to wrap the defined methods. Explicit grouping with @racket[implements] simplifies this significantly, and also allows us to expect simply the @emph{name} of the interface being implemented (e.g., @racket[(implements stack ...)]), instead of its exact binding (i.e., avoiding @racket[gen:stack]).

@subsection{@racket[loop] as Named @racket[let]}

Racket's @racket[let] does double duty, both as a binding form as well a looping form. This makes it very useful, but it also prevents programmers from gaining valuable clues from visually scanning code that could allow them to identify that the form refers to a recursion rather than a simple scoped expression. Therefore, Raqit's @racket[let] explicitly @emph{forbids} a name, and @racket[loop], while otherwise equivalent, explicitly @emph{requires} a name. Additionally, both of these are pattern matching, making them strictly more powerful than Racket's @racket[let] (and equivalent to @racket[match-let]).

@subsection{Research and Planned Features}

It would be valuable to explore:

@itemlist[#:style 'ordered
  @item{More dedicated DSLs for major language facilities, such as exception-handling}
  @item{List comprehensions}
  @item{Pervasive persistent, immutable data structures}
  @item{Generic collection and sequence operations}
]

There are also many compelling research directions being explored for Qi, intended to enable new possibilities for functional #langs including Raqit.
