.. image:: https://img.shields.io/badge/documentation-raqit-blue
    :target: https://countvajhula.github.io/raqit/

raqit
=====

Language experiments in Racket. The blog post `#langs that Fit in Your Head <https://countvajhula.com/2024/12/09/langs-that-fit-in-your-head/>`_ introduces this repo.

`Read the documentation <https://countvajhula.github.io/raqit/>`__ to learn more about the current working language. After installing (instructions below), you can `try out the examples <https://github.com/countvajhula/raqit/blob/main/examples.rkt>`__ and use the language in your own programs.

Install
-------

1. Clone this repo.
2. While in the repo root folder: ``make install``.
3. Try out the language using the ``examples.rkt`` file at the top level of the repo, as a starting point.

How It Works
------------

The repo is structured so that each language experiment exists on its own dedicated branch. Every stable language has a branch name beginning with ``lang/`` --- for instance, ``lang/6``. To use this language, simply check out this branch. Every language is usable in any Racket module as:

.. code-block:: racket

  #lang raqit

If you have the ``lang/6`` branch checked out, then that's the language that will be in use in this module.

Note that if you check out a different branch in the ``raqit`` repo, your source module may no longer work. So it may be worth noting the branch name in a comment in the source module, so that you know *which* language experiment it's using.

.. code-block:: racket

  #lang raqit  ; lang/6

What Does the Main Branch Contain?
----------------------------------

The ``main`` branch doesn't function as the usual kind of main branch, which is usually concerned with integrating diverse changes into a linear sequence of changes. Instead, the main branch here is simply an alias for an existing stable language, typically whatever the "latest hotness" may be (say, ``lang/6``). As new experiments usually start from recent stable languages, the language referred to by the main branch **typically synthesizes the best of many recent experiments**, so it's a good branch to start on.

The code, examples, and documentation within each branch are *self-contained and consistent*.

The repo is structured this way so that, even while supporting diverse experimentation, the repo is still usable as a language immediately when installed, without requiring an additional step to select the language by switching to a new branch. Please keep in mind, though, that unlike the ``lang/``-prefixed branches which are stable and linear, ``main`` could point to anything, and it isn't intended to track linear development (which there isn't, in this repo) --- at any given time, it's just an alias for some branch!

Doing an Experiment
-------------------

To make a new language, just pick any existing language as a starting point (by checking out its branch). Then, create a new branch and name it whatever you like (say, ``lang/9``).

Now, if you'd like to modify some existing feature of the language, just edit the corresponding module (say, ``let.rkt`` for the ``let`` binding form). Otherwise if you'd like to experiment with a new feature (say, exceptions), then add a new module for the feature. Once you've got the functionality you want, ``require`` that module in the ``expander.rkt`` module and reprovide it in the way you'd like it to be used (i.e., typically, just require and reprovide as is, or with some simple renaming to override Racket's built-in versions). The existing languages have examples of all of these things so it's just a matter of following those patterns.

Remember to update ``examples.rkt`` for your new language to reflect your changes and also document your new feature in ``raqit-doc/scribblings/raqit.scrbl``.

If you'd like to customize syntax at the reader level (e.g., for handling special symbols, delimiters, prefixes or suffixes that don't follow the usual Lisp (symex-oriented) syntax) rather than at the expander stage (i.e., macros), then refer to ``reader.rkt`` which contains the language `readtable <https://docs.racket-lang.org/guide/hash-reader.html#(part._readtable)>`_, including some existing extensions that you could follow as examples.

Using Raqit Outside of Raqit
----------------------------

In general, features developed for Raqit are intended to be modular and self-contained and available to the entire Racket ecosystem. Once a feature stabilizes and seems to have attained a well-scoped objective, it should be possible (and would be best) to release it as a distinct Racket package which then becomes a dependency of Raqit. But typically, this will only be done in response to a tangible need, e.g., a request from a user.

So if there is a feature in Raqit that you would like to use in Racket or in another ``#lang``, please submit an issue, or even, of course, feel free to extract the component yourself, if you'd like (but ideally, please start a conversation so that Raqit could continue to use the component as a dependency without maintaining an independent implementation), and please retain the public domain dedication.

Non-Ownership
=============

This work is not owned by anyone. Please see the `Declaration of Non-Ownership <https://github.com/drym-org/foundation/blob/main/Declaration_of_Non_Ownership.md>`_.
