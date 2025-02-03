raqit
=====

Language experiments in Racket. The blog post `#langs that Fit in Your Head <https://countvajhula.com/2024/12/09/langs-that-fit-in-your-head/>`_ introduces this repo.

Install
-------

1. Clone this repo.
2. While in the repo root folder: ``raco pkg install --link raqit``.
3. Try out languages using the examples folder in the top level of the repo as a starting point.

Doing an Experiment
-------------------

To make a new language, just copy an existing one wholesale to a new path (like ``6`` or ``7``, in keeping with the existing numbering scheme). Likewise, copy the toplevel entry module. Rename all the "5" to "6" (or whatever number your new language is). Similarly, copy the examples for the language as well.

Now, add a new module for whatever specific language feature you want to experiment with (say, exceptions). Once you've got the functionality you want, ``require`` that module in the ``expander.rkt`` module and reprovide it in the way you'd like it to be used (i.e., typically, just require and reprovide as is, or with some simple renaming to override Racket's built-in versions). The existing languages have examples of all of these things so it's just a matter of following those patterns and being careful to rename all references to the other language to your language (there are only a few such references -- maybe four or five). Remember to update the examples for your new language to reflect your changes.

We'll find easier ways to do experiments as we go, but this is pretty easy, if slightly tedious!

Non-Ownership
=============

This work is not owned by anyone. Please see the `Declaration of Non-Ownership <https://github.com/drym-org/foundation/blob/main/Declaration_of_Non_Ownership.md>`_.
