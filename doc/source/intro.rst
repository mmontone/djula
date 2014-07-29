.. highlightlang:: html+django
		   
Introduction
============

Djula is an HTML templating system similar to Django templates for Common Lisp.

Djula's template language is designed to strike a balance between power and
ease. It's designed to feel comfortable to those used to working with HTML.

.. admonition:: Philosophy

    If you have a background in programming, or if you're used to languages
    which mix programming code directly into HTML, you'll want to bear in
    mind that the Djula template system is not simply Common Lisp code embedded into
    HTML. This is by design: the template system is meant to express
    presentation, not program logic.

    The Djula template system provides tags which function similarly to some
    programming constructs -- an :ttag:`if` tag for boolean tests, a :ttag:`for`
    tag for looping, etc. -- but these are not simply executed as the
    corresponding Lisp code, and the template system will not execute
    arbitrary Lisp expressions. Only the tags, filters and syntax listed below
    are supported by default (although you can add :doc:`your own extensions
    </howto/custom-template-tags>` to the template language as needed).

Prerequisites
-------------

TODO: list of Common Lisp compilers Djula works on.

Installation
------------

Djula is available on Quicklisp:

.. code-block:: common-lisp

   (ql:quickload :djula)
