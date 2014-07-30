.. _template-comments:

Comments
========

To comment-out part of a line in a template, use the comment syntax: ``{# #}``.

For example, this template would render as ``'hello'``::

    {# greeting #}hello

A comment can contain any template code, invalid or not. For example::

    {# {% if foo %}bar{% else %} #}

This syntax can only be used for single-line comments (no newlines are permitted
between the ``{#`` and ``#}`` delimiters). If you need to comment out a
multiline portion of the template, see the :ttag:`comment` tag.
