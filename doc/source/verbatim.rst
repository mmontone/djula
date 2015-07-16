.. _template-verbatim:

Verbatim
========

If you want to leave some portion of text unprocessed by Djula, use the verbatim syntax: ``{$ $}``.

For example, this template would render as ``'this is {{verbatim}}'``::

    {$ this is {{verbatim}} $}
