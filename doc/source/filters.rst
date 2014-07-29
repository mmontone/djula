.. highlightlang:: html+django
		   
Filters
=======

You can modify variables for display by using **filters**.

Filters look like this: ``{{ name|lower }}``. This displays the value of the
``{{ name }}`` variable after being filtered through the :tfilter:`lower`
filter, which converts text to lowercase. Use a pipe (``|``) to apply a filter.

Filters can be "chained." The output of one filter is applied to the next.
``{{ text|escape|linebreaks }}`` is a common idiom for escaping text contents,
then converting line breaks to ``<p>`` tags.

Some filters take arguments. A filter argument looks like this: ``{{
bio|truncatewords:30 }}``. This will display the first 30 words of the ``bio``
variable.

Filter arguments that contain spaces must be quoted; for example, to join a
list with commas and spaced you'd use ``{{ list|join:", " }}``.

Djula provides about thirty built-in template filters. You can read all about
them in the :ref:`built-in filter reference <ref-templates-builtins-filters>`.
To give you a taste of what's available, here are some of the more commonly
used template filters:
