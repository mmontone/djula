Variables
=========

Variables look like this: ``{{ variable }}``. When the template engine
encounters a variable, it evaluates that variable and replaces it with the
result. Variable names consist of any combination of alphanumeric characters
and the underscore (``"_"``). The dot (``"."``) also appears in variable
sections, although that has a special meaning, as indicated below.
Importantly, *you cannot have spaces or punctuation characters in variable
names.*

Use a dot (``.``) to access attributes of a variable.

.. admonition:: Behind the scenes

    Technically, when the template system encounters a dot, it tries the
    following lookups, in this order:

    * Dictionary lookup
    * Attribute lookup
    * Method call
    * List-index lookup

    This can cause some unexpected behavior with objects that override
    dictionary lookup. For example, consider the following code snippet that
    attempts to loop over a ``collections.defaultdict``::

        {% for k, v in defaultdict.iteritems %}
            Do something with k and v here...
        {% endfor %}

    Because dictionary lookup happens first, that behavior kicks in and provides
    a default value instead of using the intended ``.iteritems()``
    method. In this case, consider converting to a dictionary first.

In the above example, ``{{ section.title }}`` will be replaced with the
``title`` attribute of the ``section`` object.

If you use a variable that doesn't exist, the template system will insert
the value of the :setting:`TEMPLATE_STRING_IF_INVALID` setting, which is set
to ``''`` (the empty string) by default.

Note that "bar" in a template expression like ``{{ foo.bar }}`` will be
interpreted as a literal string and not using the value of the variable "bar",
if one exists in the template context.
