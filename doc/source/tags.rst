.. highlightlang:: html+django
		   
Tags
====

Tags look like this: ``{% tag %}``. Tags are more complex than variables: Some
create text in the output, some control flow by performing loops or logic, and
some load external information into the template to be used by later variables.

Some tags require beginning and ending tags (i.e. ``{% tag %} ... tag contents
... {% endtag %}``).

Djula ships with about two dozen built-in template tags. You can read all about
them in the :ref:`built-in tag reference <ref-templates-builtins-tags>`. To give
you a taste of what's available, here are some of the more commonly used
tags:

:ttag:`for`
    Loop over each item in an array. For example, to display a list of athletes
    provided in ``athlete_list``::

        <ul>
        {% for athlete in athlete_list %}
            <li>{{ athlete.name }}</li>
        {% endfor %}
        </ul>

:ttag:`if`, ``elif``, and ``else``
    Evaluates a variable, and if that variable is "true" the contents of the
    block are displayed::

        {% if athlete_list %}
            Number of athletes: {{ athlete_list|length }}
        {% elif athlete_in_locker_room_list %}
            Athletes should be out of the locker room soon!
        {% else %}
            No athletes.
        {% endif %}

    In the above, if ``athlete_list`` is not empty, the number of athletes
    will be displayed by the ``{{ athlete_list|length }}`` variable. Otherwise,
    if ``athlete_in_locker_room_list`` is not empty, the message "Athletes
    should be out..." will be displayed. If both lists are empty,
    "No athletes." will be displayed.

    You can also use filters and various operators in the :ttag:`if` tag::

        {% if athlete_list|length > 1 %}
           Team: {% for athlete in athlete_list %} ... {% endfor %}
        {% else %}
           Athlete: {{ athlete_list.0.name }}
        {% endif %}

    While the above example works, be aware that most template filters return
    strings, so mathematical comparisons using filters will generally not work
    as you expect. :tfilter:`length` is an exception.

:ttag:`block` and :ttag:`extends`
    Set up `template inheritance`_ (see below), a powerful way
    of cutting down on "boilerplate" in templates.

Again, the above is only a selection of the whole list; see the :ref:`built-in
tag reference <ref-templates-builtins-tags>` for the complete list.

You can also create your own custom template tags; see
:doc:`/howto/custom-template-tags`.

.. seealso::

    Djula's admin interface can include a complete reference of all template
    tags and filters available for a given site. See
    :doc:`/ref/contrib/admin/admindocs`.

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
