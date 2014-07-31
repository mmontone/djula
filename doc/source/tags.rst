.. highlightlang:: html+django
		   
Tags
====

Overview
--------

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
    provided in ``athlete-list``::

        <ul>
        {% for athlete in athlete-list %}
            <li>{{ athlete.name }}</li>
        {% endfor %}
        </ul>

:ttag:`if`, ``elif``, and ``else``
    Evaluates a variable, and if that variable is "true" the contents of the
    block are displayed::

        {% if athlete-list %}
            Number of athletes: {{ athlete-list|length }}
        {% elif athlete-in-locker-room-list %}
            Athletes should be out of the locker room soon!
        {% else %}
            No athletes.
        {% endif %}

    In the above, if ``athlete-list`` is not empty, the number of athletes
    will be displayed by the ``{{ athlete-list|length }}`` variable. Otherwise,
    if ``athlete-in-locker-room-list`` is not empty, the message "Athletes
    should be out..." will be displayed. If both lists are empty,
    "No athletes." will be displayed.

    You can also use filters and various operators in the :ttag:`if` tag::

        {% if athlete-list|length > 1 %}
           Team: {% for athlete in athlete-list %} ... {% endfor %}
        {% else %}
           Athlete: {{ athlete-list.0.name }}
        {% endif %}

    While the above example works, be aware that most template filters return
    strings, so mathematical comparisons using filters will generally not work
    as you expect. :tfilter:`length` is an exception.

:ttag:`block` and :ttag:`extends`
    Set up `template inheritance`_ (see below), a powerful way
    of cutting down on "boilerplate" in templates.

List of tags
------------

.. contents:: Tags
   :local:

.. templatetag:: block
		 
block
^^^^^

Defines a block that can be overridden by child templates.

Sample usage::

  {% block stylesheets %}
     ...
  {% endblock %}
	  
See :ref:`Template inheritance` for more information.

.. templatetag:: extends
		 
extends
^^^^^^^

Extends a template

Sample usage::

  {% extends "base.html" %}

.. templatetag:: super  

super
^^^^^

Gets the content of the block from the parent template. You have to pass the name of the block of the parent template you want to access.

Sample usage::

     {% super "stylesheets" %}

.. templatetag:: comment     

comment
^^^^^^^

Ignores everything between ``{% comment %}`` and ``{% endcomment %}``.
An optional note may be inserted in the first tag. For example, this is
useful when commenting out code for documenting why the code was disabled.

Sample usage::

    <p>Rendered text with {{ pub-date|date:"c" }}</p>
    {% comment "Optional note" %}
        <p>Commented out text with {{ create-date|date:"c" }}</p>
    {% endcomment %}

``comment`` tags cannot be nested.

..
   .. templatetag:: csrf-token

   csrf-token
   ^^^^^^^^^^

   This tag is used for CSRF protection, as described in the documentation for
   :doc:`Cross Site Request Forgeries </ref/contrib/csrf>`.

.. templatetag:: cycle

cycle
^^^^^

Produces one of its arguments each time this tag is encountered. The first
argument is produced on the first encounter, the second argument on the second
encounter, and so forth. Once all arguments are exhausted, the tag cycles to
the first argument and produces it again.

This tag is particularly useful in a loop::

    {% for o in some-list %}
        <tr class="{% cycle 'row1' 'row2' %}">
            ...
        </tr>
    {% endfor %}

The first iteration produces HTML that refers to class ``row1``, the second to
``row2``, the third to ``row1`` again, and so on for each iteration of the
loop.

You can use variables, too. For example, if you have two template variables,
``rowvalue1`` and ``rowvalue2``, you can alternate between their values like
this::

    {% for o in some-list %}
        <tr class="{% cycle rowvalue1 rowvalue2 %}">
            ...
        </tr>
    {% endfor %}

Variables included in the cycle will be escaped.  You can disable auto-escaping
with::

    {% for o in some-list %}
        <tr class="{% autoescape off %}{% cycle rowvalue1 rowvalue2 %}{% endautoescape %}
            ...
        </tr>
    {% endfor %}

You can mix variables and strings::

    {% for o in some-list %}
        <tr class="{% cycle 'row1' rowvalue2 'row3' %}">
            ...
        </tr>
    {% endfor %}

In some cases you might want to refer to the current value of a cycle
without advancing to the next value. To do this,
just give the ``{% cycle %}`` tag a name, using "as", like this::

    {% cycle 'row1' 'row2' as rowcolors %}

From then on, you can insert the current value of the cycle wherever you'd like
in your template by referencing the cycle name as a context variable. If you
want to move the cycle to the next value independently of the original
``cycle`` tag, you can use another ``cycle`` tag and specify the name of the
variable. So, the following template::

    <tr>
        <td class="{% cycle 'row1' 'row2' as rowcolors %}">...</td>
        <td class="{{ rowcolors }}">...</td>
    </tr>
    <tr>
        <td class="{% cycle rowcolors %}">...</td>
        <td class="{{ rowcolors }}">...</td>
    </tr>

would output::

    <tr>
        <td class="row1">...</td>
        <td class="row1">...</td>
    </tr>
    <tr>
        <td class="row2">...</td>
        <td class="row2">...</td>
    </tr>

You can use any number of values in a ``cycle`` tag, separated by spaces.
Values enclosed in single quotes (``'``) or double quotes (``"``) are treated
as string literals, while values without quotes are treated as template
variables.

By default, when you use the ``as`` keyword with the cycle tag, the
usage of ``{% cycle %}`` that initiates the cycle will itself produce
the first value in the cycle. This could be a problem if you want to
use the value in a nested loop or an included template. If you only want
to declare the cycle but not produce the first value, you can add a
``silent`` keyword as the last keyword in the tag. For example::

    {% for obj in some-list %}
        {% cycle 'row1' 'row2' as rowcolors silent %}
        <tr class="{{ rowcolors }}">{% include "subtemplate.html" %}</tr>
    {% endfor %}

This will output a list of ``<tr>`` elements with ``class``
alternating between ``row1`` and ``row2``. The subtemplate will have
access to ``rowcolors`` in its context and the value will match the class
of the ``<tr>`` that encloses it. If the ``silent`` keyword were to be
omitted, ``row1`` and ``row2`` would be emitted as normal text, outside the
``<tr>`` element.

When the silent keyword is used on a cycle definition, the silence
automatically applies to all subsequent uses of that specific cycle tag.
The following template would output *nothing*, even though the second
call to ``{% cycle %}`` doesn't specify ``silent``::

    {% cycle 'row1' 'row2' as rowcolors silent %}
    {% cycle rowcolors %}

For backward compatibility, the ``{% cycle %}`` tag supports the much inferior
old syntax from previous Django versions. You shouldn't use this in any new
projects, but for the sake of the people who are still using it, here's what it
looks like::

    {% cycle row1,row2,row3 %}

In this syntax, each value gets interpreted as a literal string, and there's no
way to specify variable values. Or literal commas. Or spaces. Did we mention
you shouldn't use this syntax in any new projects?


.. templatetag:: debug

debug
^^^^^

Outputs a whole load of debugging information, including the current context
and imported modules.

.. templatetag:: filter

filter
^^^^^^

Filters the contents of the block through one or more filters. Multiple
filters can be specified with pipes and filters can have arguments, just as
in variable syntax.

Note that the block includes *all* the text between the ``filter`` and
``endfilter`` tags.

Sample usage::

    {% filter force-escape|lower %}
        This text will be HTML-escaped, and will appear in all lowercase.
    {% endfilter %}

.. note::

    The :tfilter:`escape` and :tfilter:`safe` filters are not acceptable
    arguments. Instead, use the :ttag:`autoescape` tag to manage autoescaping
    for blocks of template code.

.. templatetag:: firstof

firstof
^^^^^^^

Outputs the first argument variable that is not ``False``. Outputs nothing if
all the passed variables are ``False``.

Sample usage::

    {% firstof var1 var2 var3 %}

This is equivalent to::

    {% if var1 %}
        {{ var1|safe }}
    {% elif var2 %}
        {{ var2|safe }}
    {% elif var3 %}
        {{ var3|safe }}
    {% endif %}

You can also use a literal string as a fallback value in case all
passed variables are False::

    {% firstof var1 var2 var3 "fallback value" %}

This tag auto-escapes variable values. You can disable auto-escaping with::

    {% autoescape off %}
        {% firstof var1 var2 var3 "<strong>fallback value</strong>" %}
    {% endautoescape %}

Or if only some variables should be escaped, you can use::

    {% firstof var1 var2|safe var3 "<strong>fallback value</strong>"|safe %}

.. templatetag:: for

for
^^^

Loops over each item in an array, making the item available in a context
variable. For example, to display a list of athletes provided in
``athlete-list``::

    <ul>
    {% for athlete in athlete-list %}
        <li>{{ athlete.name }}</li>
    {% endfor %}
    </ul>

You can loop over a list in reverse by using
``{% for obj in list reversed %}``.

If you need to loop over a list of lists, you can unpack the values
in each sublist into individual variables. For example, if your context
contains a list of (x,y) coordinates called ``points``, you could use the
following to output the list of points::

    {% for x, y in points %}
        There is a point at {{ x }},{{ y }}
    {% endfor %}

This can also be useful if you need to access the items in a dictionary.
For example, if your context contained a dictionary ``data``, the following
would display the keys and values of the dictionary::

    {% for key, value in data.items %}
        {{ key }}: {{ value }}
    {% endfor %}

The for loop sets a number of variables available within the loop:

==========================  ===============================================
Variable                    Description
==========================  ===============================================
``forloop.counter``         The current iteration of the loop (1-indexed)
``forloop.counter0``        The current iteration of the loop (0-indexed)
``forloop.revcounter``      The number of iterations from the end of the
                            loop (1-indexed)
``forloop.revcounter0``     The number of iterations from the end of the
                            loop (0-indexed)
``forloop.first``           True if this is the first time through the loop
``forloop.last``            True if this is the last time through the loop
``forloop.parentloop``      For nested loops, this is the loop surrounding
                            the current one
==========================  ===============================================

for ... empty
^^^^^^^^^^^^^

The ``for`` tag can take an optional ``{% empty %}`` clause whose text is
displayed if the given array is empty or could not be found::

    <ul>
    {% for athlete in athlete-list %}
        <li>{{ athlete.name }}</li>
    {% empty %}
        <li>Sorry, no athletes in this list.</li>
    {% endfor %}
    </ul>

The above is equivalent to -- but shorter, cleaner, and possibly faster
than -- the following::

    <ul>
      {% if athlete-list %}
        {% for athlete in athlete-list %}
          <li>{{ athlete.name }}</li>
        {% endfor %}
      {% else %}
        <li>Sorry, no athletes in this list.</li>
      {% endif %}
    </ul>

.. templatetag:: if

if
^^

The ``{% if %}`` tag evaluates a variable, and if that variable is "true" (i.e.
exists, is not empty, and is not a false boolean value) the contents of the
block are output::

    {% if athlete-list %}
        Number of athletes: {{ athlete-list|length }}
    {% elif athlete-in-locker-room-list %}
        Athletes should be out of the locker room soon!
    {% else %}
        No athletes.
    {% endif %}

In the above, if ``athlete-list`` is not empty, the number of athletes will be
displayed by the ``{{ athlete-list|length }}`` variable.

As you can see, the ``if`` tag may take one or several ``{% elif %}``
clauses, as well as an ``{% else %}`` clause that will be displayed if all
previous conditions fail. These clauses are optional.

Boolean operators
^^^^^^^^^^^^^^^^^

:ttag:`if` tags may use ``and``, ``or`` or ``not`` to test a number of
variables or to negate a given variable::

    {% if athlete-list and coach-list %}
        Both athletes and coaches are available.
    {% endif %}

    {% if not athlete-list %}
        There are no athletes.
    {% endif %}

    {% if athlete-list or coach-list %}
        There are some athletes or some coaches.
    {% endif %}

    {% if not athlete-list or coach-list %}
        There are no athletes or there are some coaches (OK, so
        writing English translations of boolean logic sounds
        stupid; it's not our fault).
    {% endif %}

    {% if athlete-list and not coach-list %}
        There are some athletes and absolutely no coaches.
    {% endif %}

Use of both ``and`` and ``or`` clauses within the same tag is allowed, with
``and`` having higher precedence than ``or`` e.g.::

    {% if athlete-list and coach-list or cheerleader-list %}

will be interpreted like:

.. code-block:: python

    if (athlete-list and coach-list) or cheerleader-list

Use of actual parentheses in the :ttag:`if` tag is invalid syntax. If you need
them to indicate precedence, you should use nested :ttag:`if` tags.

:ttag:`if` tags may also use the operators ``==``, ``!=``, ``<``, ``>``,
``<=``, ``>=`` and ``in`` which work as follows:


``==`` operator
^^^^^^^^^^^^^^^

Equality. Example::

    {% if somevar == "x" %}
      This appears if variable somevar equals the string "x"
    {% endif %}

``!=`` operator
^^^^^^^^^^^^^^^

Inequality. Example::

    {% if somevar != "x" %}
      This appears if variable somevar does not equal the string "x",
      or if somevar is not found in the context
    {% endif %}

``<`` operator
^^^^^^^^^^^^^^

Less than. Example::

    {% if somevar < 100 %}
      This appears if variable somevar is less than 100.
    {% endif %}

``>`` operator
^^^^^^^^^^^^^^

Greater than. Example::

    {% if somevar > 0 %}
      This appears if variable somevar is greater than 0.
    {% endif %}

``<=`` operator
^^^^^^^^^^^^^^^

Less than or equal to. Example::

    {% if somevar <= 100 %}
      This appears if variable somevar is less than 100 or equal to 100.
    {% endif %}

``>=`` operator
^^^^^^^^^^^^^^^

Greater than or equal to. Example::

    {% if somevar >= 1 %}
      This appears if variable somevar is greater than 1 or equal to 1.
    {% endif %}

``in`` operator
^^^^^^^^^^^^^^^

Contained within. This operator is supported by many Python containers to test
whether the given value is in the container. The following are some examples
of how ``x in y`` will be interpreted::

    {% if "bc" in "abcdef" %}
      This appears since "bc" is a substring of "abcdef"
    {% endif %}

    {% if "hello" in greetings %}
      If greetings is a list or set, one element of which is the string
      "hello", this will appear.
    {% endif %}

    {% if user in users %}
      If users is a QuerySet, this will appear if user is an
      instance that belongs to the QuerySet.
    {% endif %}

``not in`` operator
^^^^^^^^^^^^^^^^^^^

Not contained within. This is the negation of the ``in`` operator.


The comparison operators cannot be 'chained' like in Python or in mathematical
notation. For example, instead of using::

    {% if a > b > c %}  (WRONG)

you should use::

    {% if a > b and b > c %}


Filters
^^^^^^^

You can also use filters in the :ttag:`if` expression. For example::

    {% if messages|length >= 100 %}
       You have lots of messages today!
    {% endif %}

.. templatetag:: ifchanged

ifchanged
^^^^^^^^^

Check if a value has changed from the last iteration of a loop.

The ``{% ifchanged %}`` block tag is used within a loop. It has two possible
uses.

1. Checks its own rendered contents against its previous state and only
   displays the content if it has changed. For example, this displays a list of
   days, only displaying the month if it changes::

        <h1>Archive for {{ year }}</h1>

        {% for date in days %}
            {% ifchanged %}<h3>{{ date|date:"F" }}</h3>{% endifchanged %}
            <a href="{{ date|date:"M/d"|lower }}/">{{ date|date:"j" }}</a>
        {% endfor %}

2. If given one or more variables, check whether any variable has changed.
   For example, the following shows the date every time it changes, while
   showing the hour if either the hour or the date has changed::

        {% for date in days %}
            {% ifchanged date.date %} {{ date.date }} {% endifchanged %}
            {% ifchanged date.hour date.date %}
                {{ date.hour }}
            {% endifchanged %}
        {% endfor %}

The ``ifchanged`` tag can also take an optional ``{% else %}`` clause that
will be displayed if the value has not changed::

        {% for match in matches %}
            <div style="background-color:
                {% ifchanged match.ballot-id %}
                    {% cycle "red" "blue" %}
                {% else %}
                    gray
                {% endifchanged %}
            ">{{ match }}</div>
        {% endfor %}

.. templatetag:: ifequal

ifequal
^^^^^^^

Output the contents of the block if the two arguments equal each other.

Example::

    {% ifequal user.pk comment.user-id %}
        ...
    {% endifequal %}

As in the :ttag:`if` tag, an ``{% else %}`` clause is optional.

The arguments can be hard-coded strings, so the following is valid::

    {% ifequal user.username "adrian" %}
        ...
    {% endifequal %}

An alternative to the ``ifequal`` tag is to use the :ttag:`if` tag and the
``==`` operator.

.. templatetag:: ifnotequal

ifnotequal
^^^^^^^^^^

Just like :ttag:`ifequal`, except it tests that the two arguments are not
equal.

An alternative to the ``ifnotequal`` tag is to use the :ttag:`if` tag and
the ``!=`` operator.

.. templatetag:: include

include
^^^^^^^

Loads a template and renders it with the current context. This is a way of
"including" other templates within a template.

The template name can either be a variable or a hard-coded (quoted) string,
in either single or double quotes.

This example includes the contents of the template ``"foo/bar.html"``::

    {% include "foo/bar.html" %}

This example includes the contents of the template whose name is contained in
the variable ``template-name``::

    {% include template-name %}

.. versionchanged:: 1.7

    The variable may also be any object with a ``render()`` method that
    accepts a context. This allows you to reference a compiled ``Template`` in
    your context.

An included template is rendered within the context of the template that
includes it. This example produces the output ``"Hello, John"``:

* Context: variable ``person`` is set to ``"john"``.
* Template::

    {% include "name-snippet.html" %}

* The ``name-snippet.html`` template::

    {{ greeting }}, {{ person|default:"friend" }}!

You can pass additional context to the template using keyword arguments::

    {% include "name-snippet.html" with person="Jane" greeting="Hello" %}

If you want to render the context only with the variables provided (or even
no variables at all), use the ``only`` option. No other variables are
available to the included template::

    {% include "name-snippet.html" with greeting="Hi" only %}

.. note::
    The :ttag:`include` tag should be considered as an implementation of
    "render this subtemplate and include the HTML", not as "parse this
    subtemplate and include its contents as if it were part of the parent".
    This means that there is no shared state between included templates --
    each include is a completely independent rendering process.

See also: :ttag:`{% ssi %}<ssi>`.

..
   .. templatetag:: load

   load
   ^^^^

   Loads a custom template tag set.

   For example, the following template would load all the tags and filters
   registered in ``somelibrary`` and ``otherlibrary`` located in package
   ``package``::

       {% load somelibrary package.otherlibrary %}

   You can also selectively load individual filters or tags from a library, using
   the ``from`` argument. In this example, the template tags/filters named ``foo``
   and ``bar`` will be loaded from ``somelibrary``::

       {% load foo bar from somelibrary %}

   See :doc:`Custom tag and filter libraries </howto/custom-template-tags>` for
   more information.

   .. templatetag:: lorem

   lorem
   ^^^^^

   .. versionadded:: 1.8

       The tag was previously located in :mod:`django.contrib.webdesign`.

   Displays random "lorem ipsum" Latin text. This is useful for providing sample
   data in templates.

   Usage::

       {% lorem [count] [method] [random] %}

   The ``{% lorem %}`` tag can be used with zero, one, two or three arguments.
   The arguments are:

   ===========  =============================================================
   Argument     Description
   ===========  =============================================================
   ``count``    A number (or variable) containing the number of paragraphs or
		words to generate (default is 1).
   ``method``   Either ``w`` for words, ``p`` for HTML paragraphs or ``b``
		for plain-text paragraph blocks (default is ``b``).
   ``random``   The word ``random``, which if given, does not use the common
		paragraph ("Lorem ipsum dolor sit amet...") when generating
		text.
   ===========  =============================================================

   Examples:

   * ``{% lorem %}`` will output the common "lorem ipsum" paragraph.
   * ``{% lorem 3 p %}`` will output the common "lorem ipsum" paragraph
     and two random paragraphs each wrapped in HTML ``<p>`` tags.
   * ``{% lorem 2 w random %}`` will output two random Latin words.

   .. templatetag:: now

   now
   ^^^

   Displays the current date and/or time, using a format according to the given
   string. Such string can contain format specifiers characters as described
   in the :tfilter:`date` filter section.

   Example::

       It is {% now "jS F Y H:i" %}

   Note that you can backslash-escape a format string if you want to use the
   "raw" value. In this example, "f" is backslash-escaped, because otherwise
   "f" is a format string that displays the time. The "o" doesn't need to be
   escaped, because it's not a format character::

       It is the {% now "jS o\f F" %}

   This would display as "It is the 4th of September".

   .. note::

       The format passed can also be one of the predefined ones
       :setting:`DATE_FORMAT`, :setting:`DATETIME_FORMAT`,
       :setting:`SHORT_DATE_FORMAT` or :setting:`SHORT_DATETIME_FORMAT`.
       The predefined formats may vary depending on the current locale and
       if :ref:`format-localization` is enabled, e.g.::

	   It is {% now "SHORT_DATETIME_FORMAT" %}    
      
Custom tags
-----------
