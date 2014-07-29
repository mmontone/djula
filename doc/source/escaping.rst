.. highlightlang:: html+django
		   
Automatic HTML escaping
=======================

When generating HTML from templates, there's always a risk that a variable will
include characters that affect the resulting HTML. For example, consider this
template fragment::

    Hello, {{ name }}.

At first, this seems like a harmless way to display a user's name, but consider
what would happen if the user entered their name as this::

    <script>alert('hello')</script>

With this name value, the template would be rendered as::

    Hello, <script>alert('hello')</script>

...which means the browser would pop-up a JavaScript alert box!

Similarly, what if the name contained a ``'<'`` symbol, like this?

.. code-block:: html

    <b>username

That would result in a rendered template like this::

    Hello, <b>username

...which, in turn, would result in the remainder of the Web page being bolded!

Clearly, user-submitted data shouldn't be trusted blindly and inserted directly
into your Web pages, because a malicious user could use this kind of hole to
do potentially bad things. This type of security exploit is called a
`Cross Site Scripting`_ (XSS) attack.

To avoid this problem, you have two options:

* One, you can make sure to run each untrusted variable through the
  :tfilter:`escape` filter (documented below), which converts potentially
  harmful HTML characters to unharmful ones. This was the default solution
  in Django for its first few years, but the problem is that it puts the
  onus on *you*, the developer / template author, to ensure you're escaping
  everything. It's easy to forget to escape data.

* Two, you can take advantage of Django's automatic HTML escaping. The
  remainder of this section describes how auto-escaping works.

By default in Django, every template automatically escapes the output
of every variable tag. Specifically, these five characters are
escaped:

* ``<`` is converted to ``&lt;``
* ``>`` is converted to ``&gt;``
* ``'`` (single quote) is converted to ``&#39;``
* ``"`` (double quote) is converted to ``&quot;``
* ``&`` is converted to ``&amp;``

Again, we stress that this behavior is on by default. If you're using Django's
template system, you're protected.

.. _Cross Site Scripting: http://en.wikipedia.org/wiki/Cross-site_scripting

How to turn it off
------------------

If you don't want data to be auto-escaped, on a per-site, per-template level or
per-variable level, you can turn it off in several ways.

Why would you want to turn it off? Because sometimes, template variables
contain data that you *intend* to be rendered as raw HTML, in which case you
don't want their contents to be escaped. For example, you might store a blob of
HTML in your database and want to embed that directly into your template. Or,
you might be using Django's template system to produce text that is *not* HTML
-- like an email message, for instance.

For individual variables
~~~~~~~~~~~~~~~~~~~~~~~~

To disable auto-escaping for an individual variable, use the :tfilter:`safe`
filter::

    This will be escaped: {{ data }}
    This will not be escaped: {{ data|safe }}

Think of *safe* as shorthand for *safe from further escaping* or *can be
safely interpreted as HTML*. In this example, if ``data`` contains ``'<b>'``,
the output will be::

    This will be escaped: &lt;b&gt;
    This will not be escaped: <b>

For template blocks
~~~~~~~~~~~~~~~~~~~

To control auto-escaping for a template, wrap the template (or just a
particular section of the template) in the :ttag:`autoescape` tag, like so::

    {% autoescape off %}
        Hello {{ name }}
    {% endautoescape %}

The :ttag:`autoescape` tag takes either ``on`` or ``off`` as its argument. At
times, you might want to force auto-escaping when it would otherwise be
disabled. Here is an example template::

    Auto-escaping is on by default. Hello {{ name }}

    {% autoescape off %}
        This will not be auto-escaped: {{ data }}.

        Nor this: {{ other_data }}
        {% autoescape on %}
            Auto-escaping applies again: {{ name }}
        {% endautoescape %}
    {% endautoescape %}

The auto-escaping tag passes its effect onto templates that extend the
current one as well as templates included via the :ttag:`include` tag,
just like all block tags. For example::

    # base.html

    {% autoescape off %}
    <h1>{% block title %}{% endblock %}</h1>
    {% block content %}
    {% endblock %}
    {% endautoescape %}


    # child.html

    {% extends "base.html" %}
    {% block title %}This & that{% endblock %}
    {% block content %}{{ greeting }}{% endblock %}

Because auto-escaping is turned off in the base template, it will also be
turned off in the child template, resulting in the following rendered
HTML when the ``greeting`` variable contains the string ``<b>Hello!</b>``::

    <h1>This & that</h1>
    <b>Hello!</b>

Notes
-----

Generally, template authors don't need to worry about auto-escaping very much.
Developers on the Python side (people writing views and custom filters) need to
think about the cases in which data shouldn't be escaped, and mark data
appropriately, so things Just Work in the template.

If you're creating a template that might be used in situations where you're
not sure whether auto-escaping is enabled, then add an :tfilter:`escape` filter
to any variable that needs escaping. When auto-escaping is on, there's no
danger of the :tfilter:`escape` filter *double-escaping* data -- the
:tfilter:`escape` filter does not affect auto-escaped variables.

.. _string-literals-and-automatic-escaping:

String literals and automatic escaping
--------------------------------------

As we mentioned earlier, filter arguments can be strings::

    {{ data|default:"This is a string literal." }}

All string literals are inserted **without** any automatic escaping into the
template -- they act as if they were all passed through the :tfilter:`safe`
filter. The reasoning behind this is that the template author is in control of
what goes into the string literal, so they can make sure the text is correctly
escaped when the template is written.

This means you would write ::

    {{ data|default:"3 &lt; 2" }}

...rather than ::

    {{ data|default:"3 < 2" }}  <-- Bad! Don't do this.

This doesn't affect what happens to data coming from the variable itself.
The variable's contents are still automatically escaped, if necessary, because
they're beyond the control of the template author.
