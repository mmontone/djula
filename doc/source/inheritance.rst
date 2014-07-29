.. highlightlang:: html+django
		   
Template inheritance
====================

The most powerful -- and thus the most complex -- part of Djula's template
engine is template inheritance. Template inheritance allows you to build a base
"skeleton" template that contains all the common elements of your site and
defines **blocks** that child templates can override.

It's easiest to understand template inheritance by starting with an example::

    <!DOCTYPE html>
    <html lang="en">
    <head>
        <link rel="stylesheet" href="style.css" />
        <title>{% block title %}My amazing site{% endblock %}</title>
    </head>

    <body>
        <div id="sidebar">
            {% block sidebar %}
            <ul>
                <li><a href="/">Home</a></li>
                <li><a href="/blog/">Blog</a></li>
            </ul>
            {% endblock %}
        </div>

        <div id="content">
            {% block content %}{% endblock %}
        </div>
    </body>
    </html>

This template, which we'll call ``base.html``, defines a simple HTML skeleton
document that you might use for a simple two-column page. It's the job of
"child" templates to fill the empty blocks with content.

In this example, the :ttag:`block` tag defines three blocks that child
templates can fill in. All the :ttag:`block` tag does is to tell the template
engine that a child template may override those portions of the template.

A child template might look like this::

    {% extends "base.html" %}

    {% block title %}My amazing blog{% endblock %}

    {% block content %}
    {% for entry in blog_entries %}
        <h2>{{ entry.title }}</h2>
        <p>{{ entry.body }}</p>
    {% endfor %}
    {% endblock %}

The :ttag:`extends` tag is the key here. It tells the template engine that
this template "extends" another template. When the template system evaluates
this template, first it locates the parent -- in this case, "base.html".

At that point, the template engine will notice the three :ttag:`block` tags
in ``base.html`` and replace those blocks with the contents of the child
template. Depending on the value of ``blog_entries``, the output might look
like::

    <!DOCTYPE html>
    <html lang="en">
    <head>
        <link rel="stylesheet" href="style.css" />
        <title>My amazing blog</title>
    </head>

    <body>
        <div id="sidebar">
            <ul>
                <li><a href="/">Home</a></li>
                <li><a href="/blog/">Blog</a></li>
            </ul>
        </div>

        <div id="content">
            <h2>Entry one</h2>
            <p>This is my first entry.</p>

            <h2>Entry two</h2>
            <p>This is my second entry.</p>
        </div>
    </body>
    </html>

Note that since the child template didn't define the ``sidebar`` block, the
value from the parent template is used instead. Content within a ``{% block %}``
tag in a parent template is always used as a fallback.

You can use as many levels of inheritance as needed. One common way of using
inheritance is the following three-level approach:

* Create a ``base.html`` template that holds the main look-and-feel of your
  site.
* Create a ``base_SECTIONNAME.html`` template for each "section" of your
  site. For example, ``base_news.html``, ``base_sports.html``. These
  templates all extend ``base.html`` and include section-specific
  styles/design.
* Create individual templates for each type of page, such as a news
  article or blog entry. These templates extend the appropriate section
  template.

This approach maximizes code reuse and makes it easy to add items to shared
content areas, such as section-wide navigation.

Here are some tips for working with inheritance:

* If you use :ttag:`{% extends %}<extends>` in a template, it must be the first template
  tag in that template. Template inheritance won't work, otherwise.

* More :ttag:`{% block %}<block>` tags in your base templates are better. Remember,
  child templates don't have to define all parent blocks, so you can fill
  in reasonable defaults in a number of blocks, then only define the ones
  you need later. It's better to have more hooks than fewer hooks.

* If you find yourself duplicating content in a number of templates, it
  probably means you should move that content to a ``{% block %}`` in a
  parent template.

* If you need to get the content of the block from the parent template,
  the ``{{ block.super }}`` variable will do the trick. This is useful
  if you want to add to the contents of a parent block instead of
  completely overriding it. Data inserted using ``{{ block.super }}`` will
  not be automatically escaped (see the `next section`_), since it was
  already escaped, if necessary, in the parent template.

* For extra readability, you can optionally give a *name* to your
  ``{% endblock %}`` tag. For example::

      {% block content %}
      ...
      {% endblock content %}

  In larger templates, this technique helps you see which ``{% block %}``
  tags are being closed.

Finally, note that you can't define multiple :ttag:`block` tags with the same
name in the same template. This limitation exists because a block tag works in
"both" directions. That is, a block tag doesn't just provide a hole to fill --
it also defines the content that fills the hole in the *parent*. If there were
two similarly-named :ttag:`block` tags in a template, that template's parent
wouldn't know which one of the blocks' content to use.
