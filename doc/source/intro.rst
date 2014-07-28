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

    
Templates
=========

.. highlightlang:: html+django

A template is simply a text file. It can generate any text-based format (HTML,
XML, CSV, etc.).

A template contains **variables**, which get replaced with values when the
template is evaluated, and **tags**, which control the logic of the template.

Below is a minimal template that illustrates a few basics. Each element will be
explained later in this document.

.. code-block:: html+django

    {% extends "base_generic.html" %}

    {% block title %}{{ section.title }}{% endblock %}

    {% block content %}
    <h1>{{ section.title }}</h1>

    {% for story in story_list %}
    <h2>
      <a href="{{ story.get_absolute_url }}">
        {{ story.headline|upper }}
      </a>
    </h2>
    <p>{{ story.tease|truncatewords:"100" }}</p>
    {% endfor %}
    {% endblock %}

.. admonition:: Philosophy

    Why use a text-based template instead of an XML-based one (like Zope's
    TAL)? We wanted Django's template language to be usable for more than
    just XML/HTML templates. At World Online, we use it for emails,
    JavaScript and CSV. You can use the template language for any text-based
    format.

    Oh, and one more thing: Making humans edit XML is sadistic!
    
..
   Example:

   .. code-block:: html+jinja

      <title>{% block title %}{% endblock %}</title>
      <ul>
	{% for user in users %}
	  <li><a href="{{ user.url }}">{{ user.username }}</a></li>
	{% endfor %}
      </ul>

Prerequisites
-------------

TODO: list of Common Lisp compilers Djula works on.

Installation
------------

Djula is available on Quicklisp:

.. code-block:: common-lisp

   (ql:quickload :djula)
