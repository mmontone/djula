.. highlightlang:: html+django
		   
Basics
======

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
