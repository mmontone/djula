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

    {% extends "base.html" %}

    {% block title %}{{ section.title }}{% endblock %}

    {% block content %}
    <h1>{{ section.title }}</h1>

    {% for story in story_list %}
    <h2>
      <a href="{{ story.url }}">
        {{ story.headline|upper }}
      </a>
    </h2>
    <p>{{ story.tease|truncatewords:100 }}</p>
    {% endfor %}
    {% endblock %}
