# Djula

[![Build Status](https://travis-ci.org/mmontone/djula.svg?branch=master)](https://travis-ci.org/mmontone/djula)
[![Quicklisp](http://quickdocs.org/badge/djula.svg)](http://quickdocs.org/djula/)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

Djula is a port of Python's [Django](http://www.djangoproject.com) template engine to Common Lisp. 

## Nutshell


Here a small example of a template::

```HTML+Django

    {% extends "base.html" %}
    {% block title %}Memberlist{% endblock %}
    {% block content %}
      <ul>
      {% for user in users %}
        <li><a href="{{ user.url }}">{{ user.username }}</a></li>
      {% endfor %}
      </ul>
    {% endblock %}
```

## Philosophy

Application logic is for the controller but don't try to make the life
for the template designer too hard by giving him too few functionality.

For more information visit the new [Djula webpage](http://mmontone.github.io/djula) and [documentation](http://mmontone.github.io/djula/doc/build/html/index.html).
