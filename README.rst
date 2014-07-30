Djula
~~~~~~

Djula is a port of Python's `Django`_ template engine to Common Lisp. 

Nutshell
--------

Here a small example of a template::

    {% extends "base.html" %}
    {% block title %}Memberlist{% endblock %}
    {% block content %}
      <ul>
      {% for user in users %}
        <li><a href="{{ user.url }}">{{ user.username }}</a></li>
      {% endfor %}
      </ul>
    {% endblock %}

Philosophy
----------

Application logic is for the controller but don't try to make the life
for the template designer too hard by giving him too few functionality.

For more information visit the new `Djula webpage`_ and `documentation`_.

.. _Django: http://www.djangoproject.com/
.. _Djula webpage: http://mmontone.github.io/djula
.. _documentation: http://mmontone.github.io/djula/doc/build/html/index.html
