.. highlightlang:: html+django
		   
Internationalization
====================

Syntax
------

The easiest way to translate a string or variable is to enclose it between ``{_`` and ``_}``::

  {_ var _}
  {_ "hello" _}


Tags
----

.. templatetag:: trans

trans
^^^^^

Translates a variable or string

Example::

  {% trans var %}
  {% trans "hello" %}


Filters
-------

.. templatefilter:: urlencode

trans
^^^^^

Translates a variable or string.

For example::

    {{ var | trans }}
    {{ "my string" |  trans }}


Choosing language
-----------------

To choose the language to use, set the :cl:symbol:`*CURRENT-LANGUAGE*` variable.

For example:

.. code-block:: common-lisp
		
	(let ((djula:*current-language* :es))
	   (djula:render-template* +translation.html+))
      
Backends
--------

Djula supports two backends for doing translations: `cl-locale <https://github.com/arielnetworks/cl-locale>`_ and `gettext <https://github.com/copyleft/gettext>`_

Please have a look at the demo and the documentation of those packages to figure out how to use them.
