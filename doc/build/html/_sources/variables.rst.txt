Variables
=========

Variables look like this: ``{{ variable }}``. When the template engine
encounters a variable, it evaluates that variable and replaces it with the
result. Variable names consist of any combination of alphanumeric characters
and the underscore (``"_"``). The dot (``"."``) also appears in variable
sections, although that has a special meaning, as indicated below.
Importantly, *you cannot have spaces or punctuation characters in variable
names.*

Use a dot (``.``) to access attributes of a variable.

.. admonition:: Behind the scenes

    For accessing variables the ``ACCESS`` Common Lisp library is used: https://github.com/AccelerationNet/access

In the above example, ``{{ section.title }}`` will be replaced with the
``title`` attribute of the ``section`` object.

Note that "bar" in a template expression like ``{{ foo.bar }}`` will be
interpreted as a literal string and not using the value of the variable "bar",
if one exists in the template context.

Default template variables
--------------------------

You can use the ``*default-template-arguments*`` variable to store arguments that will be available for all templates. It is a plist, so use getf to add arguments, like this:

.. code-block:: common-lisp

    (setf (getf djula:*default-template-arguments* :foo) 'some-value)

And now, you can access ``{{ foo }}`` in your template.

This is useful when you have many templates that rely on the same set of variables. Use this variable to refactor your code when appropriate.

Note that you could also write a function that wraps ``render-template*`` and uses a default list of variables plus other ones given as arguments:

.. code-block:: common-lisp

    (defun my-render-template (template stream &rest args)
      (apply #'djula:render-template* template stream (list* :foo 'some-value args)))
