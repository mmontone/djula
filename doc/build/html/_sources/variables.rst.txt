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
