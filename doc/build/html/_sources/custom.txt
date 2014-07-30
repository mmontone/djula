.. highlightlang:: html+django

Custom tag and filter libraries
===============================

Certain applications provide custom tag and filter libraries. To access them in
a template, use the :ttag:`load` tag::

    {% load comments %}

    {% comment_form for blogs.entries entry.id with is_public yes %}

In the above, the :ttag:`load` tag loads the ``comments`` tag library, which then
makes the ``comment_form`` tag available for use. Consult the documentation
area in your admin to find the list of custom libraries in your installation.

The :ttag:`load` tag can take multiple library names, separated by spaces.
Example::

    {% load comments i18n %}

See :doc:`/howto/custom-template-tags` for information on writing your own custom
template libraries.

Custom libraries and template inheritance
-----------------------------------------

When you load a custom tag or filter library, the tags/filters are only made
available to the current template -- not any parent or child templates along
the template-inheritance path.

For example, if a template ``foo.html`` has ``{% load comments %}``, a child
template (e.g., one that has ``{% extends "foo.html" %}``) will *not* have
access to the comments template tags and filters. The child template is
responsible for its own ``{% load comments %}``.

This is a feature for the sake of maintainability and sanity.
