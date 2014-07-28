Basics
======

A template is a text document that is marked-up using Djula template language. A template can contain block tags or variables.

A block tag is a symbol within a template that does something.

This definition is deliberately vague. For example, a block tag can output content, serve as a control structure (an “if” statement or “for” loop), grab content from a database or enable access to other template tags.

Block tags are surrounded by "{%" and "%}".

Example template with block tags:

{% if is_logged_in %}Thanks for logging in!{% else %}Please log in.{% endif %}

A variable is a symbol within a template that outputs a value.

Variable tags are surrounded by "{{" and "}}".

Example template with variables:

My first name is {{ first_name }}. My last name is {{ last_name }}.

A context is a “variable name” -> “variable value” mapping that is passed to a template.

A template renders a context by replacing the variable “holes” with values from the context and executing all block tags.
