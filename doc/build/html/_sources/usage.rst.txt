Usage
=====

To render our templates, they need to be compiled first. We do that with the :cl:function:`COMPILE-TEMPLATE*` function.
For inheritance to work, we need to put all the templates in the same directory so that Djula can find them when resolving templates inheritance.

Djula looks for templates in the :cl:symbol:`*CURRENT-STORE*`. For our templates to be found, we have to add the template folder path the templates store. We can do that with the :cl:function:`add-template-directory` function.

Here is an example:
  
.. code-block:: common-lisp
		
  (add-template-directory (asdf:system-relative-pathname "webapp" "templates/"))

  (defparameter +base.html+ (djula:compile-template* "base.html"))

  (defparameter +welcome.html+ (djula:compile-template* "welcome.html"))

  (defparameter +contact.html+ (djula:compile-template* "contact.html"))

Then we can render our compiled templates using the :cl:function:`RENDER-TEMPLATE*` function:

.. code-block:: common-lisp
		
   (djula:render-template* +welcome.html+ s
			      :title "Ukeleles"
			      :project-name "Ukeleles"
			      :mode "welcome")

API
---

.. cl:package:: djula

.. cl:function:: add-template-directory		

.. cl:generic:: compile-template

.. cl:function:: compile-template*

.. cl:function:: render-template*		 
