Usage
=====

To render our templates, they need to be compiled first. We do that with the :cl:function:`COMPILE-TEMPLATE*` function.
For inheritance to work, we need to put all the templates in the same directory so that Djula can find them when resolving templates inheritance.

In the following example we use :cl:symbol:`*TEMPLATE-FOLDER*` as the templates folder, and then several templates are compiled with the :cl:function:`COMPILE-TEMPLATE*` function.

.. code-block:: common-lisp
		
  (defparameter *templates-folder*
    (asdf:system-relative-pathname "webapp" "templates/"))

  (defparameter +base.html+ (djula:compile-template*
                              (princ-to-string
     			        (merge-pathnames "base.html" *templates-folder*))))

  (defparameter +welcome.html+ (djula:compile-template*
			         (princ-to-string
			           (merge-pathnames "welcome.html" *templates-folder*))))

  (defparameter +contact.html+ (djula:compile-template*
			         (princ-to-string
			         (merge-pathnames "contact.html" *templates-folder*))))

Then we can render our compiled templates using the :cl:function:`RENDER-TEMPLATE*` function:

.. code-block:: common-lisp
		
   (djula:render-template* +welcome.html+ s
			      :title "Ukeleles"
			      :project-name "Ukeleles"
			      :mode "welcome")

API
---

.. cl:package:: djula

.. cl:generic:: compile-template

.. cl:function:: compile-template*

.. cl:function:: render-template*		 
