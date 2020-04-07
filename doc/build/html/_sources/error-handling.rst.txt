Error handling
==============

Djula catches errors and barfs them to the template output by default.

That is controlled via the :cl:symbol:`*CATCH-TEMPLATE-ERRORS-P*`. If changed to ``NIL``, then errors are not caught anymore and are debuggable from the lisp listener.

Djula provides more or less verbosity in template errors. Verbosity is controlled via the variable :cl:symbol:`*VERBOSE-ERRORS-P*`.

Also, there's a fancy page to display errors, which can be disabled if desired. That is controlled via the variable :cl:symbol:`*FANCY-ERROR-TEMPLATE-P*`


API
---

.. cl:variable:: *catch-template-errors-p*

.. cl:variable:: *fancy-error-template-p*

.. cl:variable:: *verbose-errors-p*   
