(in-package #:djula)

(defvar *template-arguments*)

(defvar *default-template-arguments* nil
  "List of arguments available for all templates. Add arguments to this list if you want them to be available in every template.
This is a plist, so use getf to add arguments, like: (setf (getf djula:*default-template-arguments* :foo) 'some-value)")

(defvar *current-language* nil)

(defvar *djula-execute-package* (find-package :common-lisp-user))

(defvar *default-language* :en)

(defvar *catch-template-errors-p* t "When enabled, caught errors during the rendering of the template are written to the output instead of being handled by the lisp listener")

(defvar *verbose-errors-p* t "When enabled, errors are displayed more verbosely. Good for debugging")

(defvar *fancy-error-template-p* t "When enabled, show a fancy template when an error ocurrs")

(defvar *fancy-debug-p* t "When enabled, displays fancy html based debugging information for the {% debug %} tag")

(defvar *allow-include-roots* ())

(defvar *eval-lisp-tags* t)

(defvar *block-alist* nil)

(defvar *current-block* nil)

(defvar *linked-templates*)

(defvar *current-template*)

(defvar *error-template* (asdf:system-relative-pathname :djula "templates/error-template.djhtml")
  "The error template used by `render-error-template'.")

(defvar *elision-string* "..."
  "The string to be used by `truncatechars' at the end of truncated strings.")

(defvar *accumulated-javascript-strings* nil)
