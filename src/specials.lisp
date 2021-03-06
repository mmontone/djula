(in-package #:djula)

(defvar *template-arguments*)

(defvar *default-template-arguments* nil
  "List of arguments available for all templates. Add arguments to this list if you want them to be available in every template.
This is a plist, so use getf to add arguments, like: (setf (getf djula:*default-template-arguments* :foo) 'some-value)")

(defvar *current-language* nil)

(defvar *djula-execute-package* (find-package :common-lisp-user)
  "The package in which template expressions are evaluated.")

(defvar *default-language* :en
  "The default i18n language. English is the default.")

(defvar *catch-template-errors-p* t "When enabled, caught errors during the rendering of the template are written to the output instead of being handled by the lisp listener.")

(defvar *verbose-errors-p* t "When enabled, errors are displayed more verbosely. Good for debugging.")

(defvar *fancy-error-template-p* t "When enabled, show a fancy template when an error ocurrs.")

(defvar *fancy-debug-p* t "When enabled, displays fancy html based debugging information for the {% debug %} tag.")

(defvar *allow-include-roots* ()
  "A list of folder names that are allowed to be included by SSI.")

(defvar *eval-lisp-tags* t
  "Enable/disable Lisp evaluation in templates.")

(defvar *block-alist* nil)

(defvar *current-block* nil)

(defvar *linked-templates*)

(defvar *current-template*)

(defvar *error-template* (asdf:system-relative-pathname :djula "templates/error-template.djhtml")
  "The error template used by `render-error-template'.")

(defvar *elision-string* "..."
  "The string to be used by `truncatechars' at the end of truncated strings.")

(defvar *accumulated-javascript-strings* nil)
