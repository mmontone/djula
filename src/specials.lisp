(in-package #:djula)

(defvar *template-arguments*)

(defvar *default-template-arguments* nil
  "List of arguments available for all templates. Add arguments to this list if you want them to be available in every template.
This is a plist, so use getf to add arguments, like: (setf (getf djula:*default-template-arguments* :foo) 'some-value)")

(defvar *current-language* nil)

(defvar *template-package* (find-package :common-lisp-user)
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

(defvar *recompile-templates-on-change* t
  "When enabled, templates are recompiled if they have changed on disk, before they are rendered.
This is the default and convenient for development. For production, this can be turned off and the filesystem check is bypassed.")

(defvar *block-alist* nil)

(defvar *current-block* nil)

(defvar *linked-templates*)

(defvar *current-template*)

(defvar *error-template* (asdf:system-relative-pathname :djula "templates/error-template.djhtml")
  "The error template used by `render-error-template'.")

(defvar *elision-string* "..."
  "The string to be used by `truncatechars' at the end of truncated strings.")

(defvar *accumulated-javascript-strings* nil)

;; Type declarations

;; (declaim
;;  (ftype (function ((or string pathname)) string) fetch-template*)
;;  (ftype (function (compiler (or string pathname) &optional boolean) *) compile-template)
;;  (ftype (function ((or string pathname)) *) compile-template*)
;;  (ftype (function (template-store (or string pathname) &optional boolean) (or null string pathname)) find-template)
;;  (ftype (function (pathname &optional template-store) *) add-template-directory)
;;  (ftype (function (template-store (or string pathname)) (or string null)) fetch-template)
;;  (ftype (function (pathname) string) url-encode-path)
;;  (ftype (function ((or pathname function string) &optional (or null stream) &rest t) *) render-template*)
;;  (ftype (function (string) string) url-encode)
;;  (ftype (function (string &optional list symbol symbol) (or string null)) translate)
;;  (ftype (function ((or string pathname) &optional boolean) (or string null)) find-template*))
