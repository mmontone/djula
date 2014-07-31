(in-package #:djula)

(declaim (special *template-arguments*))

(defvar *current-language* nil)

(defvar *djula-execute-package* (find-package :common-lisp-user))

(defvar *default-language* :english)

(defvar *use-example-values-p* nil)

(defvar *catch-template-errors-p* t)

(defvar *verbose-errors-p* t)

(defvar *allow-include-roots* ())

(defvar *template-string-if-invalid* nil)

(defvar *eval-lisp-tags* t)

(defvar *translation-table-regexps*
  '("inter.*\\.lisp$" "inter.*\\.cl$" "inter.*\\.sexp$"))

(defvar *example-table-regexps*
  '("example.*\\.lisp" "example.*\\.cl" "example.*\\.sexp"))

(defvar *known-translation-tables*)

(defvar *known-example-tables*)

(defvar *block-alist* nil)

(defvar *current-block* nil)

(defvar *linked-files*)
