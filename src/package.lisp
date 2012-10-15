(in-package #:common-lisp-user)

(defpackage #:djula
  (:use #:anaphora #:cl #:logv #:f-underscore)
  (:export ; lisp api
           :def-template
           :compile-template

	   #:*current-store*
	   #:file-store
	   #:find-template
	   #:find-template*
	   #:fetch-template
	   #:fetch-template*
	   #:compile-template-string

	   #:def-tag-compiler

	   ; runtime options
	   :*current-language*
	   :*default-language*
	   :*catch-template-errors-p*
	   :*allow-include-roots*
	   :*template-string-if-invalid*
	   :*use-example-values-p*
	   :*template-eval*
           :*template-search-path*

	   ; compile-time option
	   :*template-root-folder*

	   ; utils
           :djula-html-escape
	   :djula-url-encode
	   :djula-url-encode-path
	   :djula-url-decode
	   
	   ; filter documentation
	   :get-all-filters
	   :get-filter-documentation

	   ; tag documentation
	   :get-all-tags
	   :get-tag-documentation

	   ; building html documentation
	   :build-html-documentation-from-source

	   ; used by cl-terrace
	   :example-table-p
	   :compile-example-table-p

	   :translation-table-p
	   :compile-translation-table))
