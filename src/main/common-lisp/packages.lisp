(in-package #:common-lisp-user)

(defpackage #:djula
  (:use #:access
        #:alexandria
        #:common-lisp)
  (:import-from #:arnesi
                #:aand
                #:acond
                #:aif
                #:it)
  (:export #:*allow-include-roots*
           #:*catch-template-errors-p*
           #:*current-language*
           #:*current-store*
           #:*default-language*
           #:*template-eval*
           #:*template-root-folder*
           #:*template-search-path*
           #:*template-string-if-invalid*
           #:*use-example-values-p*
           #:compile-example-table-p
           #:compile-template
           #:compile-template-string
           #:compile-translation-table
           #:def-tag-compiler
           #:example-table-p
           #:fetch-template
           #:fetch-template*
           #:file-store
           #:find-template
           #:find-template*
           #:html-escape
           #:translation-table-p
           #:url-decode
           #:url-encode
           #:url-encode-path))
