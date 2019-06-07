(in-package #:common-lisp-user)

(defpackage #:djula
  (:use #:iterate
        #:alexandria
        #:anaphora
        #:common-lisp
        #:parser-combinators)
  (:export #:*allow-include-roots*
           #:*auto-escape*
           #:*catch-template-errors-p*
           #:*verbose-errors-p*
           #:*fancy-error-template-p*
           #:*fancy-debug-p*
           #:*current-compiler*
           #:*current-language*
           #:*current-store*
           #:*default-language*
           #:*error-template*
           #:*elision-string*
           #:*template-eval*
           #:*template-root-folder*
           #:*template-search-path*
           #:*djula-execute-package*
           #:compile-template
           #:compile-template*
           #:compiler
           #:def-tag-compiler
           #:def-filter
           #:fetch-template
           #:fetch-template*
           #:file-store
           #:find-template
           #:find-template*
           #:fragment-compiler
           #:html-escape
           #:render-template*
           #:toplevel-compiler
           #:url-encode
           #:url-encode-path
           #:add-template-directory
           #:translate
           #:*translation-backend*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "DJULA.TOKEN-PROCESSORS")
    (defpackage "DJULA.TOKEN-PROCESSORS"
      (:use)
      (:documentation "Contains the token processors")
      (:export #:comment-tag
               #:almost-parsed-ifequal
               #:almost-parsed-ifnotequal
               #:comment
               #:unparsed-variable
               #:semi-parsed-js-script
               #:unparsed-translation
               #:tag
               #:semi-parsed-if
               #:string
               #:unparsed-tag)))

  (unless (find-package "DJULA.UNPARSED-TAG-PROCESSORS")
    (defpackage "DJULA.UNPARSED-TAG-PROCESSORS"
      (:use)
      (:documentation "This package contains the unparsed tag processors.")
      (:export #:filter
               #:set
               #:trans
               #:lisp
               #:ifequal
               #:ifnotequal
               #:js)))

  (unless (find-package "DJULA.TAG-PROCESSORS")
    (defpackage "DJULA.TAG-PROCESSORS"
      (:use)
      (:documentation "This package contains the tag processors.")
      (:export #:semi-parsed-ifequal
               #:semi-parsed-filter
               #:js-script
               #:comment
               #:block
               #:autoescape
               #:for
               #:ifchanged
               #:if
               #:semi-parsed-ifnotequal
               #:extends)))

  (unless (find-package "DJULA.TOKEN-COMPILERS")
    (defpackage "DJULA.TOKEN-COMPILERS"
      (:use)
      (:documentation "This package contains the token compilers.")
      (:export #:parsed-js-script
               #:tag
               #:string
               #:parsed-filter
               #:verbatim
               #:translation
               #:parsed-if
               #:parsed-block
               #:parsed-js
               #:parsed-ifchanged
               #:parsed-ifequal
               #:parsed-lisp
               #:variable
               #:parsed-autoescape
               #:parsed-set
               #:parsed-for)))

  (unless (find-package "DJULA.TAG-COMPILERS")
    (defpackage "DJULA.TAG-COMPILERS"
      (:use)
      (:documentation "This package contains the tag compilers.")
      (:export #:set-package
               #:endifchanged
               #:show-language
               #:endautoescape
               #:endifnotequal
               #:cycle
               #:firstof
               #:endcomment
               #:endfor
               #:set-language
               #:emit-js
               #:endifequal
               #:templatetag
               #:endjs-script
               #:super
               #:ssi
               #:endblock
               #:endfilter
               #:endif
               #:include
               #:debug)))

  (unless (find-package "DJULA.FILTERS")
    (defpackage "DJULA.FILTERS"
      (:use)
      (:documentation "This package contains the djula filters. Filters are take as
  a first argument a string and return a string.")
      (:export #:date
               #:force-escape
               #:datetime
               #:trans
               #:safe
               #:escape
               #:linebreaksbr
               #:last
               #:truncatechars
               #:addslashes
               #:time
               #:lisp
               #:capfirst
               #:urlencode
               #:sort
               #:linebreaks
               #:length
               #:upper
               #:slice
               #:lower
               #:format
               #:first
               #:cut
               #:add
               #:default
               #:reverse
               #:join)))
  (unless (find-package "DJULA.LOCALE")
    (defpackage "DJULA.LOCALE"
      (:use :cl)
      (:documentation "This package contains code to generate cl-locale dictionary files.")
      (:export #:update-project
               #:update-caveman-project
               #:locale-list
               #:update-locale-list
               #:directory-translate-strings
               #:file-template-translate-strings))))

