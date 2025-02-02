(asdf:defsystem #:djula
  :description "An implementation of Django templates for Common Lisp."
  :version "0.2"
  :maintainer ("Eric Sessoms <eric@nubgames.com>"
               "Mariano Montone <marianomontone@gmail.com>")
  :author "Nick Allen <nallen05@gmail.com>"
  :license "MIT"
  :homepage "http://mmontone.github.io/djula"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :depends-on (#:access
               #:alexandria
               #:babel
               #:cl-ppcre
               #:split-sequence
               #:local-time
               #:closer-mop
               #:trivial-backtrace
               #:cl-slice
               #:cl-locale
               #:gettext
               #:parser-combinators
               #:iterate
               #:uiop)
  :components
  ((:module :src
    :components
    ((:file "pathnames")
     (:file "compiler"       :depends-on ("lexer" "parser" "template-store"))
     (:file "conditions"     :depends-on ("specials"))
     (:file "filters"        :depends-on ("pipeline"))
     (:file "lexer"          :depends-on ("pipeline" "util"))
     (:file "locale"         :depends-on ("lexer"))
     (:file "packages")
     (:file "parser"         :depends-on ("pipeline"))
     (:file "pipeline"       :depends-on ("conditions"))
     (:file "specials"       :depends-on ("packages"))
     (:file "static"         :depends-on ("packages"))
     (:file "tags"           :depends-on ("tag" "variables"))
     (:file "tag"            :depends-on ("pipeline"))
     (:file "template-store" :depends-on ("specials"))
     (:file "translation"    :depends-on ("specials" "pipeline"))
     (:file "util"           :depends-on ("packages"))
     (:file "variables"      :depends-on ("specials" "util")))))
  :in-order-to ((asdf:test-op (asdf:test-op :djula-test))))
