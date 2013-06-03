(asdf:defsystem #:djula
  :description "An implementation of Django templates for Common Lisp."
  :version "0.2"
  :maintainer "Eric Sessoms <eric@nubgames.com>"
  :author "Nick Allen <nallen05@gmail.com>"
  :license "MIT"
  :depends-on (#:access
               #:alexandria
               #:arnesi
               #:babel
	       #:cl-ppcre
	       #:cl-fad
	       #:split-sequence)
  :components
  ((:module :main
            :pathname "src/main/common-lisp"
	    :components
            ((:file "compiler"       :depends-on ("lexer" "parser" "template-store"))
             (:file "conditions"     :depends-on ("specials"))
             (:file "filters"        :depends-on ("pipeline"))
             (:file "lexer"          :depends-on ("pipeline"))
             (:file "packages")
             (:file "parser"         :depends-on ("pipeline"))
             (:file "pipeline"       :depends-on ("conditions"))
             (:file "specials"       :depends-on ("packages"))
             #+nil
             (:file "table"          :depends-on ("pipeline"))
             (:file "tags"           :depends-on ("tag"))
             (:file "tag"            :depends-on ("pipeline"))
             (:file "template-store" :depends-on ("specials"))
             (:file "util"           :depends-on ("packages"))
             (:file "variables"      :depends-on ("specials" "util")))))
  :in-order-to ((test-op (load-op djula-test)))
  :perform (test-op :after (op c)
                    (funcall (intern "RUN-DJULA-TESTS" :djula-test))))
