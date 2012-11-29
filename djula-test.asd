(asdf:defsystem #:djula-test
  :license "MIT"
  :depends-on (#:djula
               #:fiveam)
  :components
  ((:module :test
            :pathname "src/test/common-lisp"
            :components
            ((:file "compiler"  :depends-on ("packages"))
             (:file "filters"   :depends-on ("packages"))
             (:file "lexer"     :depends-on ("packages"))
             (:file "packages")
             (:file "parser"    :depends-on ("packages"))
             (:file "tags"      :depends-on ("packages"))
             (:file "variables" :depends-on ("packages"))))))
