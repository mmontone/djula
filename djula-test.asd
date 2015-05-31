(asdf:defsystem #:djula-test
  :license "MIT"
  :description "Tests for the Djula template system"
  :author "Nick Allen <nallen05@gmail.com>"
  :maintainer ("Eric Sessoms <eric@nubgames.com>"
	       "Mariano Montone <marianomontone@gmail.com>")
  :depends-on (#:djula
               #:fiveam)
  :components
  ((:module :test
	    :components
            ((:file "compiler"  :depends-on ("packages"))
             (:file "filters"   :depends-on ("packages"))
             (:file "lexer"     :depends-on ("packages"))
             (:file "packages")
             (:file "parser"    :depends-on ("packages"))
             (:file "tags"      :depends-on ("packages"))
             (:file "variables" :depends-on ("packages"))
	     (:file "inheritance" :depends-on ("packages")))))
  :perform (asdf:test-op (op c)
			 (uiop:symbol-call :djula-test :run-djula-tests)))
