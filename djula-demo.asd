(asdf:defsystem #:djula-demo
  :license "MIT"
  :description "Demo of the Djula template system"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :depends-on (#:djula
               #:hunchentoot)
  :components
  ((:module :demo
	    :components
            ((:file "demo")))))
