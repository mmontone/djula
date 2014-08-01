(asdf:defsystem #:djula-demo
  :license "MIT"
  :depends-on (#:djula
               #:hunchentoot)
  :components
  ((:module :demo
	    :pathname "test/demo"
	    :components
            ((:file "demo")))))
