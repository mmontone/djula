(asdf:defsystem #:djula-demo
  :license "MIT"
  :depends-on (#:djula
               #:hunchentoot)
  :components
  ((:module :demo
	    :components
            ((:file "demo")))))
