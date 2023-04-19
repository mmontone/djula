(asdf:defsystem #:djula-gettext
  :license "MIT"
  :description "Gettext translation backend for Djula."
  :author "Mariano Montone <marianomontone@gmail.com>"
  :depends-on (#:djula #:gettext)
  :components
  ((:module :src
	    :components
            ((:file "translation-gettext")))))
