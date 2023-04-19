(asdf:defsystem #:djula-locale
  :license "MIT"
  :description "CL-LOCALE translation backend for Djula."
  :author "Mariano Montone <marianomontone@gmail.com>"
  :depends-on (#:djula #:cl-locale)
  :components
  ((:module :src
	    :components
            ((:file "translation-locale")))))
