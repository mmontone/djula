(asdf:defsystem #:djula-translate
  :license "MIT"
  :description "Translation backend for Djula using TRANSLATE library."
  :author "Mariano Montone <marianomontone@gmail.com>"
  :depends-on (#:djula #:translate)
  :components
  ((:module :src
	    :components
            ((:file "translation-translate")))))
