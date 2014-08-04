(defpackage :djula-demo
  (:use :cl)
  (:export #:start-demo
	   #:stop-demo))

(in-package :djula-demo)

(djula:add-template-directory
 (asdf:system-relative-pathname :djula "demo/"))

(defparameter *demo-acceptor* (make-instance 'hunchentoot:easy-acceptor :port 9090))

(push
 (hunchentoot:create-static-file-dispatcher-and-handler
  "/simplegrid.css"
  (asdf:system-relative-pathname :djula "demo/simplegrid.css"))
 hunchentoot:*dispatch-table*)

(push
 (hunchentoot:create-static-file-dispatcher-and-handler
  "/styles.css"
  (asdf:system-relative-pathname :djula "demo/styles.css"))
 hunchentoot:*dispatch-table*)

(defun start-demo ()
  (hunchentoot:start *demo-acceptor*))

(defun stop-demo ()
  (hunchentoot:stop *demo-acceptor*))

(defparameter +demo.html+ (djula:compile-template* "demo.html"))
(defparameter +error.html+ (djula:compile-template* "error.html"))
(defparameter +debug.html+ (djula:compile-template* "debug.html"))

(defun render-demos (demos)
  (loop for demo in demos
     collect (list :title (first demo)
		   :examples
		   (loop for example in (cdr demo)
		      collect
			(destructuring-bind (source &rest args) example
			  (list :source source
				:args (format nil "~S" args)
				:output (apply
					 #'djula:render-template*
					 (djula::compile-string source)
					 nil
					 args)))))))

(defparameter +custom-date-format+ '((:YEAR 4) #\/ (:MONTH 2) #\/ (:DAY 2)))

(defparameter *demos*
  (render-demos
   `(("variables"
      ("{{var}}" :var ,"foo")
      ("{{var.x}}" :var (:x ,"baz")))
     ("if"
      ("{% if foo %}yes{% else %}no{% endif %}"
       :foo ,t)
      ("{% if foo %}yes{% else %}no{% endif %}"
       :foo ,nil)
      ("{% ifequal foo bar %}yes{% else %}no{% endifequal %}"
       :foo ,"foo" :bar ,"bar")
      ("{% ifequal foo bar %}yes{% else %}no{% endifequal %}"
       :foo ,"foo" :bar ,"foo"))
     ("for"
      ("<ul>{% for x in list %}<li>{{x}}</li>{% endfor %}</ul>"
       :list ,(list 1 2 3)))
     ("cycle"
      ("{% for x in list %}
        <tr class=\"{% cycle \"row1\" \"row2\" %}\">
           <td>{{x}}</td> 
        </tr>
        {% endfor %}" :list ,(list 1 2 3))
      ("{% for x in list %}
        <tr class=\"{% cycle row1 row2 %}\">
           <td>{{x}}</td> 
        </tr>
        {% endfor %}" :list ,(list 1 2 3) :row1 "r1" :row2 "r2"))
     ("filter"
      ("{% filter force-escape|lower %}
           This text will be <b>HTML</b>-escaped, and will <p>appear</p> in all lowercase.
        {% endfilter %}"))
     ("firstof"
      ("{% firstof var1 var2 var3 %}" :var2 "x")
      ("{% firstof var1 var2 var3 %}" :var3 "x")
      ("{% firstof var1 var2 var3 \"fallback\" %}"))
     ("ifchanged"
      ("{% for x in list %}
          {% ifchanged x %}changed{% endifchanged %}
          {{x}}
        {% endfor %}" :list ,(list 1 1 2 2 2 3)))
     ("lisp"
      ("{% lisp (+ 2 5) %}"))
     ("length"
      ("{{ list | length }}" :list ,(list 1 2 3)))
     ("filter composition"
      ("{{ text | truncatechars: 10 | upper }}" :text "This is a long text")
      ("{{ text | truncatechars: 10 | cut:This }}" :text "This is great"))
     ("format"
      ("{{ value | format:\"~:d\" }}" :value 1000000))
     ("cut"
      ("{{ text | cut: IT }}" :text "cutITout"))
     ("default"
      ("{{ text | default: hello!! }}" :text ,nil))
     ("lower"
      ("{{ text | lower }}" :text ,"Hello"))
     ("upper"
      ("{{ text | upper }}" :text ,"Hello"))
     ("capfirst"
      ("{{ text | capfirst }}" :text ,"hello"))
     ("join"
      ("{{ list | join:\",\"}}" :list ,(list 1 2 3))
      ("{{ list | join:\" // \"}}" :list ,(list 1 2 3)))
     ("slice"
      ("{{ list | slice: 4 }}" :list ,(list 1 2 3 4 5 6))
      ("{{ list | slice: (2 . 4) }}" :list ,(list 1 2 3 4 5 6))
      ("{{ string | slice: 3 }}" :string "Hello world")
      ("{{ string | slice: -5 }}" :string "Hello world")
      ("{{ string | slice: (3 . 5) }}" :string "Hello world")
      ("{{ string | slice: (5 . nil) }}" :string "Hello world")
      ("{{ string | slice: (0 . 5) }}" :string "Hello world"))
     ("first"
      ("{{ list | first }}" :list ,(list 1 2 3)))
     ("last"
      ("{{ list | last }}" :list ,(list 1 2 3)))
     ("sort"
      ("{{ list | sort }}" :list ,(list 1 3 2 5))
      ("{{ list | sort: > }}" :list ,(list 1 3 2 5)))
     ("reverse"
      ("{{ list | reverse }}" :list ,(list 1 2 3 4)))
     ("add"
      ("{{ n | add: 4 }}" :n ,1))
     ("truncatechars"
      ("{{ text | truncatechars: 10 }}" :text "This is a long text")) 
     ("lisp filter"
      ("{{ text | lisp: string-upcase }}" :text ,"hello")
      ("{{ num | lisp: 1+}}" :num ,0))
     ("safe"
      ("{{ html | safe }}" :html ,"<p>Hello</p>"))
     ("date"
      ("{{ date | date }}" :date ,(get-universal-time))
      ("{{ date | date: djula-demo::+custom-date-format+}}" :date ,(get-universal-time)))
     ("translation"
      ("{_ hello _}" :hello "hello")
      ("{% set-language :es %}{_ hello _}" :hello "hello")
      ("{_ \"hello\" _}")
      ("{% set-language :es %}{_ \"hello\"_}")
      ("{% trans hello %}" :hello "hello")
      ("{% set-language :es %}{% trans hello %}" :hello "hello")
      ("{% trans \"hello\" %}")
      ("{% set-language :es %}{% trans \"hello\" %}")
      ("{{ hello | trans }}" :hello "hello")
      ("{% set-language :es %}{{ hello | trans }}" :hello "hello"))
     ("debug"
      ("{% debug %}")))))

(hunchentoot:define-easy-handler (demo :uri "/") ()
  (let ((djula:*catch-template-errors-p* nil)
	(djula:*fancy-error-template-p* nil))
  (djula:render-template* +demo.html+
			  nil
			  :demos *demos*)))

(hunchentoot:define-easy-handler (default-error :uri "/error") ()
  (let ((djula:*catch-template-errors-p* t)
	(djula:*fancy-error-template-p* nil))
    (djula:render-template* +error.html+)))

(hunchentoot:define-easy-handler (error-not-catched :uri "/error-uncatched") ()
  (let ((djula:*catch-template-errors-p* nil)
	(djula::*fancy-error-template-p* nil))
    (djula:render-template* +error.html+)))

(hunchentoot:define-easy-handler (fancy-error :uri "/fancy-error") ()
  (let ((djula:*catch-template-errors-p* t)
	(djula::*fancy-error-template-p* t))
    (djula:render-template* +error.html+)))

(hunchentoot:define-easy-handler (debug-demo :uri "/debug") ()
  (djula:render-template* +debug.html+))

(defparameter +translation.html+ (djula:compile-template* "translation.html"))

(hunchentoot:define-easy-handler (locale-demo :uri "/locale")
    (lang)
  (let ((lang-key (intern (string-upcase lang) :keyword)))
    (let ((djula:*current-language* lang-key)
	  (djula::*translation-backend* :locale))
      (djula:render-template* +translation.html+))))

(hunchentoot:define-easy-handler (gettext-demo :uri "/gettext")
    (lang)
  (let ((djula:*current-language* lang)
	(djula::*translation-backend* :gettext))
      (djula:render-template* +translation.html+)))

(cl-locale:define-dictionary demo
  (:en (asdf:system-relative-pathname :djula-demo "demo/i18n/en/message.lisp"))
  (:es (asdf:system-relative-pathname :djula-demo "demo/i18n/es/message.lisp")))

(gettext:setup-gettext #:djula-demo "demo")
(gettext:preload-catalogs #.(asdf:system-relative-pathname :djula-demo "demo/locale/"))
;(setf (gettext:textdomaindir "demo")
;      (asdf:system-relative-pathname :djula-demo "demo/locale/"))
(setf djula::*gettext-domain* "demo")
