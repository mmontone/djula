(defpackage :djula-demo
  (:use :cl)
  (:export #:start-demo
	   #:stop-demo))

(in-package :djula-demo)

(djula:add-template-directory
 (asdf:system-relative-pathname :djula "test/demo/"))

(defparameter *demo-acceptor* (make-instance 'hunchentoot:easy-acceptor :port 9090))

(push
 (hunchentoot:create-static-file-dispatcher-and-handler
  "/simplegrid.css"
  (asdf:system-relative-pathname :djula "test/demo/simplegrid.css"))
 hunchentoot:*dispatch-table*)

(push
 (hunchentoot:create-static-file-dispatcher-and-handler
  "/styles.css"
  (asdf:system-relative-pathname :djula "test/demo/styles.css"))
 hunchentoot:*dispatch-table*)

(defun start-demo ()
  (hunchentoot:start *demo-acceptor*))

(defun stop-demo ()
  (hunchentoot:stop *demo-acceptor*))

(defparameter +demo.html+ (djula:compile-template* "demo.html"))
(defparameter +error.html+ (djula:compile-template* "error.html"))

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
(let ((djula:*catch-template-errors-p* nil)
	(djula:*fancy-error-template-p* nil))
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
     ("lisp"
      ("{% lisp (+ 2 5) %}"))
     ("length"
      ("{{ list | length }}" :list ,(list 1 2 3)))
     ("filter composition"
      ("{{ text | truncatechars: 10 | upper }}" :text "This is a long text")
      ("{{ text | truncatechars: 10 | cut:This }}" :text "This is great"))
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
     ("first"
      ("{{ list | first }}" :list ,(list 1 2 3)))
     ("last"
      ("{{ list | last }}" :list ,(list 1 2 3)))
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
      ("{{ date | date: djula-demo::+custom-date-format+}}" :date ,(get-universal-time)))))))

(hunchentoot:define-easy-handler (demo :uri "/") ()
  (let ((djula:*catch-template-errors-p* nil)
	(djula:*fancy-error-template-p* nil))
  (djula:render-template* +demo.html+
			  nil
			  :demos *demos*)))

(hunchentoot:define-easy-handler (default-error :uri "/error") ()
  (let ((djula:*catch-template-errors-p* t))
    (djula:render-template* +error.html+)))

(hunchentoot:define-easy-handler (error-not-catched :uri "/error-uncatched") ()
  (let ((djula:*catch-template-errors-p* nil)
	(djula::*fancy-error-template-p* nil))
    (djula:render-template* +error.html+)))

(hunchentoot:define-easy-handler (fancy-error :uri "/fancy-error") ()
  (let ((djula:*catch-template-errors-p* nil)
	(djula::*fancy-error-template-p* t))
    (djula:render-template* +error.html+)))
