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

(defparameter *demos*
  (render-demos
  `(("variables"
     ("{{var}}" :var "foo")
     ("{{var.x}}" :var (:x "baz")))
    ("for"
     ("<ul>{% for x in list %}<li>{{x}}</li>{% endfor %}</ul>"
      :list (list 1 2 3))))))

(hunchentoot:define-easy-handler (demo :uri "/") ()
  (djula:render-template* +demo.html+
			  nil
			  :demos *demos*))

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
