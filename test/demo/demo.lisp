(defpackage :djula-demo
  (:use :cl)
  (:export #:start-demo
	   #:stop-demo))

(in-package :djula-demo)

(djula:add-template-directory
 (asdf:system-relative-pathname :djula "test/demo/"))

(defparameter *demo-acceptor* (make-instance 'hunchentoot:easy-acceptor :port 9090))

(defun start-demo ()
  (hunchentoot:start *demo-acceptor*))

(defun stop-demo ()
  (hunchentoot:stop *demo-acceptor*))

(defparameter +demo.html+ (djula:compile-template* "demo.html"))
(defparameter +error.html+ (djula:compile-template* "error.html"))

(hunchentoot:define-easy-handler (demo :uri "/") ()
  (djula:render-template* +demo.html+))

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
