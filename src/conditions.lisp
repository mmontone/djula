(in-package #:djula)

(define-condition template-error (error)
  ((message
    :reader template-error-message
    :initarg :message
    :initform (error "Provide the error message")))
  (:report
   (lambda (e stream)
     (princ (template-error-message e) stream))))

(defun template-error-string (fmt &rest args)
  (format nil "{# Error: ~? #}" fmt args))

(defun template-error-string* (error fmt &rest args)
  (if *verbose-errors-p*
      (format nil "{# Error: ~A : ~A #}"
	      (apply #'format nil fmt args)
	      error)
      (apply #'template-error-string fmt args)))

(defun template-error (msg &rest args)
  (error 'template-error
         :message (if args
                      (apply #'template-error-string msg args)
                      msg)))

(defun template-error* (error msg &rest args)
  (if *verbose-errors-p*
      (error 'template-error
	     :message (format nil "~A: ~A"
			      (if args
				  (apply #'template-error-string args)
				  msg)
			      error))
      (apply #'template-error msg args)))	 

(defmacro with-template-error (recovery-form &body body)
  (with-unique-names (e)
    `(handler-case
         (progn
           ,@body)
       (error (,e)
         (if *catch-template-errors-p*
             ,recovery-form
             (error ,e))))))

(defun render-error-template (error backtrace &optional template stream)
  (let ((error-template (djula::compile-string
			 "<html>
   <head>
       <font face='arial'>
       <style type='text/css'>
           body {
               margin: 0px;
               background: #ececec;
           }
           #header {
               padding-top:  5px;
               padding-left: 25px;
               color: white;
               background: #a32306;
               text-shadow: 1px 1px #33333;
               border-bottom: 1px solid #710000;
           }
           #error-wrap {
               border-top: 5px solid #d46e6b;
           }
           #error-message {
               color: #800000;
               background: #f5a29f;
               padding: 10px;
               text-shadow: 1px 1px #FFBBBB;
               border-top: 1px solid #f4b1ae;
           }
           #file-wrap {
               border-top: 5px solid #2a2a2a;
           }
           #file {
               color: white;
               background: #333333;
               padding: 10px;
               padding-left: 20px;
               text-shadow: 1px 1px #555555;
               border-top: 1px solid #444444;
           }
           #line-number {
               width=20px;
               color: #8b8b8b;
               background: #d6d6d6;
               float: left;
               padding: 5px;
               text-shadow: 1px 1px #EEEEEE;
               border-right: 1px solid #b6b6b6;
           }
           #line-content {
               float: left;
               padding: 5px;
           }
           #error-content {
               float: left;
               width: 100%;
               border-top: 5px solid #cdcdcd;
               border-bottom: 5px solid #cdcdcd;
           }
       </style>
   </head>
   <body>
       <div id='header'>
           <h1>Template Compilation Error</h1>
       </div>
       <div id='error-wrap'>
           <div id='error-message'>{{error}}.</div>
       </div>
       {% if template %}
         <div id='file-wrap'>
             <div id='file'>In {{template}}{% if line %} on line {{line}}{% endif %}.</div>
         </div>
         <div id='error-content'>
           {{error-backtrace | linebreaksbr }}
         </div>
       {% endif %}
   </body>
</html>
")))
    (djula:render-template* error-template stream
			    :error error
			    :error-backtrace backtrace
			    :template template)))
