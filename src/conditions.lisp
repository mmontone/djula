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
         (if (and *catch-template-errors-p*
		  (not *fancy-error-template-p*))
             ,recovery-form
             (error ,e))))))

(defun render-error-template (error backtrace &optional template stream)
  "Render the *ERROR-TEMPLATE* with the ERROR, the BACKTRACE and the TEMPLATE
where the error ocurred."
  (let ((error-template (compile-template* *error-template*)))
    (djula:render-template* error-template stream
                            :error error
                            :error-backtrace backtrace
                            :template template)))
