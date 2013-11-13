(in-package #:djula)

(define-condition template-error (error)
  ((message
    :reader template-error-message
    :initarg :message))
  (:report
   (lambda (e stream)
     (princ (template-error-message e) stream))))

(defun template-error-string (fmt &rest args)
  (format nil "{# Error: ~? #}" fmt args))

(defun template-error (msg &rest args)
  (error 'template-error
         :message (if args
                      (apply #'template-error-string msg args)
                      msg)))

(defmacro with-template-error (recovery-form &body body)
  (with-unique-names (e)
    `(handler-case
         (progn
           ,@body)
       (error (,e)
         (if *catch-template-errors-p*
             ,recovery-form
             (error ,e))))))
