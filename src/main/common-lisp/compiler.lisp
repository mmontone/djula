(in-package #:djula)

(defgeneric compile-template (compiler name)
  (:documentation "Provides a hook to customize template compilation."))

(defclass compiler ()
  ())

(defmethod compile-template ((compiler compiler) name)
  (when-let ((key (find-template* name)))
    (compile-string (fetch-template* key))))

(defclass toplevel-compiler ()
  ((fragment-compiler
    :reader fragment-compiler
    :initarg :fragment-compiler
    :initform (make-instance 'compiler))))

(defmethod compile-template ((compiler toplevel-compiler) name)
  (let ((*block-alist* nil)
        (*linked-files* nil))
    (let ((*current-compiler* (fragment-compiler compiler)))
      (call-next-method))))

(defvar *current-compiler* (make-instance 'toplevel-compiler))

(defun compile-template* (name)
  (compile-template *current-compiler* name))

(defun render-template* (template stream &rest *template-arguments*)
  (cond
    ((or (pathnamep template)
         (stringp template))
     ;; Accept strings and pathnames as template designators.
     (apply #'render-template* (compile-template* template) stream *template-arguments*))
    ((functionp template)
     (let ((*known-translation-tables* nil)
           (*known-example-tables* nil)
           (*accumulated-javascript-strings* nil)
           (*current-language* *current-language*))
       (if stream
           (funcall template stream)
           (with-output-to-string (s)
             (funcall template s)))))))

(defun compile-string (string)
  (let ((fs (mapcar #'compile-token (process-tokens (parse-template-string string)))))
    (lambda (stream)
      (dolist (f fs)
        (funcall f stream)))))

(defun compile-token (token)
  (destructuring-bind (name . args) token
    (let ((compiler (get name 'token-compiler)))
      (if (null compiler)
	  (lambda (stream)
            (princ (template-error-string "Unknown token ~A" name) stream))
	  (handler-case
              ;; Handle compile-time errors.
              (let ((f (apply compiler args)))
                (assert (functionp f)
                        nil
                        "Compiling the token ~A did not return a function"
                        name)
                (lambda (stream)
                  (handler-case
                      ;; Handle run-time errors.
                      (funcall f stream)
                    (template-error (e1)
                      (if *catch-template-errors-p*
                          (princ e1 stream)
                          (error e1)))
                    (error (e2)
                      (let ((msg (template-error-string "There was an error rendering the token ~A" token)))
                        (if *catch-template-errors-p*
                            (princ msg stream)
                            (error 'template-error msg)))))))
            (template-error (e1)
              (if *catch-template-errors-p*
                  (lambda (stream)
                    (princ e1 stream))
                  (error e1)))
            (error (e2)
              (let ((msg (template-error-string "There was an error compiling the token ~A" token)))
                (if *catch-template-errors-p*
                    (lambda (stream)
                      (princ msg stream))
                    (error 'template-error msg)))))))))

(def-token-compiler :string (string)
  ":STRING tokens compile into a function that simply returns the string"
  (lambda (stream)
    (princ string stream)))
