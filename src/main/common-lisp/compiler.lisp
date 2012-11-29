(in-package #:djula)

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

(defun .compile-template-string (string)
  (let ((fs (mapcar #'compile-token (process-tokens (parse-template-string string)))))
    (lambda (stream)
      (dolist (f fs)
        (funcall f stream)))))

(defun compile-template-string (string)
  (let* ((*block-alist* nil)
	 (*linked-files* nil)
	 (fn (.compile-template-string string)))
    (values (lambda (stream &rest *template-arguments* &key &allow-other-keys)
	      (let ((*known-translation-tables* nil)
		    (*known-example-tables* nil)
		    (*accumulated-javascript-strings* nil)
		    (*current-language* *current-language*))
		(if stream
                    (funcall fn stream)
                    (with-output-to-string (s)
                      (funcall fn s)))))
	    *linked-files*)))

(defun compile-template (path)
  (when-let ((key (find-template* path)))
    (compile-template-string (fetch-template* key))))
