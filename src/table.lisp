
(in-package :djula)

; recognizing tables

(let ((scanners (mapcar 'cl-ppcre:create-scanner *example-table-regexps*)))
  (defun example-table-p (path)
    (unless (cl-fad:directory-pathname-p path)
      (let* ((filename        (cl-ffc:filename path))
	     (filename-length (length filename)))
	(some (f_ (funcall _ filename 0 filename-length))
	      scanners)))))

(let ((scanners (mapcar 'cl-ppcre:create-scanner *translation-table-regexps*)))
  (defun translation-table-p (path)
    (unless (cl-fad:directory-pathname-p path)
      (let* ((filename        (cl-ffc:filename path))
	     (filename-length (length filename)))
	(some (f_ (funcall _ filename 0 filename-length))
	      scanners)))))

; compile example/translation tables

(defun .read-table-string (unparsed-string)
  (let (*read-eval*
	(*package* (find-package :keyword)))
    (values (read-from-string (format nil "(~A)" unparsed-string)))))

(defun .read-table (path)
  (.read-table-string (fetch-template* path)))

(defun .compile-translation-table-variable (list)
  (destructuring-bind (variable &rest language-plist &key &allow-other-keys)
      list
    (cons variable
	  (labels ((rfn (plist)
		     (when plist
		       (destructuring-bind (k v . rest) plist
			 (list* k
				(with-template-error (f0 (template-error-string "There was an error compiling the translation table variable ~A"
										k))
				  (.compile-template-string v))
				(rfn rest))))))
	    (rfn language-plist)))))

(defun .compile-translation-table-alist (alist)
  (mapcar '.compile-translation-table-variable alist))

(defun compile-translation-table (path)
  (.compile-translation-table-alist (.read-table path)))

(defun compile-example-table (path)
  (.read-table path))

; getting variables from within tables

(defun .get-translation-from-translation-table-alist (variable alist language)
  (let ((a (assoc variable alist)))
    (if a
	(let ((language-plist (rest a)))
	  (multiple-value-bind (fn present-p) (.getf language-plist language)
	    (if present-p
		(values (funcall fn) present-p)))))))

(defun .get-translation (variable)
  (dolist (d *known-translation-tables*)
    (destructuring-bind (path . alist) d
      (multiple-value-bind (v present-p)
	  (.get-translation-from-translation-table-alist variable
							 (if *template-ffc*
							     (cl-ffc:ffc-call *template-ffc*
									      path
									      *current-template-root-folder*)
							     alist)
							 (or *current-language*
							     *default-language*))
	(if present-p
	    (return-from .get-translation (values v present-p)))))))

; reading :UNPARSED-TRANSLATION-VARIABLE TOKENS created by {_ translation-variable _}

(def-token-processor :unparsed-translation-variable (unparsed-string) rest
  `((:translation-variable ,(let ((*package* (find-package :keyword))
				  *read-eval*)
				 (read-from-string unparsed-string)))
    ,@(process-tokens rest)))

; compiling :TRANSLATION-VARIABLE tokens

(def-token-compiler :translation-variable (name)

  ;; return a function that finds the value of the translation variable
  (f0 (with-template-error (template-error-string "There was an error while looking up the translation of ~A"
						  name)
	(multiple-value-bind (v present-p) (.get-translation name)
	  (if present-p
	      v
	      (or *template-string-if-invalid*
		  (template-error-string "invalid translation variable ~A" name)))))))
