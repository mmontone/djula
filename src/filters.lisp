(in-package #:djula)

(defun apply-filters (value filters)
  (reduce (lambda (value filter)
            (destructuring-bind (name . args)
                filter
              (handler-case
                  (if-let ((fn (find-filter name)))
                    (apply fn value args)
                    (template-error-string "Unknown filter ~A" name))
                (template-error (e1)
                  (princ-to-string e1))
                (error (e)
                  (template-error-string* e "There was an error applying the filter ~A" name)))))
          filters
          :initial-value value))

(def-filter :capfirst (it)
  (string-capitalize (princ-to-string it)))

(def-filter :cut (it charstring)
  (remove-if (lambda (s)
               (find s charstring :test 'char=))
	     (princ-to-string it)))

(def-filter :default (it default)
  (if (zerop (length it)) default it))

(def-filter :force-escape (it)
  (escape-for-html (princ-to-string it)))

(def-filter :length (it)
  (length (if (typep it 'sequence)
              it
              (princ-to-string it))))

(def-filter :sort (it &optional
		      (predicate #'<)
		      (key #'identity))
  (flet ((read-function (arg)
	   (let* ((*package* *djula-execute-package*)
		  (sexp (read-from-string arg)))
	     (coerce sexp 'function))))
    (let ((predicate-f (if (functionp predicate)
			   predicate
			   (read-function predicate)))
	  (key-f (if (functionp key)
		     key
		     (read-function key))))
      (sort (copy-list it) predicate-f :key key-f))))

(def-filter :reverse (it)
  (reverse it))

(def-filter :linebreaks (it)
  (cl-ppcre:regex-replace-all
   "\\n"
   (cl-ppcre:regex-replace-all "(.+)((\\n\\n)|$)" (princ-to-string it) "<p>\\1</p>")
   "<br />"))

(def-filter :linebreaksbr (it)
  (cl-ppcre:regex-replace-all "\\n" (princ-to-string it) "<br />"))

(def-filter :lisp (it &optional lisp-string)
  (unless *eval-lisp-tags*
    (template-error "I can't evaulate the \"lisp\" filter ~A because *EVAL-LISP-TAGS* is NIL" lisp-string))
  (handler-case
      (let* ((*package* *djula-execute-package*)
             (sexp (read-from-string lisp-string))
             (fn (coerce sexp 'function)))
        (funcall fn it))
    (condition (e)
      (template-error "There was an error executing the lisp tag ~S: ~A" lisp-string e))))

(def-filter :add (it n)
  (+ it (parse-integer n)))

(def-filter :first (it)
  (first it))

(def-filter :last (it)
  (car (last it)))

(def-filter :addslashes (it)
  (cl-ppcre:regex-replace-all "'" it "\\\\'"))

(def-filter :format (it fmt)
  (format nil fmt it))

(def-filter :date (it &optional format)
  (let ((timestamp (cond
		     ((integerp it)
		      (local-time:universal-to-timestamp it))
		     ((stringp it)
		      (local-time:universal-to-timestamp (parse-integer it)))
		     (t it))))
    (local-time:format-timestring
     nil
     timestamp
     :format
     (or (and format
	      (let ((read-format (read-from-string format)))
		(if (symbolp read-format)
		    (symbol-value read-format)
		    read-format)))
	 local-time:+iso-8601-date-format+))))

(def-filter :time (it &optional format)
  (let ((timestamp (cond
		     ((integerp it)
		      (local-time:universal-to-timestamp it))
		     ((stringp it)
		      (local-time:universal-to-timestamp (parse-integer it)))
		     (t it))))
    (local-time:format-timestring
     nil
     timestamp
     :format
     (or (and format (read-from-string format))
	 '((:HOUR 2) #\: (:MIN 2) #\: (:SEC 2))))))

(def-filter :datetime (it &optional format)
  (let ((timestamp (cond
		     ((integerp it)
		      (local-time:universal-to-timestamp it))
		     ((stringp it)
		      (local-time:universal-to-timestamp (parse-integer it)))
		     (t it))))
    (local-time:format-timestring
     nil
     timestamp
     :format
     (or (and format (read-from-string format))
	 local-time:+iso-8601-format+))))

(def-filter :join (it sep)
  (join sep (mapcar #'princ-to-string it)))

(def-filter :slice (it &rest slices)
  (apply #'cl-slice:slice it (mapcar #'read-from-string slices)))

(def-filter :lower (it)
  (string-downcase (princ-to-string it)))

;;; CAVEAT: THIS FILTER IS TREATED SPECIALLY BY COMPILE-TOKEN!!!
(def-filter :safe (it)
  it)

;;; this also
(def-filter :escape (it)
  it)

;;; TODO: Seems like an opportune place so adopt INTERACTIVE from Emacs.
(def-filter :truncatechars (it n)
  (if-let ((truncated-string (truncate-characters it (if (stringp n)
                                                         (parse-integer n :junk-allowed t)
                                                         n))))
    truncated-string
    (template-error "filter truncatechars: The elision-string is is larger than the maximum length." )))

(def-filter :upper (it)
  (string-upcase (princ-to-string it)))

(def-filter :urlencode (it)
  (url-encode (princ-to-string it)))

;; Regex filters

(def-filter :replace (it regex)
  (lambda (replace)
    (ppcre:regex-replace-all regex it replace)))

(def-filter :with (it replace)
  (funcall it replace))

(def-filter :scan (it regex)
  (ppcre:scan-to-strings regex it))
