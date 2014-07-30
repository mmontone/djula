(in-package #:djula)

(defun apply-filters (string filters)
  (reduce (lambda (string filter)
            (destructuring-bind (name . args)
                filter
              (handler-case
                  (if-let ((fn (get name 'filter)))
                    (apply fn string args)
                    (template-error-string "Unknown filter ~A" name))
                (template-error (e1)
                  (princ-to-string e1))
                (error ()
                  (template-error-string "There was an error applying the filter ~A" name)))))
          filters
          :initial-value string))

(def-filter :capfirst (it)
  (string-capitalize (princ-to-string it)))

(def-filter :cut (it charstring)
  (remove-if (lambda (s)
               (find s charstring :test 'char=))
	     (princ-to-string it)))

(def-filter :default (it default)
  (if (zerop (length it)) default it))

(def-filter :force_escape (it)
  (escape-for-html (princ-to-string it)))

(def-filter :length (it)
  (length (princ-to-string it)))

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
      (let* ((*package* (find-package :common-lisp-user))
             (sexp (read-from-string lisp-string))
             (fn (coerce sexp 'function)))
        (funcall fn it))
    (condition (e)
      (template-error "There was an error executing the lisp tag ~S: ~A" lisp-string e))))

(def-filter :lower (it)
  (string-downcase (princ-to-string it)))

;;; CAVEAT: THIS FILTER IS TREATED SPECIALLY BY COMPILE-TOKEN!!!
(def-filter :safe (it)
  it)

;;; TODO: Seems like an opportune place so adopt INTERACTIVE from Emacs.
(def-filter :truncatechars (it n)
  (let ((n (if (stringp n)
	       (parse-integer n :junk-allowed t)
	       n))
	(string (princ-to-string it)))
    (if (> (length string) n)
	(concatenate 'string (subseq string 0 n) "...")
	string)))

(def-filter :upper (it)
  (string-upcase (princ-to-string it)))

(def-filter :urlencode (it)
  (url-encode (princ-to-string it)))
