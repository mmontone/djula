(in-package #:djula)

(defvar *auto-escape* t)

;;; truncatechars:"30" => (:truncatechars 30)
(defun parse-filter-string (string)
  (if-let ((colon (position #\: string)))
    (list (make-keyword (string-upcase (subseq string 0 colon)))
          (string-trim '(#\") (subseq string (1+ colon))))
    (list (make-keyword (string-upcase string)))))

;;; foo.bar.baz.2 => (:foo :bar :baz 2)
(defun parse-variable-phrase (string)
  (flet ((interp (s)
	   (if (every 'digit-char-p s)
	       (parse-integer s)
	       (make-keyword (string-upcase s)))))
    (if-let ((dot (position #\. string)))
      (cons (interp (subseq string 0 dot))
            (parse-variable-phrase (subseq string (1+ dot))))
      (list (interp string)))))

;;; foo.bar.baz.2 | truncatechars:"30" | upper => ((:foo :bar :baz 2) (:truncatechars 30) (:upper))
(defun parse-variable-clause (unparsed-string)
  (destructuring-bind (var . filter-strings)
      (mapcar (lambda (s)
                (string-trim '(#\space #\tab #\newline #\return) s))
	      (split-sequence:split-sequence #\| unparsed-string))
    (cons (parse-variable-phrase var)
          (mapcar #'parse-filter-string filter-strings))))

(def-token-processor :unparsed-variable (unparsed-string) rest
  ":PARSED-VARIABLE tokens are parsed into :VARIABLE tokens by PROCESS-TOKENS"
  (cons (list* :variable (parse-variable-clause unparsed-string))
        (process-tokens rest)))

;; access library patches. we need to know if the value was accessed, regardless of
;; the result being nil. so, access should return if it the object was actually accessed
;; as a second value, regardless of result being nil. This is a problem with CL,
;; not having a real boolean datatype.

(defgeneric plist-val (id list &key test key)
  (:documentation "get a value out of a plist based on its key")
  (:method (id list &key (test #'access::equalper) (key #'identity))
    (iter (for (k v) on list by #'cddr)
	  (let ((found (funcall test (funcall key k) id)))
	    (if found
		(return-from plist-val (values v found)))))))

(defgeneric do-access  (o k &key type test key skip-call?)
  (:method ((o list) k &key type test key skip-call?)
    (declare (ignore skip-call?))
    (if (or (eql type :alist)
            (and (null type) (consp (first o))))
        ;;alist
	(let ((assoc (assoc k o :test test :key key)))
	  (values (cdr assoc)
		  (and assoc t)))
        ;;plist
	(plist-val k o :test test :key key)))

  (:method ((o array) k &key type test key skip-call?)
    (declare (ignore type test key skip-call?))
    (if (< k (length o))
	(values (aref o key) t)))

  (:method ((o hash-table) k &key type test key skip-call?)
    (declare (ignore type test key skip-call?))
    (multiple-value-bind (res found) (gethash k o)
      (if found
          (values res found)
          (awhen (ignore-errors (string k))
            (gethash it o)))))
  
  (:method (o  k &key type test key skip-call?)
    ;; not specializing on standard-object here
    ;; allows this same code path to work with conditions (in sbcl)
    (let ((actual-slot-name (access::has-slot? o k)))
      (cond
        ;; same package as requested, must be no accessor so handle slots
        ((eql actual-slot-name k)
         (when (slot-boundp o k)
           (values (slot-value o k) t)))

        ;; lets recheck for an accessor in the correct package
        (actual-slot-name
         (access o actual-slot-name :type type :test test :key key
                                    :skip-call? skip-call?))
        ))))

(defun access (o k &key type (test #'access::equalper) (key #'identity)
                   skip-call?)
  "Access plists, alists, arrays, hashtables and clos objects
   all through the same interface

   skip-call, skips trying to call "
  ;; make these easy to have the same defaults everywhere
  (unless test (setf test #'access::equalper))
  (unless key (setf key #'identity))
  (multiple-value-bind (res called)
      (unless skip-call?
        ;; lets suppress the warning if it is just being called through access
        (access::call-if-applicable o k :warn-if-not-a-fn? nil))
    (if called
        (values res t)
        (do-access o k :test test :key key :type type))))

(defun apply-keys/indexes (thing keys/indexes)
  (reduce (lambda (thing key)
            (handler-case
		(cond
		   ((numberp key) (elt thing key))
		   ((keywordp key)
		    (multiple-value-bind (val accessed-p)
			(access thing key)
		      (if accessed-p
			  val
			  (access thing (intern (symbol-name key) 
						*djula-execute-package*)))))
		   (t (access thing key)))
              (error (e)
                (template-error-string* e 
                 "There was an error while accessing the ~A ~S of the object ~S"
                 (if (numberp key)
                     "index"
                     "attribute")
                 key
                 thing))))
          keys/indexes
          :initial-value thing))

(defun get-variable (name)
  "takes a variable `NAME' and returns:
   1. the value of `NAME'
   2. any error string generated by the lookup (if there is an error string then the
      lookup was unsuccessful)"
  (access *template-arguments* name))

(defun resolve-variable-phrase (list)
  "takes a list starting wise a variable and ending with 0 or more keys or indexes [this
is a direct translation from the dot (.) syntax] and returns two values:

   1. the result [looking up the var and applying index/keys]
   2. an error string if something went wrond [note: if there is an error string then
the result probably shouldn't be considered useful."
  (aand (get-variable (first list))
        (apply-keys/indexes it (rest list))))

(def-token-compiler :variable (variable-phrase &rest filters)
  ;; check to see if the "dont-escape" filter is used
  ;; "safe" takes precedence before "escape"
  (let ((dont-escape
         (or
          (find '(:safe) filters :test #'equal) ; safe filter used
          (and (not *auto-escape*)              ; autoescape off and no escape filter used
               (not (find '(:escape) filters :test #'equal))))))
    ;; return a function that resolves the variable-phase and applies the filters
    (lambda (stream)
      (multiple-value-bind (ret error-string)
          (resolve-variable-phrase variable-phrase)
        (if error-string
            (with-template-error error-string
              (error error-string))
	    (let ((filtered-ret
		   (princ-to-string
		    (or
		     (apply-filters 
		      ret filters)
		     ""))))
	    (princ (if dont-escape
		       filtered-ret
		       (escape-for-html filtered-ret))
		   stream)))))))
