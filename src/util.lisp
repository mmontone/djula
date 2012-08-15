(in-package #:djula)

(defun .keyword (thing)
  (values (intern (string-upcase (string thing)) :keyword)))

(defun .funcall-and-concatenate (thunks)
  (with-output-to-string (s)
    (dolist (thunk thunks)
      (princ (funcall thunk) s))))

(defun .getf (place indicator)
  "like GETF but returns a second value indicating the lookup's success or failure (like GETHASH's PRESENT-P)"
  (when place
    (destructuring-bind (k v . rest) place
      (if (eql k indicator)
	  (values v place)
	  (.getf rest indicator)))))

; url/html encoding

(defmacro .do-array ((var array &optional return) &body body)
  "like DOLIST but for arrays"
  (let ((n% (gensym "array-index"))
	(a%  (gensym "array")))
    `(let ((,a% ,array))
       (declare (dynamic-extent ,a%))
       (dotimes (,n% (length ,a%) ,return)
	 (let ((,var (aref ,a% ,n%)))
	   ,@body)))))

(defmacro .do-string ((var string &optional return) &body body)
  "like DOLIST but for strings"
  `(.do-array (,var (the string ,string) ,return)
     (declare (type character ,var))
     ,@body))

(defun djula-html-escape (string)
  (with-output-to-string (out)
  "returns an html-escaped version of `STRING'"
  ;; this function was created by modifying HUNCHENTOOT:ESCAPE-FOR-HTML
  (.do-string (c string)
    (case c
      ((#\<) (write-string "&lt;" out))
      ((#\>) (write-string "&gt;" out))
      ((#\") (write-string "&quot;" out))
      ((#\') (write-string "&#039;" out))
      ((#\&) (write-string "&amp;" out))
      (otherwise (write-char c out))))))

(defun djula-url-encode (string)
  "returns a url-encoded version of `STRING'. assumes UTF-8 so be careful if seen around HUNCHENTOOT:URL-DECODE, which probably assumes LATIN-1 by default. see HUCHENTOOT:*DEFAULT-EXTERNAL-FORAT*"
  (with-output-to-string (out)
    (.do-string (c string)
      (if (or (char<= #\0 c #\9)
	      (char<= #\a c #\z)
	      (char<= #\A c #\Z)

	      ;; "note that there's no comma in there - because of cookies"
	      ;;    --Hunchentoot source
	      ;;
	      ;;       I don't know why there's no comma "because of cookies",
	      ;;       it's what hunchentoot did so I copied it..
	      ;;         --Nick

	      (find c "$-_.!*'()" :test #'char=))
	  (write-char c out)
	  (.do-array (b (trivial-utf-8:string-to-utf-8-bytes (string c)))
	    (format out "%~2,'0x" b))))))

(defun djula-url-encode-path (path)
  (with-output-to-string (out)
    (destructuring-bind (abs/rel . dpath) (pathname-directory path)

      ; maybe initial "/"
      (case abs/rel
	(:absolute (write-char #\/ out))
	(otherwise nil))

      ; the directory path
      (mapc (f_ (write-string (url-encode _) out)
		(write-char #\/ out))
	    dpath)

      ; the name
      (write-string (url-encode (pathname-name path)) out)

      ; maybe type
      (if (pathname-type path)
	  (format out ".~A" (pathname-type path))))))

(defun djula-url-decode (string)
  "returns the a new url-decoded version of `STRING'. assumes UTF-8 so be careful if seen around HUNCHENTOOT:URL-ENCODE, which probably assumes LATIN-1 by default. see HUCHENTOOT:*DEFAULT-EXTERNAL-FORAT*"
  (let ((vector (make-array (length string)
                            :element-type '(unsigned-byte 8)
                            :fill-pointer 0))
	percent-p
	buff)
    (declare (dynamic-extent vector))
    (dotimes (n (length string) (trivial-utf-8:utf-8-bytes-to-string (make-array (length vector)
										 :element-type '(unsigned-byte 8)
										 :initial-contents vector)))
      (let ((c (char string n)))
	(cond (buff          (vector-push (parse-integer string
							 :start (1- n)
							 :end (1+ n)
							 :radix 16)
					  vector)
			     (setq buff nil))
	      (percent-p     (setq buff t
				   percent-p nil))
	      ((char= c #\%) (setq percent-p t))
	      (t             (vector-push (char-code (case c
						       ((#\+)     #\Space)
						       (otherwise c)))
					  vector)))))))
