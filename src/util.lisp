(in-package #:djula)

(defun url-encode-path (path)
  (with-output-to-string (out)
    (destructuring-bind (abs/rel . dpath)
        (pathname-directory path)
      ;; maybe initial "/"
      (case abs/rel
	(:absolute (write-char #\/ out))
	(otherwise nil))
      ;; the directory path
      (dolist (x dpath)
        (write-string (url-encode x) out)
        (write-char #\/ out))
      ;; the name
      (write-string (url-encode (pathname-name path)) out)
      ;; maybe type
      (if (pathname-type path)
	  (format out ".~A" (pathname-type path))))))

;;; edi
(defun escape-for-html (string)
  "Escapes the characters #\\<, #\\>, #\\', #\\\", and #\\& for HTML output."
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for char = (read-char in nil nil)
            while char
            do (case char
                 ((#\<) (write-string "&lt;" out))
                 ((#\>) (write-string "&gt;" out))
                 ((#\") (write-string "&quot;" out))
                 ((#\') (write-string "&#039;" out))
                 ((#\&) (write-string "&amp;" out))
                 (otherwise (write-char char out)))))))

;;; edi
(defun url-encode (string)
  "URL-encodes a string using the external format EXTERNAL-FORMAT."
  (with-output-to-string (s)
    (loop for c across string
          for index from 0
          do (cond ((or (char<= #\0 c #\9)
                        (char<= #\a c #\z)
                        (char<= #\A c #\Z)
                        ;; note that there's no comma in there - because of cookies
                        (find c "$-_.!*'()" :test #'char=))
                     (write-char c s))
                   (t (loop for octet across (babel:string-to-octets string :start index :end (1+ index))
                            do (format s "%~2,'0x" octet)))))))

(defun join (separator list)
  "Join the strings in LIST, using SEPARATOR in between the elements.

Similar to Python's str.join"
  ;; We have to remove the 'extra' separator at the start.
  (when list
    (subseq
     (let ((result ""))
       (dolist (item list result)
         (setf result (concatenate 'string result separator item))))
     (length separator))))

(defun truncate-characters (string max-length &optional (elision-string *elision-string*))
  "If the STRING is larger than MAX-LENGTH, truncate it and append the
ELISION-STRING so that the total length is MAX-LENGTH. Otherwise return the
STRING unmodified. If the truncation is impossible to accomplish, return nil. "
  (assert (and (stringp string)
               (integerp max-length)))
  (let ((string-length (length string))
        (elision-string-length (length elision-string)))
    (cond
      ((> elision-string-length string-length)
       nil)
      ((> string-length max-length)
       (concatenate 'string
                    (subseq string 0 (- max-length elision-string-length))
                    elision-string))
      (t string))))

;; Taken from: http://malisper.me/2015/05/31/efficiently-building-lists/
(defmacro accum (accfn &body body)
  (let ((ghead (gensym "HEAD"))
        (gtail (gensym "TAIL"))
        (garg  (gensym "ARG")))
    `(let* ((,ghead (list nil))
            (,gtail ,ghead))
       (macrolet ((,accfn (,garg)
                    `(setf ,',gtail
                           (setf (cdr ,',gtail)
                                 (list ,,garg)))))
         ,@body
         (cdr ,ghead)))))
