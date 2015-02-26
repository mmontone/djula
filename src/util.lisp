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

(defun join (str lst &optional (jstr ""))
  (cond
    ((null lst) jstr)
    (t (let ((news (concatenate 'string
				jstr
				(first lst)
				(if (null (rest lst))
				    ""
				    str))))
	 (join str (rest lst) news)))))

(defun split (chars str &optional (lst nil) (accm ""))
  (cond
    ((= (length str) 0) (reverse (cons accm lst)))
    (t
     (let ((c (char str 0)))
       (if (member c chars)
	   (split chars (subseq str 1) (cons accm lst) "")
	   (split chars (subseq str 1) 
		  lst 
		  (concatenate 'string
			       accm
			       (string c))))
       ))))
