(in-package #:djula)

(defun get-closing-delimiter (type)
  (ecase type
    (:comment "#}")
    (:unparsed-variable "}}")
    (:unparsed-translation-variable "_}")
    (:unparsed-tag "%}")
    (:verbatim "$}")))

(defun split-template-string (string start)
  (let (({ (position #\{ string :start start :test 'char=)))
    (if (null {)
	(if (< start (length string))
	    (list `(:string ,(subseq string start))))
	(if (> { start)
	    (cons (list :string (subseq string start {))
		  (split-template-string string {))
	    (let* ((next (char string (1+ {)))
		   (type (case next
			   (#\# :comment)
			   (#\{ :unparsed-variable)
			   (#\_ :unparsed-translation-variable)
			   (#\% :unparsed-tag)
			   (#\$ :verbatim)
			   (otherwise :not-special))))
	      (ecase type
		((:comment 
		  :unparsed-variable 
		  :unparsed-translation-variable 
		  :unparsed-tag
		  :verbatim)
		 (let ((end (search (get-closing-delimiter type)
				    string
				    :start2 (1+ {))))
		   (if (null end)
		       (list (list :string (subseq string start)))
		       (cons (list type (subseq string (+ 2 {) end))
			     (split-template-string string (+ 2 end))))))
		(:not-special (cons `(:string "{") (split-template-string string (1+ start))))))))))

(def-token-compiler :verbatim (string)
  (lambda (stream)
    (write-string string stream)))

(defun parse-template-string (string)
  (split-template-string string 0))
