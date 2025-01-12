(in-package #:djula)

(defvar *static-url* nil)

(defun set-static-url (url)
  (setf *static-url* url))

(defun find-static-file (file)
  (if (string= "/" (subseq *static-url* (1- (length *static-url*))))
    (format nil "~A~A" *static-url* file)
    (format nil "~A/~A" *static-url* file)))
