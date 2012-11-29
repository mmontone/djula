(in-package #:djula)

(defgeneric find-template (store name)
  (:documentation "Return a hashable key that uniquely identifies the named template."))

(defgeneric fetch-template (store key)
  (:documentation "Return the text of the template identified by the given key."))

;; TODO: Is there a way to make CURRENT-PATH dynamic?
(defclass file-store ()
  ((current-path
    :initform nil
    :type (or null string)
    :documentation "The location of the most-recently fetched template.")
   (search-path
    :initarg :search-path
    :initform nil
    :type list
    :documentation "User-provided list of template locations."))
  (:documentation "Searches for template files on disk according to the given search path."))

(defmethod find-template ((store file-store) name)
  (with-slots (current-path search-path)
      store
    (if (char= (char name 0) #\/)
	(fad:file-exists-p name)
	(loop
	   with path = (if current-path (cons (directory-namestring current-path) search-path) search-path)
	   for dir in path
	   thereis (fad:file-exists-p (merge-pathnames name dir))))))

(defmethod fetch-template ((store file-store) key)
  (with-slots (current-path)
      store
    (and key (slurp (setf current-path key)))))

(defvar *current-store* (make-instance 'file-store)
  "The currently in-use template store.  Defaults to a FILE-STORE.")

(defun find-template* (name)
  (find-template *current-store* name))

(defun fetch-template* (key)
  "Return the text of a template fetched from the *CURRENT-STORE*."
  (fetch-template *current-store* key))

(defun slurp (pathname)
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence octets stream)
      (babel:octets-to-string octets))))
