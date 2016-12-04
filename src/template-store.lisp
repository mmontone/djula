(in-package #:djula)

(defgeneric find-template (store name &optional error-p)
  (:documentation "Return a hashable key that uniquely identifies the named template."))

(defgeneric fetch-template (store key)
  (:documentation "Return the text of the template identified by the given key."))

;; TODO: Is there a way to make CURRENT-PATH dynamic?
(defclass file-store ()
  ((current-path
    :initform nil
    :type (or null string pathname)
    :documentation "The location of the most-recently fetched template.")
   (search-path
    :initarg :search-path
    :accessor search-path
    :initform nil
    :type list
    :documentation "User-provided list of template locations."))
  (:documentation "Searches for template files on disk according to the given search path."))

(defmethod find-template ((store file-store) name &optional (error-p t))
  (with-slots (current-path search-path)
      store
    (or
     (cond
       ((pathnamep name)
        (fad:file-exists-p name))
       ((char= (char name 0) #\/)
        (fad:file-exists-p name))
       (t (loop
             with path = (if current-path
                             (cons (directory-namestring current-path) search-path)
                             search-path)
             for dir in path
             thereis (fad:file-exists-p (merge-pathnames name dir)))))
     (when error-p
       (error "Template ~A not found" name)))))

(defmethod fetch-template ((store file-store) name)
  (with-slots (current-path)
      store
    (setf current-path name)
    (and name (slurp (find-template store name)))))

(defvar *current-store* (make-instance 'file-store)
  "The currently in-use template store.  Defaults to a FILE-STORE.")

(defun add-template-directory (directory &optional (template-store *current-store*))
  "Adds DIRECTORY to the search path of the TEMPLATE-STORE"
  (pushnew directory
           (search-path template-store)
           :test #'equalp))

(defun find-template* (name &optional (error-p t))
  (find-template *current-store* name error-p))

(defun fetch-template* (key)
  "Return the text of a template fetched from the *CURRENT-STORE*."
  (fetch-template *current-store* key))

(defun slurp (pathname)
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence octets stream)
      (babel:octets-to-string octets))))
