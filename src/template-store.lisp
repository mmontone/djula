(in-package #:djula)

(defgeneric find-template (store name &optional error-p)
  (:documentation "Return a hashable key that uniquely identifies the named template."))

(defgeneric fetch-template (store key)
  (:documentation "Return the text of the template identified by the given key."))

(defclass template-store ()
  ())

;; ---------- filesystem-template-store --------------------------------

;; TODO: Is there a way to make CURRENT-PATH dynamic?
(defclass filesystem-template-store (template-store)
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

(defmethod find-template ((store filesystem-template-store) name &optional (error-p t))
  "Algorithm that finds a template in a filesystem-template-store."
  (with-slots (current-path search-path)
      store
    (or
     (cond
       ;; If it is a pathname, just check that the file exists
       ((pathnamep name)
        (uiop:file-exists-p name))
       ;; If first character is a '/', then treat it as an absolute path first, then relative to template store search paths.
       ((char= (char name 0) #\/)
        (or
         (uiop:file-exists-p name)
         (loop
           for dir in search-path
             thereis (uiop:file-exists-p (merge-pathnames (subseq (string name) 1) dir)))))
       ;; Otherwise, search relative to either current path or search paths
       (t (loop
            with path = (if current-path
                            (cons (directory-namestring current-path) search-path)
                            search-path)
            for dir in path
              thereis (uiop:file-exists-p (merge-pathnames name dir)))))
     (when error-p
       (error "Template not found: ~s" name)))))

;;------- memory-template-store ------------------------------------

;; https://github.com/mmontone/djula/issues/79

(defclass memory-template-store (filesystem-template-store)
  ((templates-contents :initform (make-hash-table :test 'equalp)
		       :documentation "A hash-table cache that maps pathnames and template names to template sources.")
   (templates :initform (make-hash-table :test 'equalp)))
  (:documentation "A template store with a memory cache.
This store works like FILESYSTEM-TEMPLATE-STORE, but when a template is compiled it saves the template contents in memory and templates are rendered from memory after.
This is useful for building standalone binaries, as there's no need to ship template files once they have been compiled (they are stored in the lisp image memory).
You need to set Djula's *current-store* to a MEMORY-TEMPLATE-STORE instance before compiling templates.
See the section on building standalong binaries in Djula manual."))

(defmethod fetch-template ((store memory-template-store) name)
  (with-slots (templates-contents) store
    (or (gethash name templates-contents)
    (let ((template-content (call-next-method)))
      (setf (gethash name templates-contents) template-content)
      template-content))))

(defmethod find-template ((store memory-template-store) name &optional (error-p t))
  (declare (ignorable error-p))
  (with-slots (templates templates-contents) store
    (or (unless (stringp name) ;; <------
          ;; if it is a string: that means we are looking for an "extends" name,
          ;; otherwise the method receives pathnames.
          ;; If we have one base system whose templates get compiled,
          ;; then a second system who defines new templates with the same name in order to override them,
          ;; we want to search again the full pathname. That way we enable overrides.
          ;; So, only if it's not a string, get the template from the hash-table cache:
          (gethash name templates))
        (let ((template (call-next-method)))
          (when template
            (setf (gethash name templates) template)
            (setf (gethash name templates-contents) (alexandria:read-file-into-string template)))
          template))))

;;----- Utilities for in-memory templates binaries ----------------------

(defun list-asdf-system-templates (asdf-system component)
  "List djula templates in ASDF-SYSTEM at COMPONENT.
A list of template PATHNAMEs is returned."

  (let* ((sys (asdf:find-system asdf-system))
         (module (find component (asdf:component-children sys) :key #'asdf:component-name :test #'equal))
         (alltemplates (remove-if-not (lambda (x) (typep x 'asdf:static-file))
                                      (asdf:module-components module))))
    (mapcar (lambda (it) (asdf:component-pathname it))
            alltemplates)))

;;--------- API ------------------------------

(defvar *current-store* (make-instance 'filesystem-template-store)
  "The currently in-use template store.  Defaults to a FILESYSTEM-TEMPLATE-STORE.")

(defmethod fetch-template ((store filesystem-template-store) name)
  (with-slots (current-path)
      store
    (setf current-path name)
    (and name (alexandria:read-file-into-string (find-template store name)))))

(defun add-template-directory (directory &optional (template-store *current-store*))
  "Adds DIRECTORY to the search path of the TEMPLATE-STORE."
  (pushnew directory
           (search-path template-store)
           :test #'equalp))

(defun find-template* (name &optional (error-p t))
  "Find template with name NAME in *CURRENT-STORE*.
If the template is not found, an error is signaled depending on ERROR-P argument value."
  (find-template *current-store* name error-p))

(defun fetch-template* (key)
  "Return the text of a template fetched from the *CURRENT-STORE*."
  (fetch-template *current-store* key))
