(in-package #:djula)

(defvar *recompile-templates-on-change* t
  "When enabled, templates are recompiled if they have changed on disk, before they are rendered.
This is the default and convenient for development. For production, this can be turned off and the filesystem check is bypassed.")

(defgeneric compile-template (compiler name &optional error-p)
  (:documentation "Provides a hook to customize template compilation."))

(defclass compiler ()
  ()
  (:documentation "Abstract class. Top-level class for template compilers."))

(defclass compiled-template ()
  ((compiled-template :initarg :compiled-template
                      :initform nil
                      :accessor compiled-template
                      :documentation "The compiled template (a closure)")
   (linked-templates :initarg :linked-templates
                     :accessor linked-templates
                     :initform '()
                     :documentation "Extends for Include files.")
   (template-file :initarg :template-file
                  :accessor template-file
                  :initform (error "Provide the template file")
                  :documentation "The filepath of the template")
   (template-file-write-date :accessor template-file-write-date
                             :documentation "The write date of the template file"))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation "A compiled template"))

(defmethod print-object ((compiled-template compiled-template) stream)
  (print-unreadable-object (compiled-template stream :type t :identity t)
    (format stream "~A" (template-file compiled-template))))

(defmethod compile-template-file ((compiled-template compiled-template))
  ;; Set the template file write date
  (setf (template-file-write-date compiled-template)
        (file-write-date (template-file compiled-template)))

  ;; Compile the template file
  (let ((*block-alist* nil)
        (*linked-templates* nil))
    (let ((compiled-str
            (compile-string (fetch-template* (template-file compiled-template)))))
      (setf (compiled-template compiled-template) compiled-str
            (linked-templates compiled-template) *linked-templates*))))

(defmethod template-changed ((compiled-template compiled-template))
  (or (not (uiop:file-exists-p (template-file compiled-template)))
      (not (= (template-file-write-date compiled-template)
              (file-write-date (template-file compiled-template))))))

(defmethod initialize-instance :after ((compiled-template compiled-template) &rest initargs)
  (declare (ignore initargs))

  (compile-template-file compiled-template)

  (closer-mop:set-funcallable-instance-function
   compiled-template
   (if *recompile-templates-on-change*
       (lambda (stream)
         ;; Recompile the template if the template-file has changed
         (let ((template-file-write-date (template-file-write-date compiled-template)))
           (when (or (not (equalp (file-write-date (template-file compiled-template))
                                  template-file-write-date))
                     (loop for linked-template in (linked-templates compiled-template)
                             thereis (template-changed linked-template)))
             (compile-template-file compiled-template)))
         (funcall (compiled-template compiled-template) stream))
       ;; else, avoid the automatic template file checking and recompilation
       (compiled-template compiled-template))))

(defmethod compile-template ((compiler compiler) name &optional (error-p t))
  (when-let ((template-file (find-template* name error-p)))
    (make-instance 'compiled-template
                   :template-file template-file)))

(defclass toplevel-compiler (compiler)
  ((fragment-compiler
    :reader fragment-compiler
    :initarg :fragment-compiler
    :initform (make-instance 'compiler)))
  (:documentation "The default Djula template compiler."))

(defvar *current-compiler* (make-instance 'toplevel-compiler)
  "The current template compiler.")

(defmethod compile-template ((compiler toplevel-compiler) name &optional (error-p t))
  (let ((*block-alist* nil)
        (*linked-templates* nil))
    (let ((*current-compiler* (fragment-compiler compiler)))
      (compile-template *current-compiler* name error-p))))

(defun compile-template* (name)
  "Compiles template NAME with compiler in *CURRENT-COMPILER*."
  (compile-template *current-compiler* name))

(defun render-template* (template &optional stream &rest *template-arguments*)
  "Render TEMPLATE into STREAM passing *TEMPLATE-ARGUMENTS*."
  (cond
    ((or (pathnamep template)
         (stringp template))
     ;; Accept strings and pathnames as template designators.
     (apply #'render-template* (compile-template* template) stream *template-arguments*))
    ((functionp template)
     (let ((*template-arguments* (append *template-arguments* *default-template-arguments*))
           (*accumulated-javascript-strings* nil)
           (*current-language* *current-language*)
           (*current-template* template))
       (handler-case
           (if stream
               (funcall template stream)
               (with-output-to-string (s)
                 (funcall template s)))
         (error (e)
           (if (and *catch-template-errors-p*
                    *fancy-error-template-p*)
               (render-error-template e
                                      (trivial-backtrace:print-backtrace e :output nil)
                                      template stream)
               (error e))))))
    (t (error 'simple-error
              :format-control "~A is not a valid template"
              :format-arguments (list template)))))

(defun compile-string (string)
  "Compile the template in STRING.
Returns a funcallable template."
  (let ((fs (mapcar #'compile-token (process-tokens (parse-template-string string)))))
    (lambda (stream)
      (dolist (f fs)
        (funcall f stream)))))

(defun compile-token (token)
  (destructuring-bind (name . args) token
    (let ((compiler (find-token-compiler name)))
      (if (null compiler)
          (lambda (stream)
            (princ (template-error-string "Unknown token ~A" name) stream))
          (handler-case
              ;; Handle compile-time errors.
              (let ((f (apply compiler args)))
                (assert (functionp f)
                        nil
                        "Compiling the token ~A did not return a function"
                        name)
                (lambda (stream)
                  (handler-case
                      ;; Handle run-time errors.
                      (funcall f stream)
                    (template-error (e1)
                      (if *catch-template-errors-p*
                          (princ e1 stream)
                          (error e1)))
                    (error (e2)
                      (let ((msg (template-error-string* e2 "There was an error rendering the token ~A" token)))
                        (if (and *catch-template-errors-p*
                                 (not *fancy-error-template-p*))
                            (princ msg stream)
                            (template-error msg)))))))
            (template-error (e1)
              (if (and *catch-template-errors-p*
                       (not *fancy-error-template-p*))
                  (lambda (stream)
                    (princ e1 stream))
                  (error e1)))
            (error (e2)
              (let ((msg (template-error-string* e2 "There was an error compiling the token ~A" token)))
                (if (and *catch-template-errors-p*
                         (not *fancy-error-template-p*))
                    (lambda (stream)
                      (princ msg stream))
                    (template-error msg)))))))))

(def-token-compiler :string (string)
  ":STRING tokens compile into a function that simply returns the string"
  (lambda (stream)
    (write-string string stream)))
