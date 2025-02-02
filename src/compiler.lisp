(in-package #:djula)

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

(defun render-template* (template &optional destination &rest *template-arguments*)
  "Render TEMPLATE into DESTINATION passing *TEMPLATE-ARGUMENTS*.
DESTINATION is the same as FORMAT destination.
TEMPLATE-ARGUMENTS is a property-list. "
  (cond
    ((or (pathnamep template)
         (stringp template))
     ;; Accept strings and pathnames as template designators.
     (apply #'render-template* (compile-template* template) destination *template-arguments*))
    ((functionp template)
     (let ((*template-arguments* (append *template-arguments* *default-template-arguments*))
           (*accumulated-javascript-strings* nil)
           (*current-language* *current-language*)
           (*current-template* template))
       (handler-case
           (uiop:with-output (stream destination)
             (when *debug-mode*
               (if *fancy-debug-p*
                   (print-fancy-debugging-information stream)
                   (print-debugging-information stream)))
             (funcall template stream))
         (error (e)
           (if (and *catch-template-errors-p*
                    *fancy-error-template-p*)
               (render-error-template e destination
                                      :backtrace (with-output-to-string (s)
                                                   (trivial-backtrace:print-backtrace-to-stream s))
                                      :template template
                                      :context (list :arguments *template-arguments*
                                                     :language *current-language*
                                                     :template-package *template-package*))
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
  "Compiles TOKEN.
The compilation of a TOKEN creates a LAMBDA that given a stream renders the token."
  ;; Note that given that the compilation of a token is also in charge
  ;; of determining how to render the token, there are both error handlers for
  ;; compile-time and also run-time here.
  (destructuring-bind (name . args) token
    (let ((compiler (find-token-compiler name)))
      (if (null compiler)
          (lambda (stream)
            (princ (template-error-string "unknown token ~A" name) stream))
          (handler-case
              ;; Handle compile-time errors.
              (let ((f (apply compiler args)))
                (assert (functionp f)
                        nil
                        "Compiling the token ~A did not return a function"
                        name)
                (lambda (stream)
                  ;; This is the rendering lambda, so handle run-time errors here.
                  (handler-case
                      (funcall f stream)
                    (template-error (e1)
                      (if (and *catch-template-errors-p*
                               (not *fancy-error-template-p*))
                          (princ e1 stream)
                          ;; If fancy-errors are enabled, then resignal the condition. Fancy errors are handled higher in the condition handlers, in RENDER-TEMPLATE*.
                          (error e1)))
                    (error (e2)
                      (let ((msg (template-error-string* e2 "rendering the token ~A" (write-to-string token :length 10))))
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
              (let ((msg (template-error-string* e2 "compiling the token ~A" token)))
                (if (and *catch-template-errors-p*
                         (not *fancy-error-template-p*))
                    (lambda (stream)
                      (princ msg stream))
                    (template-error msg)))))))))

(def-token-compiler :string (string)
  ":STRING tokens compile into a function that simply returns the string"
  (lambda (stream)
    (write-string string stream)))
