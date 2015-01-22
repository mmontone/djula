(in-package #:djula)

(defgeneric compile-template (compiler name &optional error-p)
  (:documentation "Provides a hook to customize template compilation."))

(defclass compiler ()
  ())

(defclass compiled-template ()
  ((compiled-template :initarg :compiled-template
                      :initform nil
                      :accessor compiled-template
                      :documentation "The compiled template (a closure)")
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

(defmethod initialize-instance :after ((compiled-template compiled-template) &rest initargs)
  (declare (ignore initargs))
  (flet ((compile-template-file ()
           ;; Set the template file write date
           (setf (template-file-write-date compiled-template)
                 (file-write-date (template-file compiled-template)))

           ;; Compile the template file
           (setf (compiled-template compiled-template)
                 (let ((*block-alist* nil)
                       (*linked-files* nil))
                   (compile-string (fetch-template* (template-file compiled-template)))))))

    (compile-template-file)

    (closer-mop:set-funcallable-instance-function
     compiled-template
     (lambda (stream)
       ;; Recompile the template if the template-file has changed
       (when (not (equalp (file-write-date (template-file compiled-template))
                          (template-file-write-date compiled-template)))
         (compile-template-file))
       (funcall (compiled-template compiled-template) stream)))))

(defmethod compile-template ((compiler compiler) name &optional (error-p t))
  (when-let ((template-file (find-template* name error-p)))
    (make-instance 'compiled-template
                   :template-file template-file)))

(defclass toplevel-compiler (compiler)
  ((fragment-compiler
    :reader fragment-compiler
    :initarg :fragment-compiler
    :initform (make-instance 'compiler))))

(defvar *current-compiler* (make-instance 'toplevel-compiler))

(defmethod compile-template :around ((compiler toplevel-compiler) name &optional (error-p t))
  (let ((*block-alist* nil)
        (*linked-files* nil))
    (let ((*current-compiler* (fragment-compiler compiler)))
      (call-next-method))))

(defun compile-template* (name)
  "Compiles template NAME with compiler in *CURRENT-COMPILER*"
  (compile-template *current-compiler* name))

(defun render-template* (template &optional stream &rest *template-arguments*)
  "Render TEMPLATE into STREAM passing *TEMPLATE-ARGUMENTS*"
  (check-type stream (or null stream))
  (cond
    ((or (pathnamep template)
         (stringp template))
     ;; Accept strings and pathnames as template designators.
     (apply #'render-template* (compile-template* template) stream *template-arguments*))
    ((functionp template)
     (let ((*accumulated-javascript-strings* nil)
           (*current-language* *current-language*))
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
               (error e))))))))

(defun compile-string (string)
  (let ((fs (mapcar #'compile-token (process-tokens (parse-template-string string)))))
    (lambda (stream)
      (dolist (f fs)
        (funcall f stream)))))

(defun compile-token (token)
  (destructuring-bind (name . args) token
    (let ((compiler (get name 'token-compiler)))
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
                       (not *catch-template-errors-p*))
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
    (princ string stream)))
