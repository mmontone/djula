
(in-package :djula)

; configure these dynamically at runtime

(defvar *current-language* nil)

(defvar *default-language* :english)

(defvar *use-example-values-p* nil)

(defvar *catch-template-errors-p* t)

(defvar *allow-include-roots* ())

(defvar *template-string-if-invalid* nil) ;or string

(defvar *eval-lisp-tags* t)

; configure these before compiling Djula [or recompile after changing]

(defvar *translation-table-regexps* (list "inter.*\\.lisp$" "inter.*\\.cl$" "inter.*\\.sexp$"))

(defvar *example-table-regexps* (list "example.*\\.lisp" "example.*\\.cl" "example.*\\.sexp"))

; special

(defvar *current-template-path* nil)

(defvar *template-search-path* nil)

(defvar *template-arguments*)

(defvar *known-translation-tables*)

(defvar *known-example-tables*)

(defvar *block-alist*)

(defvar *linked-files*)

(defvar *template-ffc*)

; templates used when building documentation

(defparameter *djula-source-folder*
  (asdf:component-pathname (asdf:find-component (asdf:find-system :djula)
						"src")))

(defparameter  *djula-documentation-template-source-folder*
  (merge-pathnames "documentation-templates/"  *djula-source-folder*))

(defparameter *djula-template-api-documentation-template*
  (merge-pathnames "template-api-documentation.html" *djula-documentation-template-source-folder*))

; template paths

(defun template-path (path)
  (if (char= (char path 0) #\/)
      (fad:file-exists-p path)
      (dolist (dir
                (if *current-template-path*
                    (cons (directory-namestring *current-template-path*) *template-search-path*)
                    *template-search-path*))
        (aif (fad:file-exists-p (merge-pathnames path dir))
             (return it)))))

; error

(defmacro with-template-error (recovery-form &body body)
  "executes `BODY', returning it's results. if there is an error in `BODY' and
*CATCH-TEMPLATE-ERRORS-P* is T, then the debugger is not invoked: instead
`RECOVERY-FORM' is evaluated and it's results returned."
  (let ((e (gensym "error")))
    `(handler-case (progn ,@body)
       (error (,e) (if *catch-template-errors-p*
		       ,recovery-form
		       (error ,e))))))

(defun template-error-string (fmt-string &rest fmt-args)
  "creates a fancy error string to display error information in the browser"
  (concatenate 'string "{# Error: " (apply 'format nil fmt-string fmt-args) " #}"))

; parser
  
(defun .get-closing-delimiter (type)
  (ecase type
    (:comment "#}")
    (:unparsed-variable "}}")
    (:unparsed-translation-variable "_}")
    (:unparsed-tag "%}")))

(defun .split-template-string (string start)
  (let (({ (position #\{ string :start start :test 'char=)))
    (if (null {)
	(if (< start (length string))
	    (list `(:string ,(subseq string start))))
	(if (> { start)
	    (cons `(:string ,(subseq string start {))
		  (.split-template-string string {))
	    (let* ((next (char string (1+ {)))
		   (type (case next
			   (#\# :comment)
			   (#\{ :unparsed-variable)
			   (#\_ :unparsed-dictionary-variable)
			   (#\% :unparsed-tag)
			   (otherwise :not-special))))
	      (ecase type
		((:comment :unparsed-variable :unparsed-dictionary-variable :unparsed-tag)
		 (let ((end (search (.get-closing-delimiter type)
				    string
				    :start2 (1+ {))))
		   (if (null end)
		       (list `(:string ,(subseq string start)))
		       (cons (list type (subseq string (+ 2 {) end))
			     (.split-template-string string (+ 2 end))))))
		(:not-special (cons `(:string "{") (.split-template-string string (1+ start))))))))))

(defun parse-template-string (string)
  (.split-template-string string 0))

; token processor

(defmacro def-token-processor (name args rest-var &body body)
  `(setf (get ,name 'token-processor)
	 (m (,rest-var ,@args) ,@body)))

(defun .process-token (token rest-token-list)
  (destructuring-bind (name . args) token
    (let ((f (get name 'token-processor)))
      (if (null f)
	  (cons token (process-tokens rest-token-list))
	  (with-template-error (cons `(:string ,(template-error-string "There was an error processing the token ~A" token))
				     (process-tokens rest-token-list))
	    (apply f rest-token-list args))))))

(defun process-tokens (tokens)
  (if tokens
      (.process-token (first tokens) (rest tokens))))

; token compiler

(defmacro def-token-compiler (name args &body body)
  `(setf (get ,name 'token-compiler)
	 (m ,args ,@body)))

(defun compile-token (token)
  (destructuring-bind (name . args) token
    (let ((compiler (get name 'token-compiler)))
      (if (null compiler)
	  (let ((s (template-error-string "Unknown token ~A" name)))
	    (f0 s))
	  (with-template-error (f0 (template-error-string "There was an error compiling the token ~A" name))
	    (let ((f (apply compiler args)))
	      (assert (functionp f)
		      nil
		      "Compiling the token ~A did not return a function"
		      name)
	      (f0 (with-template-error (template-error-string "There was an error rendering the token ~A" name)
		    (funcall f)))))))))

; basic token: comments

(def-token-processor :comment (comment-string) rest
  ":COMMENT tokens are removed by PROCESS-TOKENS"
  (declare (ignore comment-string))
  (process-tokens rest))

; basic token: strings

(def-token-processor :string (string) unprocessed
  "adjacent :STRING tokens are concatenated together by PROCESS-TOKENS as a small optimization"
  (let ((processed (process-tokens unprocessed)))
    (if (or (null processed)
	    (not (eql (caar processed) :string)))
	`((:string ,string) ,@processed)
	`((:string ,(format nil "~A~A" string (second (first processed))))
	  ,@(rest processed)))))

(def-token-compiler :string (string)
  ":STRING tokens compile into a function that simply returns the string"
  (constantly string))

; API

(defun compile-template (path
			 &key (return-format :string) ;or :OCTETS
			      ffc)
  (let ((*current-template-path* (template-path path)))
    (compile-template-string (cl-ffc:slurp-utf-8-file *current-template-path*)
          :return-format return-format
          :ffc ffc)))

(defun .compile-template-string (string &key return-bytes)
  (if return-bytes
      (let ((fn (.compile-template-string string)))
	(f0 (trivial-utf-8:string-to-utf-8-bytes (funcall fn))))
      (let* ((fs (mapcar 'compile-token (process-tokens (parse-template-string string)))))
	(f0 (.funcall-and-concatenate fs)))))

(defun compile-template-string (string
				&key (return-format :string) ; or :OCTETS
				     ffc)
  (let* (*block-alist*
	 *linked-files*
	 (*template-ffc* ffc)
	 (fn (.compile-template-string string
				       :return-bytes (eql return-format :octets))))
    (values (f (&rest *template-arguments* &key &allow-other-keys)
	      (let (*known-translation-tables*
		    *known-example-tables*
		    *accumulated-javascript-strings*
		    (*template-ffc* ffc)
		    (*current-language* *current-language*)) ;; this is rebound so that
		                                             ;; {% set-language %} can SETF *CURRENT-LANGUAGE*
		(funcall fn)))
	    (mapcar 'namestring *linked-files*))))

(defmacro def-template (name
			template-root-folder
			relative-path
			template-kwd-args
			&rest compile-template-kwd-args
			&key return-format
			     ffc
			     template-path)
  (declare (ignore return-format ffc template-path))
  (let ((<fn> (gensym "fn-")))
    `(let ((,<fn> (compile-template ,template-root-folder ,relative-path
				    ,@compile-template-kwd-args)))
       (defun ,name ,(if template-kwd-args
			 `(&key ,@template-kwd-args))
	 (apply ,<fn>
		,@(or (mapcar (f_ (if (listp _)
				      (first _)
				      _))
			      template-kwd-args)
		      (list nil)))))))
