; this file contains the source code to the tags documented in "tag-documentation.lisp"
; if you change anything in this file make damned sure to update
; "tag-documentation.lisp" accordingly!
;
; note: see "tag.lisp" for an explenation of how imlementing new tags work

(in-package :djula)

; util

(defmacro .with-file-handler ((string-var template-path) &body body)
  "evaluates `BODY' with `STRING-VAR' bound to a string representing the contents of
the file pointed to be the template-path `TEMPLATE-PATH', returning it's results.
if there is an error while binding `STRING-VAR' and *CATCH-TEMPLATE-ERRORS-P* is T then
it returns a function that is suitable output for the body of a DEF-TOKEN-COMPILER
form that returns some debugging info."
  (let ((path (gensym "template-path"))
	(real-path (gensym "real-path")))
    `(let* ((,path ,template-path)
	    (,real-path (find-template* ,path)))
       (if (null ,real-path)
	   (constantly (template-error-string "The file ~S does not exist" ,path))
	   (with-template-error (constantly (template-error-string "There was an error opening the file ~A. Perhaps an encoding error?" ,real-path))
	     (let ((,string-var (fetch-template* ,real-path)))
	       ,@body))))))

; definitions:

;; ; css / emit-css
;;
;; (defvar *css*)
;;
;; (def-unparsed-tag-processor :css (string) rest
;;   `((:parsed-css ,string) ,@(process-tokens rest)))
;;
;; (def-token-compiler :parsed-css (string)
;;   (pushnew string *css* :test 'equal)
;;   "")
;;
;; (def-tag-compiler :emit-css ()
;;   (apply 'concatenate
;; 	 'string
;; 	 (mapcar (f_ (format nil "~%<script type='text/javascript' src=~A></script>" _))
;;		 *css*)))

; block / endblock / extends

(def-delimited-tag :block :endblock :parsed-block)

(def-token-compiler :parsed-block ((name) . block-tokens)
  (let* ((target (member name *block-alist* :key #'first :test #'eq))
	 (fs (mapcar #'compile-token (if target (rest (first target)) block-tokens))))
    (lambda () (.funcall-and-concatenate fs))))

(def-tag-compiler :super (name)
  ;; Pay attention to scoping here.  *BLOCK-ALIST* is dynamic and cannot be
  ;; refactored to inside the lambda.  Well, not with the desired results, anyway,
  (let* ((target (member name *block-alist* :key #'first :test #'eq))
	 (*block-alist* (if target (rest target) *block-alist*))
	 (fs (mapcar #'compile-token (if target (rest (first target)) nil))))
    (lambda () (.funcall-and-concatenate fs))))

(def-tag-processor :extends (template-path) rest-tokens
  (let ((real-path (find-template* template-path)))
    (pushnew real-path *linked-files* :test 'equal)
    (if (null real-path)
	`((:string ,(template-error-string "Cannot extend the template ~A because the file ~A does not exists"
					   template-path
					   real-path)))
	(with-template-error `((:string ,(template-error-string "Cannot extend the template ~A because there was an error opening the file ~A. Perhaps an encoding error?"
								template-path
								real-path)))
	  (let ((string (fetch-template* real-path)))
	    (with-template-error `((:string ,(template-error-string "Cannot extend the template ~A because there was an error parsing the template file ~A"
								    template-path
								    real-path)))
	      (let ((processed (process-tokens (parse-template-string string))))
		(with-template-error `((:string ,(template-error-string "Cannot extend the template ~A because there was an error parsing this template file"
									template-path)))
		  (let ((extend-blocks (mapcar (f_ (destructuring-bind (<parsed-block> (name) . tokens) _
						     (declare (ignore <parsed-block>))
						     `(,name ,@tokens)))
					       (remove :parsed-block
						       (process-tokens rest-tokens)
						       :key #'first
						       :test-not #'eq))))
		    (setf *block-alist* (append extend-blocks *block-alist*))
		    processed)))))))))

; comment / endcomment

(def-delimited-tag :comment :endcomment :comment-tag)
(def-token-processor :comment-tag (&rest %) rest-tokens
  (declare (ignore %))
  (process-tokens rest-tokens))

; cycle

(def-tag-compiler :cycle (&rest list)
  (let ((circle (copy-list list))
	(unique-id (gensym "cycle")))
    (setf (rest (last circle)) circle)

    (f0 (if (nth-value 1 (.getf *template-arguments* unique-id))
	    #1=(pop (getf *template-arguments* unique-id))
	    (progn (setf *template-arguments* (list* unique-id circle *template-arguments*))
		   #1#)))))

(def-tag-compiler :debug ()
  (f0 (with-output-to-string (out)
	(flet ((% (fmt-string &rest fmt-args)
		 (terpri out)
		 (apply 'format out fmt-string fmt-args))
	       (short (thing)
		 (let ((string (princ-to-string thing)))
		   (if (> (length string) 25)
		       (format nil "~A..." (subseq string 0 22))
		       string))))
	  (macrolet ((with-safe (about &body body)
		       `(with-template-error (% "<<<There was an error gathering debug information about ~A>>>" ,about)
			  ,@body)))
	    (% "<<<START DEBUG INFO>>>")

	    (with-safe "the current language"
	      (% "Language: ~S" (or *current-language*
				    "none")))

	    (with-safe "the default language"
	      (% "Language: ~S" (or *default-language*
				    "none")))

	    (with-safe "whether or not template errors are printing to the browser"
	      (% "~A" (if *catch-template-errors-p*
			  "Printing template errors in the browser"
			  "<<<Not printing template errors in the browser>>>")))

	    (with-safe "the template error string if invalid"
	      (if *template-string-if-invalid*
		  (% "<<<Temlate string if invalid: ~S>>>" *template-string-if-invalid*)))

	    (with-safe "*ALLOW-INCLUDE-ROOTS*"
	      (% "Allow include-roots: ~A" *allow-include-roots*))

	    (with-safe "the current template root folder"
	      (% "Template folder: ~A" *current-template-root-folder*))

	    (with-safe "the arguments given to the template"
	      (if (null *template-arguments*)
		  (% "There were no arguments given to the template")
		  (progn
		    (% "Template arguments:")
		    (let ((n 0))
		      (labels ((rfn (plist)
				 (when plist
				   (destructuring-bind (k v . rest) plist
				     (% "   ~A. ~A = ~A" (incf n) k (short v))
				     (rfn rest)))))
			(rfn *template-arguments*))))))

	    (with-safe "known translation tables"
	      (if (null *known-translation-tables*)
		  (% "No known translation tables")
		  (progn
		    (% "Translation tables:")
		    (let ((n 0))
		      (dolist (d *known-translation-tables*)
			(destructuring-bind (path . alist) d
			  (% "   ~S. ~A" (incf n) (or (format nil "~S" path)
						      "{% translation %}"))
			  (let ((n 0))
			    (dolist (a alist)
			      (destructuring-bind (var . plist) a
				(% "       ~S. ~S" (incf n) var)
				(labels ((rfn (language-plist)
					   (when language-plist
					     (destructuring-bind (k v . rest) language-plist
					       (% "          language: ~S" k)
					       (% "          value: ~S" (short v))
					       (rfn rest)))))
				  (rfn plist)))))))))))

	    (with-safe "The current example tables"
	      (if (null *known-example-tables*)
		  (% "No known example tables")
		  (progn
		    (% "Example tables:")
		    (let ((n 0))
		      (dolist (d *known-example-tables*)
			(destructuring-bind (path . plist) d
			  (% "   ~S. ~A" (incf n) (or (format nil "~S" path)
						      "{% example %}"))
			  (labels ((rfn (var-plist)
				     (when plist
				       (destructuring-bind (k v . rest) var-plist
					 (% "      ~A. ~A = ~A" (incf n) k (short v))
					 (rfn rest)))))
			    (rfn plist))))))))

	    (% "<<<END DEBUG INFO>>>"))))))

; devel-language /show-language

; todo: test

(def-tag-compiler :set-language (name)
  ":SET-LANGUAGE tags are compiled into a function that set *CURRENT-LANGUAGE* to the
keyword version of `NAME' [or NIL if `NAME' is not supplied]"
  (f0 (setf *current-language* name)
      ""))

(def-tag-compiler :show-language ()
  ":SHOW-LANGUAGE tags are compiled into a function that just shows the values of
*CURRENT-LANGUAGE* or *DEFAULT-LANGUAGE* if there is no current language"
  (f0 (or *current-language* *default-language*)))

; translation-table / example-table / translation / example

(def-tag-compiler :translation-table (template-path)
  ":TRANSLATION-TABLE tags compile into a function that pushes a thunk that pushes
the list

   (`PATH' . ALIST)

to *KNOWN-TRANSLATION-TABLES*, where ALIST is composed of elements that look like:

   (VARIABLE . LANGUAGE-PLIST)

and LANGUAGE-PLIST contains LANGUAGE/VALUE key val pairs]

pushing this stuff to *KNOWN-TRANSLATION-TABLES* lets GET-TRANSLATION know
about the variable definitions contained in the translation table indicated by
`TEMPLATE-PATH'"
  (with-template-error (f0 (template-error-string "There was an error reading or parsing the contents of the translation table ~S"
						  template-path))
    (if (not (translation-table-p template-path))

	;; it doesn't look like a translation table, complain
	(progn (logv:format-log  "<<<the path ~S is not TRANSLATION-TABLE-P>>>"
				 template-path)
	       (f0 (template-error-string "the path ~S does not name a translation table! the names of translation table must match one of the following regular expressions: ~{~S ~}"
					  template-path
					  *translation-table-regexps*)))

	(aif (find-template* template-path)
	     (progn
	       
	       (pushnew it *linked-files* :test 'equal)
	       (if (not (every 'listp (.read-table it)))

		   ;; it doesn't smell like a translation table, complain
		   (progn (logv:format-log "<<<The translation table ~A doesn't look like a translation table...>>>" template-path)
			  (f0 (template-error-string "The translation table ~A doesn't look like a translation table..." template-path)))

		   ;; use the current contents of the translation table forever
		   (let ((compiled (compile-translation-table it)))
		     (f0 (push (cons it compiled)
			       *known-translation-tables*)
			 ""))))

	     ;; the file doesn't exist, complain
	     (progn (logv:format-log "<<<The translation table ~A doesn't exist!" it)
		    (f0 (template-error-string "The translation table ~A doesn't exist!" it)))))))


(def-tag-compiler :example-table (template-path)
  ":EXAMPLE-TABLE tags compile into a function that pushes a thunk that pushes the
list

   (`PATH' . PLIST)

to *KNOWN-EXAMPLE-TABLES* [where PLIST is composed of variable/value pairs], thus letting
GET-VARIABLES know about the variable definitions contained in the example table
pointed to by `TEMPLATE-PATH'

Note: definitions contained in an example table are only visible to the template if
*USE-EXAMPLE-VALUES-P* is non-NULL. If *USE-EXAMPLE-VALUES-P* is NULL, then the example table
checks to make sure *TEMPLATE-ARGUMENTS* contains all its variables, complaining if
it doesn't"
  (with-template-error (f0 (template-error-string "There was an error reading or parsing the contents of the example table ~S"
						  template-path))
    (if (not (example-table-p template-path))

	;; it doesn't look like an example table, complain
	(progn (logv:format-log "<<<the path ~S is not EXAMPLE-TABLE-P"
				template-path)
	       (f0 (template-error-string "the path ~S does not name an example table! the name of an example table must match one of the following regular expressions: ~{~S ~}"
					  template-path
					  *example-table-regexps*)))

	(aif (find-template* template-path)

	     (if (not (evenp (length (.read-table it))))

		 ;; it doesn't smell like an example table, complain
		 (progn (logv:format-log "<<<~S does not look like an example table>>>"
					 template-path)
			(f0 (template-error-string "the example table ~S does not look like an example table!"
						   template-path)))

		 (if *template-ffc*
		 
		     ;; if there is a file function cache for example table then
		     ;; always get the contents of the example table from the cache.
		     ;; This way it will be updated if someone calls FFC-SYNC on
		     ;; the cache
		     (f0 (let ((plist (cl-ffc:ffc-call *template-ffc* it)))
			   (push (cons it plist)
				 *known-example-tables*)
			   (if *use-example-values-p*
			       ""
			       (apply 'concatenate
				      'string
				      ""
				      (.check-example-table-plist plist)))))
			     
		     ;; otherwise use the current contents of the table forever
		     (let ((plist (.read-table it)))
		       (f0 (push (cons it plist)
				 *known-example-tables*)
			   (if *use-example-values-p*
			       ""
			       (apply 'concatenate
				      'string
				      ""
				      (.check-example-table-plist plist)))))))

	     ;; it doesn't exist, complain
	     (progn (logv:format-log "<<<the example table ~S doesn't exist" template-path)
		    (f0 (template-error-string "The example table ~A doesn't exist" template-path)))))))

(def-unparsed-tag-processor :translation (unparsed-string) rest
  (process-tokens `((:parsed-translation ,@(.read-table-string unparsed-string))
		    ,@rest)))

(def-unparsed-tag-processor :example (unparsed-string) rest
  (process-tokens `((:example ,@(.read-table-string unparsed-string))
		    ,@rest)))

(def-token-compiler :parsed-translation (variable . language/value-plist)
  ":PARSED-TRANSLATION tags compile into a function that pushes the list
\(`LANGUAGE' NIL `VARIABLE-NAME' `VALUE') to *KNOWN-TRANSLATION-TABLES*, thus
letting GET-VARIABLE know about the variable `VARIABLE-NAME'"
  (let ((d (list nil (.compile-translation-table-variable (cons variable language/value-plist)))))
    (f0 (push d *known-translation-tables*)
	"")))

(def-token-compiler :parsed-example (&rest variable/value-plist &key &allow-other-keys)
  ":PARSED-EXAMPLE compiles into a thunk that pushes

   (NIL . `VARIABLE/VALUE-PLIST')

to *KNOWN-EXAMPLE-TABLES*. if *USE-EXAMPLE-VALUES-P* is NULL then it checks to make
sure all the variables in `VARIABLE/VALUE-PLIST' are in *TEMPLATE-ARGUMENTS*"
  (let ((d (cons nil variable/value-plist)))
    (f0 (push d *known-example-tables*)
	(if *use-example-values-p*
	    ""
	    (apply 'concatenate
		   'string
		   ""
		   (.check-example-table-plist variable/value-plist))))))

; filter

(def-unparsed-tag-processor :filter (unparsed-string) rest
  (process-tokens `((:tag :semi-parsed-filter ,@(rest (.parse-variable-clause (format nil "INTERNAL|~A" unparsed-string))))
		    ,@rest)))

(def-delimited-tag :semi-parsed-filter :endfilter :parsed-filter)

(def-token-compiler :parsed-filter (filters . clauses)
  (let ((fs (mapcar 'compile-token clauses)))
    (f0 (.apply-filters (.funcall-and-concatenate fs) filters))))

; for / endfor

(def-delimited-tag :for :endfor :parsed-for)

(def-token-compiler :parsed-for ((var in %listvar% &optional reversed) . clause)
  (if (not (eql in :in))
      `((:string ,(template-error-string "error parsing {% for %}, it doesn't look like {% for X in XS %}...")))
      (let ((fs (mapcar 'compile-token clause))
	    (phrase (.parse-variable-phrase (string %listvar%))))
	(f0 (multiple-value-bind (list error-string) (.resolve-variable-phrase phrase)
	      (if error-string
		  (with-template-error error-string
		    (error error-string))
		  (let* ((length (length list))
			 (loopfor (list (cons :counter 1)
					(cons :counter0 0)
					(cons :revcounter length)
					(cons :revcounter0 (1- length))
					(cons :first t)
					(cons :last (= length 1))
					(cons :parentloop (get-variable :forloop))))
			 (*template-arguments* (list* var nil
						      :forloop loopfor
						      *template-arguments*))
			 (%set-this-cons% (rest *template-arguments*)))
		    (apply 'concatenate
			   'string
			   (mapcar 'princ-to-string
				   (mapcan (f_ ; set :var for use :for clause
					     (setf (first %set-this-cons%) _)

					; execute :for clause
					     (prog1 (mapcar 'funcall fs)
					 
					; update :loopfor
					       (incf (rest (assoc :counter loopfor)))
					       (incf (rest (assoc :counter0 loopfor)))
					       (decf (rest (assoc :revcounter loopfor)))
					       (decf (rest (assoc :revcounter0 loopfor)))
					       (setf (rest (assoc :first loopfor)) nil
						     (rest (assoc :last loopfor)) (zerop (rest (assoc :revcounter0 loopfor))))))
					   (if reversed
					       (reverse list)
					       list)))))))))))

; if / else / endif

(defun .split-if-clause (clause-tokens)
  "returns two values:

   1. all clause tokens that appear _before_ the first :ELSE token
   2. all clause tokens that appear _after_ the first :ELSE token"
  (let ((else (position-if (f_ (and (eql (first _) :tag)
				    (eql (second _) :else)))
			   clause-tokens)))
    (if else
	(values (subseq clause-tokens 0 else)
		(subseq clause-tokens (1+ else)))
	clause-tokens)))

(def-delimited-tag :if :endif :semi-parsed-if)

(def-token-processor :semi-parsed-if (args . clause) unprocessed
  ":SEMI-PARSED-IF tags are parsed into :PARSED-IF tags. a :PARSED-IF tag looks more 
ike a traditional IF statement [a test, an \"if\" branch, and an \"else\" branch], so
:SEMI-PARSED-IF has to look for the :ELSE token to split up `CLAUSE'"
  (multiple-value-bind (before-else after-else)
      (.split-if-clause clause)
    `((:parsed-if ,args ,before-else ,after-else) ,@(process-tokens unprocessed))))

(defun .compile-logical-statement (statement)
  "takes a \"logical statement\" like you would give {% if %} that has been parsed
into a list of keywords [eg: '(:not :foo) or '(:foo :and :baz) or
`(:foo.bar :or :list.1)] and turns them into a thunk predicate for dispatching the
conditional branching of the {% if %} tag. when called, the function returns two values:

   1. the value returned by resolving the phrase
   2. an error message string if something went wrong [ie, an invalid variable].
      [note: if return value 2 is present, then its probably not safe to consider return
       value 1 useful]"
  (labels ((% (%s)
	     "takes a \"logical statement\" [a list of keywords] minus any :OR or :AND tokens and
returns a list of thunks which, when called, return two values:

   1. whether or not the local statement is true or false
   2. an error-string if something went wrong"
	     (when %s
	       (destructuring-bind (1st . rest) %s
		 (if (eql 1st :not)
		     (let* ((2nd (first rest))
			    (phrase (.parse-variable-phrase (string 2nd))))
		       (cons (f0 (multiple-value-bind (ret error-string) 
				     (.resolve-variable-phrase phrase)
				   (values (not ret) error-string)))
			     (% (rest rest))))
		     (let ((phrase (.parse-variable-phrase (string 1st))))
		       (cons (f0 (.resolve-variable-phrase phrase))
			     (% rest))))))))
    (multiple-value-bind (fs error-string) (% (remove :and (remove :or statement)))
      (let ((and-token-seen-p (find :and statement)))
	(values (f0 (block <f0>
		      (flet ((! (_)
			       "takes a thunks and funcalls it, returning the 1st value. if there is a second value
it treats it as a template error string. see #'%"
			       (multiple-value-bind (ret error-string) (funcall _)
				 (if error-string
				     (return-from <f0> (values ret error-string))
				     ret))))
			(if and-token-seen-p
			    (every #'! fs)
			    (some #'! fs)))))
		error-string)))))

(def-token-compiler :parsed-if (statement then &optional else)
  ":PARSED-IF tags are compiled into a function that executes the {% if %} clause"
  (multiple-value-bind (test error-string)
      (.compile-logical-statement statement)

    (if error-string

	;; there was an error parsing the {% if %} tag [problably an invalid variable]
	;; return a thunk that signals or prints the template error
	(f0 (with-template-error error-string
	      (error error-string)))
	
	  ;; return the function that does the {% if %}
	(let ((then (mapcar 'compile-token then))
	      (else (mapcar 'compile-token else)))
	  (f0 (multiple-value-bind (ret error-string) (funcall test)
		(if error-string
		    (with-template-error error-string
		      (error error-string))
		    (.funcall-and-concatenate (if ret
						  then
						  else)))))))))

; ifchanged

; todo: add else tag, test

(def-delimited-tag :ifchanged :endifchanged :parsed-ifchanged)

(def-token-compiler :parsed-ifchanged (%keywords% . clause)
  (let ((memory (make-list (length %keywords%) :initial-element (gensym "virgin-ifchanged")))
	(fs (mapcar 'compile-token clause))
	(phrases (mapcar '.parse-variable-phrase (mapcar 'string %keywords%))))
    (f0 (block <f0>
	  (let ((new (mapcar (f_ (multiple-value-bind (ret error-string)
				     (.resolve-variable-phrase _)
				   (if error-string
				       (with-template-error (return-from <f0> error-string)
					 (error error-string))
				       ret)))
			     phrases)))
	    (if (every 'equalp memory new)
		""
		(prog1 (.funcall-and-concatenate fs)
		  (replace memory new))))))))

; ifequal / ifnotequal

(defun .process-ifequal-args (unparsed-string)
  (flet ((% (start)
	   (let ((s (string-trim '(#\space #\newline #\tab #\return)
				 (subseq unparsed-string start))))
	     (if (char= (char s 0) #\")
      
		 ;; is a hard-coded string
		 (read-from-string s)

		 ;; is a variable
		 (let ((end (or (position-if (f_ (or (char= _ #\space)
						     (char= _ #\tab)
						     (char= _ #\return)
						     (char= _ #\newline)
						     (char= _ #\")))
					     s)
				(length s))))
		   (values (.parse-variable-phrase (subseq s 0 end))
			   (1+ end)))))))
    (multiple-value-bind (a end-a) (% 0)
      (values a (% end-a)))))

(def-unparsed-tag-processor :ifequal (unparsed-string) rest
  (with-template-error `((:string ,(template-error-string "There was an error parsing the tag {% ifequal %}")))
    (multiple-value-bind (a b) (.process-ifequal-args unparsed-string)
      (process-tokens `((:tag :semi-parsed-ifequal ,a ,b) ,@rest)))))

(def-unparsed-tag-processor :ifnotequal (unparsed-string) rest
  (with-template-error `((:string ,(template-error-string "There was an error parsing the tag {% ifnotequal %}")))
    (multiple-value-bind (a b) (.process-ifequal-args unparsed-string)
      (process-tokens `((:tag :semi-parsed-ifnotequal ,a ,b) ,@rest)))))

(def-delimited-tag :semi-parsed-ifequal :endifequal :almost-parsed-ifequal)
(def-delimited-tag :semi-parsed-ifnotequal :endifnotequal :almost-parsed-ifnotequal)

(def-token-processor :almost-parsed-ifequal ((a b) . clauses) unprocessed
  (multiple-value-bind (before-else after-else) (.split-if-clause clauses)
    (process-tokens `((:parsed-ifequal ,a ,b ,before-else ,after-else) ,@unprocessed))))

(def-token-processor :almost-parsed-ifnotequal ((a b) . clauses) unprocessed
  (multiple-value-bind (before-else after-else) (.split-if-clause clauses)

    ;; from this point on {% ifnotequal %} is just like {% ifequal %},
    ;; the THEN and ELSE clauses have just been switched
    (process-tokens `((:parsed-ifequal ,a ,b ,after-else ,before-else) ,@unprocessed))))

(def-token-compiler :parsed-ifequal (a b then &optional else)
  (flet ((% (x)
	   (etypecase x
	     (string x)
	     (list (.resolve-variable-phrase x)))))
    

    ;; return a thunk that executes the {% ifequal %} clause
    (let ((then (mapcar 'compile-token then))
	  (else (mapcar 'compile-token else)))
      (f0 (multiple-value-bind (a-value a-error-string) (% a)
	    (multiple-value-bind (b-value b-error-string) (% b)

	      (or ;; if there is an error just return the error string
	          a-error-string
		  b-error-string

		  (.funcall-and-concatenate (if (equalp a-value b-value)
						then
						else)))))))))

; include

; todo: test

(def-tag-compiler :include (path)
  "when compiled, :INCLUDE tags first compile the template pointed to by `PATH' then
they compile into a function that simply calls this function with *TEMPLATE-ARGUMENTS*"
  (aif (find-template* path)
       (progn
	 (pushnew it *linked-files* :test 'equal)
	 (with-template-error (constantly (template-error-string "TThere was an error including the template ~A" it))
	   (if *template-ffc*
	       (f0 (apply 'cl-ffc:ffc-call *template-ffc* it *template-arguments*))
	       (.compile-template-string (fetch-template* it)))))
       (constantly (template-error-string "Cannot include the template ~A because it does not exist." path))))

; js / js-script / emit-js

(defvar *accumulated-javascript-strings*)

(def-unparsed-tag-processor :js (string) rest
  `((:parsed-js ,string) ,@(process-tokens rest)))

(def-token-compiler :parsed-js (string)
  (f0 (push (format nil "~%<script type='text/javascript' src=~A></script>" string)
	    *accumulated-javascript-strings*)
      ""))

(def-delimited-tag :js-script :endjs-script :semi-parsed-js-script)
(def-token-processor :semi-parsed-js-script  (% . processed-clause) rest
  (declare (ignore %))
  `((:parsed-js-script ,@processed-clause) ,@(process-tokens rest)))

(def-token-compiler :parsed-js-script (&rest clauses)
  (let ((compiled (mapcar 'compile-token clauses)))
    (f0 (push (format nil
		      "~%<script type='text/javascript'>~A</script>"
		      (.funcall-and-concatenate compiled))
	      *accumulated-javascript-strings*)
	"")))

(def-tag-compiler :emit-js ()
  (f0 (apply 'concatenate 'string (reverse *accumulated-javascript-strings*))))

; lisp

(def-unparsed-tag-processor :lisp (unparsed-string) rest
  (with-template-error (process-tokens `((:string ,(template-error-string "There was an error parsing the lisp statement ~S"
									  unparsed-string))
					 ,@rest))
    (let ((*package* (find-package :cl-user)))
      (process-tokens `((:parsed-lisp ,(read-from-string unparsed-string)) ,@rest)))))

(def-token-compiler :parsed-lisp (sexp)
  (with-template-error (constantly (template-error-string "There was an error compiling the lisp form ~S"
							  sexp))
    (let ((fn (compile nil (coerce `(lambda () ,sexp) 'function))))
      (f0 (if (not *eval-lisp-tags*)
	      (with-template-error #1=(template-error-string "I can't evaulate the {% lisp %} tag ~A because *EVAL-LISP-STRINGS* is NIL"
							     sexp)
				   (error #1#))
	      (with-template-error (template-error-string "There was an error executing the lisp form ~A"
							  sexp)
		(funcall fn)))))))

; show translation/example table

(def-tag-compiler :show-table (path)
  ":SHOW-TABLE tags compile into a function that return the html-escaped contents of
the file pointed to by the template-path `PATH'"
  (.with-file-handler (string path)
    (let ((escaped (djula-html-escape string)))
      (constantly escaped))))

; ssi

; todo: test

(def-tag-compiler :ssi (path &optional parse)
  "if `PATH' lives in a folder reckognized by *ALLOW-INCLUDE-ROOTS*, then :SSI tags
compile into a function that return the contents of the file pointed to
by the template-path `PATH'. If `PARSE' is T then the function renders `PATH' as a
template."
  (let ((path-string (namestring path)))
    (if (not #1=(find-if (f_ (eql (mismatch _ path-string :test 'char=)
				  (length _)))
			 *allow-include-roots*))
	(constantly (template-error-string "Cannot SSI to path ~A because ~A is not in a folder recognized by *ALLOW-INCLUDE-ROOTS*. Allowed folders are: ~A"
					   path-string
					   path-string
					   *allow-include-roots*)))
    (format-log "Danger! {% ssi ~A~A~A}"
		path
		(if parse
		    " "
		    "")
		(if parse
		    parse
		    ""))
    (if parse
	(compile-template path :recursivep t)      ; danger: will have screwy *CURRENT-TEMPLATE-ROOT-FOLDER*!!!!!
	(.with-file-handler (string path)
	  (constantly string)))))

; templatetag

(def-tag-compiler :templatetag (argument)
  ":SHOW-FILE tags compile into a function that return the html-escaped contents of
the file pointed to by the template-path `PATH'"
  (let ((string (case argument
		  (:openblock "{%")
		  (:closeblock "%}")
		  (:openvariable "{{")
		  (:closevariable "}}")
		  (:openbrace "{")
		  (:closebrace "}")
		  (:opencomment "{#")
		  (:closecomment "#}")
		  (:opentranslationvariable "{_")
		  (:closetranslationvariable "_}")
		  (otherwise (template-error-string "Unknown templatetag ~A. known template tags are: openblock, closeblock, openvariable, closevariable, openbrace, closebrace, opencomment, closecomment"
						    argument)))))
    (constantly string)))

; url

;; (def-unparsed-tag-processor :url (unparsed-string) rest
;;     (multiple-value-bind (method-name start-args)
;; 	(let ((*read-eval* nil)
;; 	      (*package* (find-package :keyword)))
;; 	  (read-from-string unparsed-string))

;;       (let* ((rest-string (subseq unparsed-string start-args))
;; 	     (method-args (if (find-if 'alphanumericp rest-string)
;; 			      (mapcar '.parse-variable-phrase
;; 				      (split-sequence:split-sequence #\space
;; 								     rest-string
;; 								     :remove-empty-subseqs t)))))
;; 	(process-tokens `((:parsed-url ,method-name ,@method-args) ,@rest)))))

;; (defun .plist-keys (plist)
;;   (when plist
;;     (cons (first plist) (.plist-keys (rest (rest plist))))))

;; (def-token-compiler :parsed-url (method-name . method-args)
;;   (f0 (let ((method (getf *url-method-plist* method-name)))
;; 	(if (null method)
;; 	    (f0 (template-error-string "No known {% url %} method for ~A. Known {% url %} methods are ~A"
;; 				       method-name
;; 				       (.plist-keys *url-method-plist*)))
;; 	    (block <url>
;; 	      (apply method
;; 		     (mapcar (f_ (multiple-value-bind (val error-string)
;; 				     (.resolve-variable-phrase _)
;; 				   (if error-string
;; 				       (return-from <url> error-string)
;; 				       val)))
;; 			     method-args)))))))

; with

;; (def-delimited-tag :parsed-with :endwith :parsed-with)
