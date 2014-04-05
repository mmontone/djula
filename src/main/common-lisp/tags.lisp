(in-package #:djula)

(defmacro with-file-handler ((string-var template-path) &body body)
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

(def-tag-processor :extends (template-path) rest-tokens
  (let ((real-path (find-template* template-path)))
    (pushnew real-path *linked-files* :test 'equal)
    (handler-case
        (when real-path
          (let* ((string (fetch-template* real-path))
                 (processed (process-tokens (parse-template-string string)))
                 (extend-blocks
                  (mapcar (lambda (x)
                            (destructuring-bind (<parsed-block> (name) . tokens)
                                x
                              (declare (ignore <parsed-block>))
                              (list* name tokens)))
                          (remove :parsed-block
                                  (process-tokens rest-tokens)
                                  :key #'first
                                  :test-not #'eq))))
            (setf *block-alist* (append *block-alist* extend-blocks))
            processed))
      (condition ()
        (template-error "Cannot extend the template ~A because there was an error parsing the template file ~A" template-path real-path)))))

(def-delimited-tag :block :endblock :parsed-block)

(def-token-compiler :parsed-block ((name) . block-tokens)
  (let* ((target (member name *block-alist* :key #'first :test #'eq))
	 (fs (mapcar #'compile-token (if target (rest (first target)) block-tokens))))
    (lambda (stream)
      (dolist (f fs)
        (funcall f stream)))))

(def-tag-compiler :super (name)
  ;; Pay attention to scoping here.  *BLOCK-ALIST* is dynamic and cannot be
  ;; refactored to inside the lambda.  Well, not with the desired results, anyway,
  (let* ((target (member name *block-alist* :key #'first :test #'eq))
	 (*block-alist* (if target (rest target) *block-alist*))
	 (fs (mapcar #'compile-token (if target (rest (first target)) nil))))
    (lambda (stream)
      (dolist (f fs)
        (funcall f stream)))))

(def-delimited-tag :comment :endcomment :comment-tag)

(def-token-processor :comment-tag (&rest %) rest-tokens
  (process-tokens rest-tokens))

(def-tag-compiler :cycle (&rest list)
  (let ((circle (copy-list list))
	(unique-id (gensym "cycle")))
    (setf (rest (last circle)) circle)
    (lambda (stream)
      (unless (getf *template-arguments* unique-id)
        (setf *template-arguments* (list* unique-id circle *template-arguments*)))
      (princ (pop (getf *template-arguments* unique-id)) stream))))

(def-tag-compiler :debug ()
  (lambda (out)
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
          (% "Language: ~S" (or *current-language* "none")))

        (with-safe "the default language"
          (% "Language: ~S" (or *default-language* "none")))

        (with-safe "whether or not template errors are printing to the browser"
          (% "~A" (if *catch-template-errors-p*
                      "Printing template errors in the browser"
                      "<<<Not printing template errors in the browser>>>")))

        (with-safe "the template error string if invalid"
          (if *template-string-if-invalid*
              (% "<<<Temlate string if invalid: ~S>>>" *template-string-if-invalid*)))

        (with-safe "*ALLOW-INCLUDE-ROOTS*"
          (% "Allow include-roots: ~A" *allow-include-roots*))

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

        (% "<<<END DEBUG INFO>>>")))))

(def-tag-compiler :set-language (name)
  ":SET-LANGUAGE tags are compiled into a function that set *CURRENT-LANGUAGE* to the
keyword version of `NAME' [or NIL if `NAME' is not supplied]"
  (lambda (stream)
    (declare (ignore stream))
    (setf *current-language* name)))


(def-tag-compiler :set-package (package-name)
  ":SET-PACKAGE tags are compiled into a function that set *DJULA-EXeCUTE-PACKAGE*
to the the package value of find package on the keyword `PACKAGE-NAME' or the 
package `common-lisp-user' if the package for `PACKAGE-NAME' is not found. This
is useful to determine the package in which :LISP tags are executed"
  (lambda (stream)
    (declare (ignore stream))
    (let ((temp-package (find-package package-name)))
      (if (packagep temp-package)
	  (setf *djula-execute-pacakge* temp-package)
	  (setf *djula-execute-pacakge* (find-package :common-lisp-user))))))


(def-tag-compiler :show-language ()
  ":SHOW-LANGUAGE tags are compiled into a function that just shows the values of
*CURRENT-LANGUAGE* or *DEFAULT-LANGUAGE* if there is no current language"
  (lambda (stream)
    (princ (or *current-language* *default-language*) stream)))

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
  (with-template-error
      (lambda ()
        (template-error-string "There was an error reading or parsing the contents of the translation table ~S"
                               template-path))
    (if (not (translation-table-p template-path))

	;; it doesn't look like a translation table, complain
	(lambda ()
          (template-error-string "the path ~S does not name a translation table! the names of translation table must match one of the following regular expressions: ~{~S ~}"
                                 template-path
                                 *translation-table-regexps*))

	(aif (find-template* template-path)
	     (progn
	       
	       (pushnew it *linked-files* :test 'equal)
	       (if (not (every 'listp (.read-table it)))

		   ;; it doesn't smell like a translation table, complain
		   (lambda ()
                     (template-error-string "The translation table ~A doesn't look like a translation table..." template-path))

		   ;; use the current contents of the translation table forever
		   (let ((compiled (compile-translation-table it)))
		     (lambda ()
                       (push (cons it compiled)
                             *known-translation-tables*)
                       ""))))

	     ;; the file doesn't exist, complain
	     (lambda ()
               (template-error-string "The translation table ~A doesn't exist!" it))))))

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
  (with-template-error
      (lambda ()
        (template-error-string "There was an error reading or parsing the contents of the example table ~S"
                               template-path))
    (if (not (example-table-p template-path))

	;; it doesn't look like an example table, complain
	(lambda ()
          (template-error-string "the path ~S does not name an example table! the name of an example table must match one of the following regular expressions: ~{~S ~}"
                                 template-path
                                 *example-table-regexps*))

	(aif (find-template* template-path)

	     (if (not (evenp (length (.read-table it))))

		 ;; it doesn't smell like an example table, complain
		 (lambda ()
                   (template-error-string "the example table ~S does not look like an example table!"
                                          template-path))

		 (let ((plist (.read-table it)))
                   (lambda ()
                     (push (cons it plist)
                           *known-example-tables*)
                     (if *use-example-values-p*
                         ""
                         (apply 'concatenate
                                'string
                                ""
                                (.check-example-table-plist plist))))))

	     ;; it doesn't exist, complain
	     (lambda ()
               (template-error-string "The example table ~A doesn't exist" template-path))))))

(def-unparsed-tag-processor :translation (unparsed-string) rest
  (process-tokens
   `((:parsed-translation ,@(.read-table-string unparsed-string))
     ,@rest)))

(def-token-compiler :parsed-translation (variable . language/value-plist)
  ":PARSED-TRANSLATION tags compile into a function that pushes the list
\(`LANGUAGE' NIL `VARIABLE-NAME' `VALUE') to *KNOWN-TRANSLATION-TABLES*, thus
letting GET-VARIABLE know about the variable `VARIABLE-NAME'"
  (let ((d (list nil (.compile-translation-table-variable (cons variable language/value-plist)))))
    (lambda (stream)
      (declare (ignore stream))
      (push d *known-translation-tables*))))

(def-unparsed-tag-processor :example (unparsed-string) rest
  (process-tokens
   `((:example ,@(.read-table-string unparsed-string))
     ,@rest)))

(def-token-compiler :parsed-example (&rest variable/value-plist &key &allow-other-keys)
  ":PARSED-EXAMPLE compiles into a thunk that pushes

   (NIL . `VARIABLE/VALUE-PLIST')

to *KNOWN-EXAMPLE-TABLES*. if *USE-EXAMPLE-VALUES-P* is NULL then it checks to make
sure all the variables in `VARIABLE/VALUE-PLIST' are in *TEMPLATE-ARGUMENTS*"
  (let ((d (cons nil variable/value-plist)))
    (lambda (stream)
      (push d *known-example-tables*)
      (unless *use-example-values-p*
        (princ (.check-example-table-plist variable/value-plist) stream)))))

(def-delimited-tag :semi-parsed-filter :endfilter :parsed-filter)

(def-unparsed-tag-processor :filter (unparsed-string) rest
  (process-tokens
   `((:tag :semi-parsed-filter ,@(rest (.parse-variable-clause (format nil "INTERNAL|~A" unparsed-string))))
     ,@rest)))

(def-token-compiler :parsed-filter (filters . clauses)
  (let ((fs (mapcar #'compile-token clauses)))
    (lambda (stream)
      (princ
       (apply-filters
        (with-output-to-string (s)
          (dolist (f fs)
            (funcall f s)))
        filters)
       stream))))

(def-delimited-tag :for :endfor :parsed-for)

(def-token-compiler :parsed-for ((var in %listvar% &optional reversed) . clause)
  (if (not (eql in :in))
      (list
       (list :string (template-error-string "error parsing {% for %}, it doesn't look like {% for X in XS %}...")))
      (let ((fs (mapcar #'compile-token clause))
	    (phrase (parse-variable-phrase (string %listvar%))))
	(lambda (stream)
          (multiple-value-bind (list error-string)
              (resolve-variable-phrase phrase)
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
                       (*template-arguments*
                        ;; NIL is a placeholder for the value of the loop variable.
                        (list* var nil :forloop loopfor *template-arguments*)))
                  (dolist (x (if reversed (reverse list) list))
                    ;; Update the value of the loop variable.
                    (setf (cadr *template-arguments*) x)
                    (dolist (f fs)
                      (funcall f stream))
                    (incf (cdr (assoc :counter loopfor)))
                    (incf (cdr (assoc :counter0 loopfor)))
                    (decf (cdr (assoc :revcounter loopfor)))
                    (decf (cdr (assoc :revcounter0 loopfor)))
                    (setf (cdr (assoc :first loopfor)) nil
                          (cdr (assoc :last loopfor)) (zerop (cdr (assoc :revcounter0 loopfor))))))))))))

(defun split-if-clause (clause-tokens)
  "returns two values:

   1. all clause tokens that appear _before_ the first :ELSE token
   2. all clause tokens that appear _after_ the first :ELSE token"
  (let ((else (position-if
               (lambda (x)
                 (and (eql (first x) :tag)
                      (eql (second x) :else)))
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
      (split-if-clause clause)
    (cons (list :parsed-if args before-else after-else)
          (process-tokens unprocessed))))

(defun compile-logical-statement (statement)
  "takes a \"logical statement\" like you would give {% if %} that has been parsed
into a list of keywords [eg: '(:not :foo) or '(:foo :and :baz) or
`(:foo.bar :or :list.1)] and turns them into a thunk predicate for dispatching the
conditional branching of the {% if %} tag. when called, the function returns two values:

   1. the value returned by resolving the phrase
   2. an error message string if something went wrong [ie, an invalid variable].
      [note: if return value 2 is present, then its probably not safe to consider return
       value 1 useful]"
  (labels ((resolvers (list)
	     "takes a \"logical statement\" [a list of keywords] minus any :OR or :AND tokens and
returns a list of thunks which, when called, return two values:

   1. whether or not the local statement is true or false
   2. an error-string if something went wrong"
	     (when list
	       (destructuring-bind (head . tail)
                   list
		 (if (eql head :not)
		     (let* ((2nd (first tail))
			    (phrase (parse-variable-phrase (string 2nd))))
		       (cons (lambda ()
                               (multiple-value-bind (ret error-string) 
                                   (resolve-variable-phrase phrase)
                                 (values (not ret) error-string)))
			     (resolvers (rest tail))))
		     (let ((phrase (parse-variable-phrase (string head))))
		       (cons (lambda ()
                               (resolve-variable-phrase phrase))
			     (resolvers tail))))))))
    (multiple-value-bind (fs error-string)
        (resolvers (remove :and (remove :or statement)))
      (let ((and-token-seen-p (find :and statement)))
	(values (lambda ()
                  (block evaluate-logical-statement
                    (flet ((evaluate (statement)
                             "takes a thunks and funcalls it, returning the 1st value. if there is a second value
it treats it as a template error string. see #'%"
                             (multiple-value-bind (ret error-string)
                                 (funcall statement)
                               (if error-string
                                   (return-from evaluate-logical-statement (values ret error-string))
                                   ret))))
                      (if and-token-seen-p
                          (every #'evaluate fs)
                          (some #'evaluate fs)))))
		error-string)))))

(def-token-compiler :parsed-if (statement then &optional else)
  ":PARSED-IF tags are compiled into a function that executes the {% if %} clause"
  (multiple-value-bind (test error-string)
      (compile-logical-statement statement)
    (if error-string
	;; there was an error parsing the {% if %} tag [problably an invalid variable]
	;; return a thunk that signals or prints the template error
	(lambda (stream)
          (with-template-error error-string
            (error error-string)))
        ;; return the function that does the {% if %}
	(let ((then (mapcar #'compile-token then))
	      (else (mapcar #'compile-token else)))
	  (lambda (stream)
            (multiple-value-bind (ret error-string)
                (funcall test)
              (if error-string
                  (with-template-error error-string
                    (error error-string))
                  (dolist (f (if ret then else))
                    (funcall f stream)))))))))

(def-delimited-tag :ifchanged :endifchanged :parsed-ifchanged)

(def-token-compiler :parsed-ifchanged (%keywords% . clause)
  (let ((memory (make-list (length %keywords%) :initial-element (gensym "virgin-ifchanged")))
	(fs (mapcar #'compile-token clause))
	(phrases (mapcar #'parse-variable-phrase (mapcar 'string %keywords%))))
    (lambda (stream)
      (block <f0>
        (let ((new (mapcar (lambda (x)
                             (multiple-value-bind (ret error-string)
                                 (resolve-variable-phrase x)
                               (if error-string
                                   (with-template-error (return-from <f0> error-string)
                                     (error error-string))
                                   ret)))
                           phrases)))
          (unless (every #'equalp memory new)
            (dolist (f fs)
              (funcall f stream))
            (replace memory new)))))))

(defun process-ifequal-args (unparsed-string)
  (flet ((% (start)
	   (let ((s (string-trim '(#\space #\newline #\tab #\return)
				 (subseq unparsed-string start))))
	     (if (char= (char s 0) #\")
		 ;; is a hard-coded string
		 (read-from-string s)
		 ;; is a variable
		 (let ((end (or (position-if (lambda (x)
                                               (or (char= x #\space)
                                                   (char= x #\tab)
                                                   (char= x #\return)
                                                   (char= x #\newline)
                                                   (char= x #\")))
					     s)
				(length s))))
		   (values (parse-variable-phrase (subseq s 0 end))
			   (1+ end)))))))
    (multiple-value-bind (a end-a)
        (% 0)
      (values a (% end-a)))))

(def-unparsed-tag-processor :ifequal (unparsed-string) rest
  (with-template-error
      `((:string ,(template-error-string "There was an error parsing the tag {% ifequal %}")))
    (multiple-value-bind (a b)
        (process-ifequal-args unparsed-string)
      (process-tokens `((:tag :semi-parsed-ifequal ,a ,b) ,@rest)))))

(def-unparsed-tag-processor :ifnotequal (unparsed-string) rest
  (with-template-error
      `((:string ,(template-error-string "There was an error parsing the tag {% ifnotequal %}")))
    (multiple-value-bind (a b)
        (process-ifequal-args unparsed-string)
      (process-tokens `((:tag :semi-parsed-ifnotequal ,a ,b) ,@rest)))))

(def-delimited-tag :semi-parsed-ifequal :endifequal :almost-parsed-ifequal)
(def-delimited-tag :semi-parsed-ifnotequal :endifnotequal :almost-parsed-ifnotequal)

(def-token-processor :almost-parsed-ifequal ((a b) . clauses) unprocessed
  (multiple-value-bind (before-else after-else)
      (split-if-clause clauses)
    (process-tokens `((:parsed-ifequal ,a ,b ,before-else ,after-else) ,@unprocessed))))

(def-token-processor :almost-parsed-ifnotequal ((a b) . clauses) unprocessed
  (multiple-value-bind (before-else after-else)
      (split-if-clause clauses)
    ;; from this point on {% ifnotequal %} is just like {% ifequal %},
    ;; the THEN and ELSE clauses have just been switched
    (process-tokens `((:parsed-ifequal ,a ,b ,after-else ,before-else) ,@unprocessed))))

(def-token-compiler :parsed-ifequal (a b then &optional else)
  (flet ((% (x)
	   (etypecase x
	     (string x)
	     (list (resolve-variable-phrase x)))))
    ;; return a thunk that executes the {% ifequal %} clause
    (let ((then (mapcar #'compile-token then))
	  (else (mapcar #'compile-token else)))
      (lambda (stream)
        (multiple-value-bind (a-value a-error-string)
            (% a)
          (multiple-value-bind (b-value b-error-string)
              (% b)
            (or
             a-error-string
             b-error-string
             (dolist (f (if (equalp a-value b-value)
                            then
                            else))
               (funcall f stream)))))))))

(def-tag-compiler :include (path)
  "when compiled, :INCLUDE tags first compile the template pointed to by `PATH' then
they compile into a function that simply calls this function with *TEMPLATE-ARGUMENTS*"
  (aif (find-template* path)
       (progn
	 (pushnew it *linked-files* :test 'equal)
	 (handler-case
             (compile-template* path)
           (error ()
             (template-error "There was an error including the template ~A" it))))
       (template-error "Cannot include the template ~A because it does not exist." path)))

(defvar *accumulated-javascript-strings* nil)

(def-unparsed-tag-processor :js (string) rest
  (cons (list :parsed-js string)
        (process-tokens rest)))

(def-token-compiler :parsed-js (string)
  (lambda (stream)
    (declare (ignore stream))
    (push (format nil "~%<script type='text/javascript' src=~S></script>" string)
          *accumulated-javascript-strings*)))

(def-delimited-tag :js-script :endjs-script :semi-parsed-js-script)

(def-token-processor :semi-parsed-js-script  (_ . processed-clause) rest
  (declare (ignore _))
  (cons (list* :parsed-js-script processed-clause)
        (process-tokens rest)))

(def-token-compiler :parsed-js-script (&rest clauses)
  (let ((compiled (mapcar #'compile-token clauses)))
    (lambda (stream)
      (declare (ignore stream))
      (push (with-output-to-string (s)
              (format s "~%<script type='text/javascript'>")
              (dolist (f compiled)
                (funcall f s))
              (format s "</script>"))
            *accumulated-javascript-strings*))))

(def-tag-compiler :emit-js ()
  (lambda (stream)
    (dolist (js (nreverse *accumulated-javascript-strings*))
      (princ js stream))))

(def-unparsed-tag-processor :lisp (unparsed-string) rest
  (handler-case
      (process-tokens
       (cons (list :parsed-lisp
                   (let ((*package* *djula-execute-pacakge*))
                     (read-from-string unparsed-string)))
             rest))
    (error ()
      (template-error "There was an error parsing the lisp statement ~S" unparsed-string))))

(def-token-compiler :parsed-lisp (sexp)
  (handler-case
      (let ((fn (compile nil (coerce `(lambda () ,sexp) 'function))))
        (lambda (stream)
          (if (not *eval-lisp-tags*)
              (template-error "I can't evaulate the {% lisp %} tag ~A because *EVAL-LISP-STRINGS* is NIL" sexp)
              (handler-case
		  (princ (funcall fn) stream)
		(error ()
		  (template-error "There was an error executing the lisp form ~A" sexp))))))
    (error ()
      (template-error "There was an error executing the lisp form ~A" sexp))))

(def-tag-compiler :show-table (path)
  ":SHOW-TABLE tags compile into a function that return the html-escaped contents of
the file pointed to by the template-path `PATH'"
  (with-file-handler (string path)
    (let ((escaped (html-escape string)))
      (lambda (stream)
        (princ escaped stream)))))

(def-tag-compiler :ssi (path &optional parse)
  "if `PATH' lives in a folder reckognized by *ALLOW-INCLUDE-ROOTS*, then :SSI tags
compile into a function that return the contents of the file pointed to
by the template-path `PATH'. If `PARSE' is T then the function renders `PATH' as a
template."
  (let ((path-string (namestring path)))
    (if (not (find-if (lambda (x)
                        (eql (mismatch x path-string :test 'char=)
                             (length x)))
                      *allow-include-roots*))
	(lambda (stream)
          (princ
           (template-error-string
            "Cannot SSI to path ~A because ~A is not in a folder recognized by *ALLOW-INCLUDE-ROOTS*. Allowed folders are: ~A"
            path-string
            path-string
            *allow-include-roots*)
           stream)))
    (if parse
	(compile-template path :recursivep t)
	(with-file-handler (string path)
          (lambda (stream)
            (princ string stream))))))

(def-tag-compiler :templatetag (argument)
  ":SHOW-FILE tags compile into a function that return the html-escaped contents of
the file pointed to by the template-path `PATH'"
  (let ((string
         (case argument
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
           (otherwise
            (template-error-string "Unknown templatetag ~A. known template tags are: openblock, closeblock, openvariable, closevariable, openbrace, closebrace, opencomment, closecomment" argument)))))
    (lambda (stream)
      (princ string stream))))
