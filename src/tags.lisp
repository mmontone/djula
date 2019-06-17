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
  (labels ((blocks-list (tokens)
             (mapcar (lambda (x)
                       (destructuring-bind (<parsed-block> (name) . tokens)
                           x
                         (declare (ignore <parsed-block>))
                         (list* name tokens)))
                     (remove :parsed-block
                             tokens
                             :key #'first
                             :test-not #'eq)))
           (flattened-blocks-list (tokens)
             (flet ((block-children (x)
                      (remove :parsed-block
                              (destructuring-bind (<parsed-block> (name) . tokens)
                                  x
                                (declare (ignore name <parsed-block>))
                                tokens)
                              :key #'first
                              :test-not #'eq)))
               (when tokens
                 (let ((top-level-blocks (remove :parsed-block
                                                 tokens
                                                 :key #'first
                                                 :test-not #'eq)))
                   (append
                    (blocks-list tokens)
                    (flattened-blocks-list
                     (apply #'append
                            (mapcar #'block-children
                                    top-level-blocks)))))))))
    (let ((real-path (find-template* template-path)))
      (pushnew (compile-template* real-path) *linked-templates*
               :test 'equal
               :key 'template-file)
      (handler-case
          (when real-path
            (let* ((string (fetch-template* real-path))
                   (processed (process-tokens (parse-template-string string)))
                   (super-blocks (flattened-blocks-list processed))
                   (extend-blocks (blocks-list (process-tokens rest-tokens))))
              (setf *block-alist* (if *block-alist*
                                      (append extend-blocks *block-alist*)
                                      (append extend-blocks super-blocks)))
              processed))
        (condition (e)
          (template-error* "Cannot extend the template ~A because there was an error parsing the template file ~A" e template-path real-path))))))

(def-delimited-tag :block :endblock :parsed-block)

(def-token-compiler :parsed-block ((name) . block-tokens)
  (let ((*current-block* name))
    (let* ((target (member name *block-alist* :key #'first :test #'eq))
           (fs (mapcar #'compile-token (if target (rest (first target)) block-tokens))))
      (lambda (stream)
        (dolist (f fs)
          (funcall f stream))))))

(defun remove-first (item sequence &key (test #'eql) (key #'identity))
  "Removes the first ocurrence of item in sequence"
  (loop
     :with removed-p = nil
     :for x :in sequence
     :when (or removed-p (not (funcall test (funcall key x) item)))
     :collect x
     :when (funcall test (funcall key x) item)
     :do (setf removed-p t)))

(def-tag-compiler :super (&optional name)
  (let* ((super-block-name (or name *current-block*
                               (template-error "No parent block")))
         (target (or
                  (second (remove-if-not (lambda (block)
                                           (equalp super-block-name (first block)))
                                         *block-alist*))
                  (template-error "Parent block ~A not found" (or name *current-block*))))
         (*block-alist* (if target
                            (remove-first (first target) *block-alist* :key #'first :test #'equalp)
                            *block-alist*))
         (fs (when target
               (mapcar #'compile-token (rest target)))))
    (lambda (stream)
      (dolist (f fs)
        (funcall f stream)))))

(def-delimited-tag :comment :endcomment :comment-tag)

(def-token-processor :comment-tag (&rest %) rest-tokens
  (process-tokens rest-tokens))

(def-tag-compiler :firstof (&rest list)
  (flet ((% (x)
           (etypecase x
             (string x)
             (number x)
             (symbol (resolve-variable-phrase (list x))))))
    (lambda (stream)
      (dolist (item list)
        (multiple-value-bind (val val-error)
            (% item)
          (when (or val val-error)
            (princ (or val-error val)
                   stream)
            (return)))))))

(def-tag-compiler :cycle (&rest list)
  (let ((circle (copy-list list))
        (unique-id (gensym "cycle")))
    ;; Make the circle list circular
    (setf (cdr (last circle)) circle)
    (lambda (stream)
      (unless (getf *template-arguments* unique-id)
        (setf *template-arguments* (list* unique-id circle *template-arguments*)))
      (let ((cycle-item (pop (getf *template-arguments* unique-id))))
        (let ((cycle-item-value
               (if (symbolp cycle-item)
                   (getf *template-arguments* cycle-item)
                   cycle-item)))
          (princ cycle-item-value stream))))))

(defun print-debugging-information (out)
  (flet ((% (fmt-string &rest fmt-args)
           (terpri out)
           (apply 'format out fmt-string fmt-args)))
    (macrolet ((with-safe (about &body body)
                 `(with-template-error (% "<<<There was an error gathering debug information about ~A>>>" ,about)
                    ,@body)))
      (% "<<<START DEBUG INFO>>>")

      (with-safe "the default language"
        (% "Default language: ~A" (or *default-language* "none")))

      (with-safe "the current language"
        (% "Current language: ~A" (or *current-language* "none")))

      (with-safe "the current lisp execution package"
        (% "Lisp execution package: ~A" (or *djula-execute-package* "none")))

      (with-safe "whether or not template errors are printing to the browser"
        (% "~A" (if *catch-template-errors-p*
                    "Printing template errors in the browser"
                    "<<<Not printing template errors in the browser>>>")))

      (with-safe "whether or not template errores are verbose"
        (% "~A" (if *verbose-errors-p*
                    "Signaling verbose errors"
                    "<<<Not signaling verbose errors>>>")))

      (with-safe "whether or not fancy errors are active"
        (% "~A" (if (and *catch-template-errors-p*
                         *fancy-error-template-p*)
                    "Fancy template on errors is enabled"
                    "<<<Fancy error template is disabled>>>")))

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
                               (% "   ~A. ~A = ~A" (incf n) k (truncate-characters v 25 "..."))
                               (rfn rest)))))
                  (rfn *template-arguments*))))))

      (% "<<<END DEBUG INFO>>>"))))

(defun print-fancy-debugging-information (stream)
  (macrolet ((with-safe (about &body body)
               `(with-template-error (format stream "<<<There was an error gathering debug information about ~A>>>" ,about)
                  ,@body)))
    (format stream "<div class=\"debug\" style=\"position:fixed;bottom:0;font-size:12px;height:100px;overflow-y:auto;background-color:white;\">")
    (format stream "<ul style=\"list-style-type:none;\">")

    (with-safe "the default language"
      (format stream "<li><b>Default language:</b> ~A </li>" (or *default-language* "none")))

    (with-safe "the current language"
      (format stream "<li><b>Current language:</b> ~A</li>" (or *current-language* "none")))

    (with-safe "the current lisp execution package"
      (format stream "<li><b>Lisp execution package:</b> ~A</li>"
              (escape-for-html (princ-to-string (or *djula-execute-package* "none")))))

    (with-safe "whether or not template errors are printing to the browser"
      (format stream "<li><b>Print errors in browser:</b> ~A</li>"
              (if *catch-template-errors-p*
                  "yes" "no")))

    (with-safe "whether or not template errores are verbose"
      (format stream "<li><b>Verbose errors:</b> ~A</li>"
              (if *verbose-errors-p* "yes" "no")))

    (with-safe "whether or not fancy errors are active"
      (format stream
              "<li><b>Fancy errors:</b> ~A</li>"
              (if (and *catch-template-errors-p*
                       *fancy-error-template-p*)
                  "enabled" "disabled")))

    (with-safe "*ALLOW-INCLUDE-ROOTS*"
      (format stream "<li><b>Allow include-roots:</b> ~A</li>" *allow-include-roots*))

    (with-safe "the arguments given to the template"
      (format stream "<li><b>Template arguments:</b> ")
      (if (null *template-arguments*)
          (format stream "There were no arguments given to the template")
          (let ((n 0))
            (labels ((rfn (plist)
                       (when plist
                         (destructuring-bind (k v . rest) plist
                           (format stream "   ~A. ~A = ~A" (incf n) k (escape-for-html (truncate-characters v 25 "...")))
                           (rfn rest)))))
              (rfn *template-arguments*)))))
    (format stream "</ul>")
    (format stream "</div>")))

(def-tag-compiler :debug ()
  (lambda (stream)
    (if *fancy-debug-p*
        (print-fancy-debugging-information stream)
        (print-debugging-information stream))))

(def-tag-compiler :set-language (name)
  ":SET-LANGUAGE tags are compiled into a function that set *CURRENT-LANGUAGE* to the
keyword version of `NAME' [or NIL if `NAME' is not supplied]"
  (lambda (stream)
    (declare (ignore stream))
    (setf *current-language* name)))

(def-tag-compiler :set-package (package-name)
  ":SET-PACKAGE tags are compiled into a function that set *DJULA-EXECUTE-PACKAGE*
to the the package value of find package on the keyword `PACKAGE-NAME' or the
package `common-lisp-user' if the package for `PACKAGE-NAME' is not found. This
is useful to determine the package in which :LISP tags are executed"
  (lambda (stream)
    (declare (ignore stream))
    (let ((temp-package (find-package package-name)))
      (if (packagep temp-package)
          (setf *djula-execute-package* temp-package)
          (setf *djula-execute-package* (find-package :common-lisp-user))))))

(def-tag-compiler :show-language ()
  ":SHOW-LANGUAGE tags are compiled into a function that just shows the values of
*CURRENT-LANGUAGE* or *DEFAULT-LANGUAGE* if there is no current language"
  (lambda (stream)
    (princ (or *current-language* *default-language*) stream)))

(def-delimited-tag :semi-parsed-filter :endfilter :parsed-filter)

(def-unparsed-tag-processor :filter (unparsed-string) rest
  (process-tokens
   `((:tag :semi-parsed-filter ,@(rest (parse-variable-clause (format nil "INTERNAL|~A" unparsed-string))))
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

(defun iterable-list (iterable)
  (typecase iterable
    (array (coerce iterable 'list))
    (hash-table (alexandria:hash-table-alist iterable))
    (sequence iterable)
    (t (error "Cannot iterate on ~A" iterable))))

(def-token-compiler :parsed-for ((var in %listvar% &optional reversed) . clause)
  (if (not (eql in :in))
      (list
       (list :string (template-error-string "error parsing {% for %}, it doesn't look like {% for X in XS %}...")))
      (let ((fs (mapcar #'compile-token clause))
            (phrase (parse-variable-phrase (string %listvar%))))
        (lambda (stream)
          (multiple-value-bind (iterable error-string)
              (resolve-variable-phrase phrase)
            (if error-string
                (with-template-error error-string
                  (error error-string))
                (let* ((list (iterable-list iterable))
                       (length (length list))
                       (loopfor (list (cons :counter 1)
                                      (cons :counter0 0)
                                      (cons :revcounter length)
                                      (cons :revcounter0 (1- length))
                                      (cons :first t)
                                      (cons :last (= length 1))
                                      (cons :parentloop (get-variable :forloop))))
                       (*template-arguments*
                        ;; NIL is a placeholder for the value of the loop variable.
                        (if (consp var)
                            (list* (car var) nil (cdr var) nil :forloop loopfor *template-arguments*)
                            (list* var nil :forloop loopfor *template-arguments*))))
                  (dolist (x (if reversed (reverse list) list))
                    ;; Update the value of the loop variable.
                    (if (consp var)
                        (progn
                          (setf (getf *template-arguments* (car var)) (car x))
                          (setf (getf *template-arguments* (cdr var)) (cdr x)))
                        (setf (getf *template-arguments* var) x))
                    (dolist (f fs)
                      (funcall f stream))
                    (incf (cdr (assoc :counter loopfor)))
                    (incf (cdr (assoc :counter0 loopfor)))
                    (decf (cdr (assoc :revcounter loopfor)))
                    (decf (cdr (assoc :revcounter0 loopfor)))
                    (setf (cdr (assoc :first loopfor)) nil
                          (cdr (assoc :last loopfor))
                          (zerop (cdr (assoc :revcounter0 loopfor))))))))))))

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
  (let ((parsed-statement (parse-sequence* (boolexp-parser)
                                           statement
                                           :complete t
                                           )))
    (when (not parsed-statement)
      (template-error "Invalid boolean expression: ~A" statement))
    (values (lambda ()
              (compile-boolexp parsed-statement))
            nil)))

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
  (let ((fs (mapcar #'compile-token clause))
        (phrases (mapcar #'parse-variable-phrase (mapcar 'string %keywords%))))
    (lambda (stream)
      (block <f0>
        (let* ((forloop (get-variable :forloop))
               (memory (or (cdr (assoc :if-changed forloop))
                           (let ((initial-memory (make-list (length %keywords%)
                                                            :initial-element (gensym "virgin-ifchanged"))))
                             (push (cons :if-changed initial-memory)
                                   (getf *template-arguments* :forloop))
                             initial-memory)))
               (new (mapcar (lambda (x)
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
            (setf (cdr (assoc :if-changed (getf *template-arguments* :forloop)))
                  new)
            ))))))

(defun process-ifequal-args (unparsed-string)
  (flet ((% (start)
           (let ((s (string-trim '(#\space #\newline #\tab #\return)
                                 (subseq unparsed-string start))))
             (cond
               ((char= (char s 0) #\")
                ;; is a hard-coded string
                (read-from-string s))
               ((member (char s 0)
                        (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                        :test #'char=)
                ;; it is a number
                (let* ((num-member (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\.))
                       (end (or (position-if-not (lambda (x)
                                                   (member x num-member
                                                           :test #'char=))
                                                 s)
                                (length s))))
                  (values (read-from-string (subseq s 0 end))
                          (1+ end))))
               (t
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
                          (1+ end))))))))
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
             (number x)
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

(def-tag-compiler :include (path &rest parameters)
  "when compiled, :INCLUDE tags first compile the template pointed to by `PATH' then
they compile into a function that simply calls this function with *TEMPLATE-ARGUMENTS*

`PARAMETERS' should have the form (:param1 value1 :param2 value2 ...). If given, they
are prepended to *TEMPLATE-ARGUMENTS*"
  (flet ((template-with-parameters (template)
           (lambda (stream)
             (let ((*template-arguments*
                    (append (loop
                               :for (var value) :on parameters :by #'cddr
                               :collect var
                               :collect (etypecase value
                                          (symbol (case value
                                                    (:t t)
                                                    (:nil nil)
                                                    (t (resolve-variable-phrase (parse-variable-phrase (string value))))))
                                          (string value)
                                          (number value)))
                            *template-arguments*)))
               (funcall template stream)))))
    (cond
      ((stringp path)
       (aif (find-template* path)
            (handler-case
                (let ((template (compile-template* it)))
                  (pushnew template *linked-templates*
                           :test 'equal
                           :key 'template-file)
                  (template-with-parameters template))
              (error ()
                (template-error "There was an error including the template ~A" it)))
            ;; else
            (template-error "Cannot include the template ~A because it does not exist." path)))
      ((keywordp path)
       (template-with-parameters
        (lambda (stream)
          (let ((path (resolve-variable-phrase (parse-variable-phrase (string path)))))
            (aif (and path (find-template* path))
                 (let ((compiled-template
                        (handler-case
                            (compile-template* path)
                          (error ()
                            (template-error "There was an error including the template ~A" it)))))
                   (funcall compiled-template stream))
                 ;; else
                 (template-error "Cannot include the template ~A because it does not exist." path))))))
      (t (error "Invalid include template path: ~A" path)))))

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
                   (let ((*package* *djula-execute-package*))
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
                (error (e)
                  (template-error* e "There was an error executing the lisp form ~A" sexp))))))
    (error (e)
      (template-error* e "There was an error executing the lisp form ~A" sexp))))

(def-unparsed-tag-processor :set (unparsed-string) rest
  (handler-case
      (let* ((eq-position (position #\= unparsed-string))
             (var-str (subseq unparsed-string 0 eq-position))
             (value-str (subseq unparsed-string (1+ eq-position))))
        (process-tokens
         (cons (list :parsed-set
                     (make-keyword (string-upcase (string-trim (list #\space ) var-str)))
                     (let ((*package* *djula-execute-package*))
                       (read-from-string value-str)))
               rest)))
    (error ()
      (template-error "There was an error parsing the lisp statement ~S" unparsed-string))))

(def-token-compiler :parsed-set (var value)
  (handler-case
      (let ((fn (compile nil (coerce `(lambda () ,value) 'function))))
        (lambda (stream)
          (declare (ignore stream))
          (handler-case
              (setf (getf *template-arguments* var) (funcall fn))
            (error (e)
              (template-error* e "There was an error executing the lisp form ~A" value)))))
    (error (e)
      (template-error* e "There was an error executing the lisp form ~A" value))))


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

(def-delimited-tag :autoescape :endautoescape :parsed-autoescape)

(def-token-compiler :parsed-autoescape ((autoescape-enabled) . block-tokens)
  (let* ((autoescape-p (cond
                         ((member autoescape-enabled (list :yes :on))
                          t)
                         ((member autoescape-enabled (list :no :off))
                          nil)
                         (t (error "Invalid argument ~A in autoescape" autoescape-enabled))))
         (fs (let ((*auto-escape* autoescape-p))
               (mapcar #'compile-token block-tokens))))
    (lambda (stream)
      (dolist (f fs)
        (funcall f stream)))))

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

;; Boolean expressions parser

(def-cached-arg-parser transform (transform)
  "Parser: transform and return the result, when the transformation applies (not null)"
  #'(lambda (inp)
      (typecase inp
        (end-context (constantly nil))
        (parser-combinators::context
         (let ((result (funcall transform (parser-combinators::context-peek inp))))
           (if result
               (let ((closure-value
                      (make-instance 'parser-combinators::parser-possibility
                                     :tree result :suffix (parser-combinators::context-next inp))))
                 #'(lambda ()
                     (when closure-value
                       (prog1
                           closure-value
                         (setf closure-value nil)))))
               (constantly nil)))))))

(defun in-list (parser)
  (transform
   (lambda (x)
     (and (listp x)
          (parse-sequence* parser x
                           :complete t
                           )))))

(defun boolexp-parser ()
  (named? boolexp
    (choices
     (bterm boolexp)
     (not-bfactor boolexp))))

(defun not-bfactor (boolexp)
  (choice
   (seq-list? :not (boolexp-factor boolexp))
   (boolexp-factor boolexp)))

(defun boolexp-factor (boolexp)
  (choices
   (boolean-comparison)
   (bliteral)
   (in-list boolexp)))

(defun bliteral ()
  (choices
   (sat #'symbolp)
   (sat #'stringp)
   (sat #'integerp)))

(defun boolean-comparison ()
  (named-seq?
   (<- e1 (bliteral))
   (<- op (comparison-operator-parser))
   (<- e2 (bliteral))
   (list op e1 e2)))

(defun bterm (boolexp)
  (choices
   (or-bterm boolexp)
   (and-bterm boolexp)))

(defun or-bterm (boolexp)
  (named-seq? (<- exp (and-bterm boolexp))
              (<- exps
                  (many1? (named-seq? :or
                                      (<- exp (and-bterm boolexp))
                                      exp)))
              (append (list :or exp) exps)))

(defun and-bterm (boolexp)
  (choice
   (named-seq? (<- exp (not-bfactor boolexp))
               (<- exps
                   (many1? (named-seq? :and
                                       (<- exp (not-bfactor boolexp))
                                       exp)))
               (append (list :and exp) exps))
   (not-bfactor boolexp)))

(defun comparison-operator-parser ()
  (choices :==
           :!=
           :<>
           :/=
           :>
           :>=
           :<
           :<=))

(defun compile-boolexp (bexp)
  (cond
    ((symbolp bexp)
     (case bexp
       (:true t)
       (:false nil)
       (t (resolve-variable-phrase (parse-variable-phrase (string bexp))))))
    ((integerp bexp)
     bexp)
    ((stringp bexp)
     bexp)
    ((listp bexp)
     (ecase (first bexp)
       (:and (every #'compile-boolexp (rest bexp)))
       (:or (some #'compile-boolexp (rest bexp)))
       (:not (not (compile-boolexp (second bexp))))
       (:< (< (compile-boolexp (second bexp))
              (compile-boolexp (third bexp))))
       (:<= (<= (compile-boolexp (second bexp))
                (compile-boolexp (third bexp))))
       (:> (> (compile-boolexp (second bexp))
              (compile-boolexp (third bexp))))
       (:>= (>= (compile-boolexp (second bexp))
                (compile-boolexp (third bexp))))
       (:== (equalp (compile-boolexp (second bexp))
                    (compile-boolexp (third bexp))))
       (:!= (not (equalp (compile-boolexp (second bexp))
                         (compile-boolexp (third bexp)))))
       (:<> (not (equalp (compile-boolexp (second bexp))
                         (compile-boolexp (third bexp)))))
       (:/= (not (equalp (compile-boolexp (second bexp))
                         (compile-boolexp (third bexp)))))))
    (t (error "Cannot compile boolean expression"))))
