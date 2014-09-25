(ql:quickload :djula)

(defun class-p (symbol)
  "Return T if the symbol is a class."
  (eql (sb-int:info :type :kind symbol) :instance))

(defun variable-p (symbol)
  "Return T if the symbol is a bound variable."
  (and (sb-int:info :variable :kind symbol)
                         (boundp symbol)))

(defun symbol-function-type (symbol)
  (cond
    ((macro-function symbol)
     'macro)
    ((fboundp symbol)
     (type-of (fdefinition symbol)))))

(defun symbol-type (symbol)
  (or (symbol-function-type symbol)
      (and (class-p symbol) 'class)
      (and (variable-p symbol) 'variable)))

(defgeneric format-sphinx-doc (thing type stream)
  (:method (thing type stream)
    ;; Do nothing
    )
  (:method (thing (type (eql 'function)) stream)
    (format stream ".. cl:function:: ~A" (string-downcase (symbol-name thing))))
  (:method (thing (type (eql 'macro)) stream)
    (format stream ".. cl:macro:: ~A" (string-downcase (symbol-name thing))))
  (:method (thing (type (eql 'variable)) stream)
    (format stream ".. cl:variable:: ~A" (string-downcase (symbol-name thing)))))

(with-open-file (f "./source/api.rst"
		   :direction :output
		   :if-exists :supersede
		   :if-does-not-exist :create)
  (format f "API~%")
  (format f "---~%~%")
  (format f "Djula external symbols documentation~%~%")
  (format f ".. cl:package:: djula~%~%")
  (do-external-symbols (symbol (find-package 'djula))
    (when (symbol-function-type symbol)
      (format-sphinx-doc symbol (symbol-function-type symbol) f)
      (format f "~%~%"))
    (when (class-p symbol)
      (format-sphinx-doc symbol 'class f)
      (format f "~%~%"))
    (when (variable-p symbol)
      (format-sphinx-doc symbol 'variable f)
      (format f "~%~%"))))
