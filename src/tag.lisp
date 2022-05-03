(in-package #:djula)

(defun semi-parse-tag (string)
  (let ((*package* (find-package :keyword))
        (*read-eval* nil))
    (read-from-string string)))

(defun parse-rest-of-tag (string start)
  (handler-case
      (let ((*package* (find-package :keyword))
            (*read-eval* nil))
        (read-from-string (format nil "(~A)" (subseq string start))))
    (error ()
      (template-error "there was an error parsing the tag ~A" string))))

(def-token-processor :unparsed-tag (unparsed-string) rest
  (multiple-value-bind (tag-name start-rest)
      (semi-parse-tag unparsed-string)
    (if-let ((f (find-unparsed-tag-processor tag-name)))
      ;; if there's an unparsed tag parser, use it
      (funcall f rest (subseq unparsed-string start-rest))
      ;; otherwise, just create a :tag token and process it
      (let ((ret (parse-rest-of-tag unparsed-string start-rest)))
        (process-tokens (cons (list* :tag tag-name ret) rest))))))

(def-token-processor :tag (name . args) rest
  ":TAG tokens are sometimes parsed into some other tokens by PROCESS-TOKENS"
  (let ((f (find-tag-processor name)))
    (if (null f)
        (cons (list* :tag name args)
              (process-tokens rest))
        (with-template-error
            (cons
             (list :string
                   (template-error-string "There was an error processing tag ~A"
                                          name))
             (process-tokens rest))
          (apply f rest args)))))

(def-token-compiler :tag (name . args)
  (let ((f (find-tag-compiler name)))
    (if (null f)
        (lambda (stream)
          (princ (template-error-string "Unknown tag ~A" name) stream))
        (with-template-error
            (lambda (stream)
              (princ (template-error-string "There was an error compiling tag ~A" name)
                     stream))
          (apply f args)))))

(defun find-end-tag (tag-name tokens)
  "returns NIL if a :TAG token with the name `TAG-NAME' can't be found in `TOKENS'.
Otherwise returns three values:

   1. a list of all the tokens up to that token
   2. a list of all tokens after that token
   3. T, indicating that `TAG-NAME' was found"
  (let ((n (position-if (lambda (x)
                          (destructuring-bind (token-name . args)
                              x
                            (and (eql token-name :tag)
                                 (eql (first args) tag-name))))
                        tokens)))
    (when n
      (values (subseq tokens 0 n)
              (subseq tokens (1+ n))
              t))))

(defun find-end-tag-nested (endtag tag tokens)
  "Find end tag ENDTAG taking into account possible nested tags of type TAG.

Like FIND-END-TAG, but taking into account control structures nesting.

Returns NIL if a :TAG token with the name `TAG-NAME' can't be found in `TOKENS'.
Otherwise returns three values:

   1. a list of all the tokens up to that token
   2. a list of all tokens after that token
   3. T, indicating that `TAG-NAME' was found"

  (let ((pos
          (iter
            (with nest-count = 0)
            (with pos = -1)
            (for token in tokens)
            (incf pos)
            (when (eql (second token) tag)
              (incf nest-count))
            (when (eql (second token) endtag)
              (when (zerop nest-count)
                (return pos))
              (decf nest-count)))))
    (when pos
      (let ((unprocessed (subseq tokens (1+ pos)))
            (to-process (subseq tokens 0 pos)))
        (values to-process unprocessed t)))))
