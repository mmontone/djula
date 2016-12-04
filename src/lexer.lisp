(in-package #:djula)

#|

The lexer tokens are of the form of (:type <string>)

The token types are:
- :comment
- :unparsed-variable
- :unparsed-translation
- :unparsed-tag
- :verbatim

Although not a lexer token, the keyword :not-special is used to signify that the string following a { is not a tag.

- :not-special: The previous { is converted into the token (:string "{")

|#

(defun token-type (char)
  "Return the token-type for CHAR."
  (case char
    (#\# :comment)
    (#\{ :unparsed-variable)
    (#\_ :unparsed-translation)
    (#\% :unparsed-tag)
    (#\$ :verbatim)
    (otherwise :not-special)))

(defun get-closing-delimiter (type)
  "Return the string that closes the corresponding token TYPE."
  (ecase type
    (:comment "#}")
    (:unparsed-variable "}}")
    (:unparsed-translation "_}")
    (:unparsed-tag "%}")
    (:verbatim "$}")))

(defun next-tag (string start)
  "Return the position of the start of next tag in STRING starting from START."
  (position #\{ string :start start :test 'char=))

(defun parse-tag (string current-position)
  "Return the lexer token and the index where the tag ended."
  (let ((type (token-type (char string (1+ current-position)))))
    (ecase type
      ((:comment
        :unparsed-variable
        :unparsed-translation
        :unparsed-tag
        :verbatim)
       (let ((end (search (get-closing-delimiter type) string :start2 (1+ current-position))))
         (if (null end)
             (values `(:string ,(subseq string current-position)) (length string))
             (values (list type (subseq string (+ 2 current-position) end)) (+ 2 end))))) ;; The 2 is the hard-coded length of the closing delimiters
      (:not-special (values '(:string "{")
                            (1+ current-position))))))

(defun parse-template-string (template)
  "Transform the TEMPLATE into a list of lexer tokens "
  (let ((current-position 0))
    (accum accumulate
      (loop
         :for open-brace := (next-tag template current-position)
         :until (null open-brace)
         :do
         (when (> open-brace current-position)
           (accumulate `(:string ,(subseq template current-position open-brace))))
         (multiple-value-bind (token next-position) (parse-tag template open-brace)
           (accumulate token)
           (setf current-position next-position)))
      (when (< current-position (length template))
        (accumulate `(:string ,(subseq template current-position)))))))

(def-token-compiler :verbatim (string)
  (lambda (stream)
    (write-string string stream)))
