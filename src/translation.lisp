(in-package :djula)

(defvar *translation-backend* :locale "The translation backend. One of :locale, :gettext")

(defun translate (string &optional (language (or *current-language* *default-language*))
			   (backend *translation-backend*))
  (backend-translate backend string language))

(defgeneric backend-translate (backend string language))

(defmethod backend-translate ((backend (eql :locale)) string language)
  (cl-locale:i18n string :locale language))

(defvar *gettext-domain* nil)

(defmethod backend-translate ((backend (eql :gettext)) string language)
  (gettext:gettext* string *gettext-domain* nil (string language)))

; reading :UNPARSED-TRANSLATION-VARIABLE TOKENS created by {_ translation-variable _}

(def-token-processor :unparsed-translation-variable (unparsed-string) rest
  `((:translation-variable
     ,(read-from-string unparsed-string))
    ,@(process-tokens rest)))

(def-unparsed-tag-processor :trans (unparsed-string) rest
  `((:translation-variable
     ,(read-from-string unparsed-string))
    ,@(process-tokens rest)))

; compiling :TRANSLATION-VARIABLE tokens

(def-token-compiler :translation-variable (name)
  (lambda (stream)
    (let ((value
	   (if (symbolp name)
	       (get-variable name)
	       name)))
      (princ (translate value) stream))))

(def-filter :trans (it)
  (translate it))
