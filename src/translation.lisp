(in-package :djula)

(defvar *translation-backend* nil "The translation backend. One of :locale, :gettext")

(defun translate (string &optional (language (or *current-language* *default-language*))
                           (backend *translation-backend*))
  (backend-translate backend string language))

(defgeneric backend-translate (backend string language)
  (:method ((backend null) string language)
    (error "Translation backend has not been setup"))
  (:method ((backend t) string language)
    (error "Invalid translation backend: ~A" backend)))

#-lispworks
(defmethod backend-translate ((backend (eql :locale)) string language)
  (cl-locale:i18n string :locale language))

(defvar *gettext-domain* nil)

(defmethod backend-translate ((backend (eql :gettext)) string language)
  (gettext:gettext* string *gettext-domain* nil (string language)))

;; reading :UNPARSED-TRANSLATION-VARIABLE TOKENS created by {_ translation-variable _}

(def-token-processor :unparsed-translation-variable (unparsed-string) rest
  `((:translation-variable
     ,(let ((thing (read-from-string unparsed-string)))
        (if (stringp thing)
            ;; is a hard-coded string
            thing
            ;; we assume it is a variable reference
            (parse-variable-phrase (string-trim (list #\space #\tab #\newline) unparsed-string)))))
    ,@(process-tokens rest)))

(def-unparsed-tag-processor :trans (unparsed-string) rest
  `((:translation-variable
     ,(let ((thing (read-from-string unparsed-string)))
        (if (stringp thing)
            ;; is a hard-coded string
            thing
            ;; we assume it is a variable reference
            (parse-variable-phrase (string-trim (list #\space #\tab #\newline) unparsed-string)))))
    ,@(process-tokens rest)))

;; compiling :TRANSLATION-VARIABLE tokens

(def-token-compiler :translation-variable (var)
  (lambda (stream)
    (let ((value
           (if (stringp var)
               var
               (resolve-variable-phrase var))))
      (princ (translate value) stream))))

(def-filter :trans (it)
  (translate it))
