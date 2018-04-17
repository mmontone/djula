(in-package :djula)

(defvar *translation-backend* nil "The translation backend. One of :locale, :gettext")

(defvar *warn-on-untranslated-messages* t)
(defvar *untranslated-messages* nil)

(defun translate (string &optional args
                           (language (or *current-language* *default-language*))
                           (backend *translation-backend*))
  (apply #'backend-translate backend string language args))

(defun format-translation (string &rest args)
  (apply #'format nil
         (ppcre:regex-replace-all
          "\\:(\\w*)"
          string
          (lambda (_ varname)
            (declare (ignore _))
            (let ((val (access:access args (make-keyword varname))))
              (or (and val (princ-to-string val))
                  (error "~A missing in ~A translation" varname string))))
          :simple-calls t)
         args))

(defgeneric backend-translate (backend string language &rest args)
  (:method ((backend null) string language &rest args)
    (error "Translation backend has not been setup"))
  (:method ((backend t) string language &rest args)
    (error "Invalid translation backend: ~A" backend)))

#-lispworks
(defmethod backend-translate ((backend (eql :locale)) string language &rest args)
  (let ((dictionary (locale:current-dictionary)))
    (when (not (arnesi:aand (not (eq language locale:*default-locale*))
                            (gethash language dictionary)
                            (gethash string arnesi:it)))
      (when *warn-on-untranslated-messages*
        (warn "DJULA TRANSLATION NOT GIVEN: ~A ~A" string language))
      (pushnew (cons string language) *untranslated-messages* :test 'equalp)))
  (apply #'format-translation
         (cl-locale:i18n string
                         :locale language
                         :params args)
         args))

(defvar *gettext-domain* nil)

(defmethod backend-translate ((backend (eql :gettext)) string language &rest args)
  (apply #'format-translation
         (gettext:gettext* string *gettext-domain* nil (string-downcase (string language)))
         args))

;; reading :UNPARSED-TRANSLATION-VARIABLE TOKENS created by {_ translation _}

(def-token-processor :unparsed-translation (unparsed-string) rest
  (multiple-value-bind (key pos) (read-from-string unparsed-string)
    (let ((args (and (< pos (length unparsed-string))
                     (read-from-string (format nil "(~A)"
                                               (subseq unparsed-string pos))))))
      `((:translation
         ,(if (stringp key)
              ;; is a hard-coded string
              key
              ;; we assume it is a variable reference
              (parse-variable-phrase (princ-to-string key)))
         ,@args)
        ,@(process-tokens rest)))))

(def-unparsed-tag-processor :trans (unparsed-string) rest
  (multiple-value-bind (key pos) (read-from-string unparsed-string)
    (let ((args (and (< pos (length unparsed-string))
                     (read-from-string (format nil "(~A)"
                                               (subseq unparsed-string pos))))))
      `((:translation
         ,(if (stringp key)
              ;; is a hard-coded string
              key
              ;; we assume it is a variable reference
              (parse-variable-phrase (princ-to-string key)))
         ,@args)
        ,@(process-tokens rest)))))

;; compiling :TRANSLATION-VARIABLE tokens

(defun resolve-plist (plist)
  (loop
     :for key :in plist :by #'cddr
     :for val :in (cdr plist) :by #'cddr
     :collect key
     :collect (if (symbolp val)
                  (resolve-variable-phrase
                   (parse-variable-phrase (princ-to-string val)))
                  (princ-to-string val))))

(def-token-compiler :translation (key &rest args)
  (lambda (stream)
    (let ((key-val
           (if (stringp key)
               key
               (resolve-variable-phrase key))))
      (princ (translate key-val (resolve-plist args)) stream))))

(def-filter :trans (it &rest args)
  (translate it args))
