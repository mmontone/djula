(in-package :djula)

;; --- Utilities ----------------------------------------------------------------

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

(defun resolve-plist (plist)
  (loop
    :for key :in plist :by #'cddr
    :for val :in (cdr plist) :by #'cddr
    :collect key
    :collect (if (symbolp val)
                 (resolve-variable-phrase
                  (parse-variable-phrase (princ-to-string val)))
                 (princ-to-string val))))

;; --- Generic interface -------------------------------------------------------

(defvar *translation-backend* nil "The translation backend. One of :locale, :gettext, :translate. Its corresponding system must be loaded: either :djula-locale, :djula-gettext or :djula-translate.")

(defvar *warn-on-untranslated-messages* t)
(defvar *untranslated-messages* nil)

(defun translate (string &optional args
                           (language (or *current-language* *default-language*))
                           (backend *translation-backend*))
  "Translate STRING using Djula transaltion backend.
LANGUAGE is the language to translate to. The default is to use either *CURRENT-LANGUAGE* or *DEFAULT-LANGUAGE*, in that order.
BACKEND is the translation backend to use. Default is *TRANSLATION-BACKEND*."
  (apply #'backend-translate backend string language args))

(defgeneric backend-translate (backend string language &rest args)
  (:method ((backend null) string language &rest args)
    (declare (ignore args))
    (error "Translation backend has not been setup. Please see `*translation-backend*'."))
  (:method ((backend t) string language &rest args)
    (declare (ignore args))
    (error "Invalid translation backend: ~A. Did you load the corresponding system, such as :djula-gettext ?" backend)))

;;---- Djula tags ---------------------------------------------------------------

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

(def-token-compiler :translation (key &rest args)
  (lambda (stream)
    (let ((key-val
            (if (stringp key)
                key
                (resolve-variable-phrase key))))
      (princ (translate key-val (resolve-plist args)) stream))))

(def-filter :trans (it &rest args)
  (translate it args))
