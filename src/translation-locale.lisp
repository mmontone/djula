(in-package :djula)

(pushnew :locale *translation-backends*)

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
