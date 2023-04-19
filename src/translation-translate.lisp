(in-package :djula)

(defmethod backend-translate ((backend (eql :translate)) string language &rest args)
  (apply #'translate:translate string language args))
