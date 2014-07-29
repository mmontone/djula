(in-package :djula-test)

(setf *current-store*
      (make-instance 'file-store
		     :search-path 
		     (list (asdf:system-relative-pathname :djula "src/test/templates/"))))

(djula:compile-template* "t1.djula")

(djula:compile-template*
 (merge-pathnames "t1.djula" (asdf:system-relative-pathname :djula "src/test/templates/")))
