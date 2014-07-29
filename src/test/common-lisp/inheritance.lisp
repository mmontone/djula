(in-package :djula-test)

(setf *current-store*
      (make-instance 'file-store
		     :search-path 
		     (list (asdf:system-relative-pathname :djula "src/test/templates/"))))

(defparameter +t1+ (djula:compile-template* "t1.djula"))
(defparameter +t2+ (djula:compile-template* "t2.djula"))
(defparameter +t3+ (djula:compile-template* "t3.djula"))

(test simple-block-test
  (let ((output (djula:render-template* +t1+ nil)))
    (is (equalp (remove-if (lambda (char)
			     (member char (list #\  #\Newline)))
			   output)
		"beforeHelloafter"))))

(test one-level-block-inheritance-test
  (let ((output (djula:render-template* +t2+ nil)))
    (is (equalp (remove-if (lambda (char)
			     (member char (list #\  #\Newline)))
			   output)
		"beforeByeafter"))))

;; Two levels inheritance doesn't work. Needs to be fixed.
(test two-levels-block-inheritance-test
  (let ((output (djula:render-template* +t3+ nil)))
    (is (equalp (remove-if (lambda (char)
			     (member char (list #\  #\Newline)))
			   output)
		"beforeFooafter"))))
