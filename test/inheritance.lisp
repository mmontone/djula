(in-package :djula-test)

(setf *current-store*
      (make-instance 'file-store
		     :search-path 
		     (list (asdf:system-relative-pathname :djula "test/templates/"))))

(defparameter +t1+ (djula:compile-template* "t1.djula"))
(defparameter +t2+ (djula:compile-template* "t2.djula"))
(defparameter +t3+ (djula:compile-template* "t3.djula"))
(defparameter +t4+ (djula:compile-template* "t4.djula"))
(defparameter +t5+ (djula:compile-template* "t5.djula"))

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

(test two-levels-block-inheritance-test
  (let ((output (djula:render-template* +t3+ nil)))
    (is (equalp (remove-if (lambda (char)
			     (member char (list #\  #\Newline)))
			   output)
		"beforeFooafter"))))

(test extends-error-test
  (signals djula::template-error
    (let ((djula:*catch-template-errors-p* nil))
      (djula::compile-string "{% extends \"foo.djula\" %}"))))

(test simple-super-test
  (let ((output (djula:render-template* +t4+ nil)))
    (is (equalp (remove-if (lambda (char)
			     (member char (list #\  #\Newline)))
			   output)
		"beforeHelloByeafter"))))

(test simple-annon-super-test
  (let ((output (djula:render-template* +t5+ nil)))
    (is (equalp (remove-if (lambda (char)
			     (member char (list #\  #\Newline)))
			   output)
		"beforeHelloByeafter"))))

(test super-error
  (signals djula::template-error
    (let ((djula:*catch-template-errors-p* nil))
      (djula::compile-string "{% super %}")))
  (signals djula::template-error
    (let ((djula:*catch-template-errors-p* nil))
      (djula::compile-string "{% super foo %}"))))
