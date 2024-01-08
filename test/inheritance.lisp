(in-package :djula-test)

(in-suite djula-test)

(djula:add-template-directory (asdf:system-relative-pathname :djula "test/templates/"))

(let ((djula:*catch-template-errors-p* nil))
  (defparameter +t1+ (djula:compile-template* "t1.djula"))
  (defparameter +t2+ (djula:compile-template* "t2.djula"))
  (defparameter +t3+ (djula:compile-template* "t3.djula"))
  (defparameter +t4+ (djula:compile-template* "t4.djula"))
  (defparameter +t5+ (djula:compile-template* "t5.djula"))
  (defparameter +t6+ (djula:compile-template* "t6.djula"))
  (defparameter +t7+ (djula:compile-template* "t7.djula"))
  (defparameter +t8+ (djula:compile-template* "t8.djula"))
  (defparameter +t9+ (djula:compile-template* "t9.djula"))
  (defparameter +t10+ (djula:compile-template* "t10.djula"))
  (defparameter +t11+ (djula:compile-template* "t11.djula"))
  (defparameter +t12+ (djula:compile-template* "t12.djula"))
  (defparameter +t13+ (djula:compile-template* "t13.djula"))
  (defparameter +t14+ (djula:compile-template* "subdir/t14.djula"))
  (defparameter +t16+ (djula:compile-template* "t16.djula")))

(def-test simple-block-test (:compile-at :definition-time)
  (let ((output (djula:render-template* +t1+ nil)))
    (is (equalp (remove-if (lambda (char)
                             (member char (list #\  #\Newline)))
                           output)
                "beforeHelloafter"))))

(def-test one-level-block-inheritance-test (:compile-at :definition-time)
  (let ((output (djula:render-template* +t2+ nil)))
    (is (equalp (remove-if (lambda (char)
                             (member char (list #\  #\Newline)))
                           output)
                "beforeByeafter"))))

(def-test two-levels-block-inheritance-test (:compile-at :definition-time)
  (let ((output (djula:render-template* +t3+ nil)))
    (is (equalp (remove-if (lambda (char)
                             (member char (list #\  #\Newline)))
                           output)
                "beforeFooafter"))))

(def-test extends-error-test (:compile-at :definition-time)
  (signals djula::template-error
    (let ((djula:*catch-template-errors-p* nil))
      (djula::compile-string "{% extends \"foo.djula\" %}"))))

(def-test simple-super-test (:compile-at :definition-time)
  (let ((output (djula:render-template* +t4+ nil)))
    (is (equalp (remove-if (lambda (char)
                             (member char (list #\  #\Newline)))
                           output)
                "beforeHelloByeafter"))))

(def-test simple-annon-super-test (:compile-at :definition-time)
  (let ((output (djula:render-template* +t5+ nil)))
    (is (equalp (remove-if (lambda (char)
                             (member char (list #\  #\Newline)))
                           output)
                "beforeHelloByeafter"))))

(def-test super-error (:compile-at :definition-time)
  (signals djula::template-error
    (let ((djula:*catch-template-errors-p* nil))
      (djula::compile-string "{% super %}")))
  (signals djula::template-error
    (let ((djula:*catch-template-errors-p* nil))
      (djula::compile-string "{% super foo %}"))))

(def-test include-test (:compile-at :definition-time)
  (let ((djula:*catch-template-errors-p* nil))
    (is (equalp (remove-if (lambda (char)
                             (member char (list #\  #\Newline)))
                           (djula::render-template* +t6+))
                "beforeHelloafterbeforeByeafter"))))

(def-test include-with-vars-test (:compile-at :definition-time)
  (let ((djula:*catch-template-errors-p* nil))
    (is (equalp (remove-if (lambda (char)
                             (member char (list #\  #\Newline)))
                           (djula::render-template* +t7+ nil
                                                    :t1 "t1.djula"
                                                    :t2 "t2.djula"))
                "beforeHelloafterbeforeByeafter"))))

(def-test multiple-levels-block-inheritance-test (:compile-at :definition-time)
  (let ((output (djula:render-template* +t9+ nil)))
    (is (equalp (remove-if (lambda (char)
                             (member char (list #\  #\Newline)))
                           output)
                "beforeHelloafterafter2after3"))))

(def-test parameterized-include-test (:compile-at :definition-time)
  (let ((djula:*catch-template-errors-p* nil))
    (is (equalp (remove-if (lambda (char)
                             (member char (list #\  #\Newline)))
                           (djula::render-template* +t11+ nil
                                                    :name "World"))
                "Hello,123Hello,World"))))

(def-test parameterized-include-with-vars-test (:compile-at :definition-time)
  (let ((djula:*catch-template-errors-p* nil))
    (is (equalp (remove-if (lambda (char)
                             (member char (list #\  #\Newline)))
                           (djula::render-template* +t12+ nil
                                                    :t10 "t10.djula"
                                                    :name "World"))
                "Hello,123Hello,World"))))

(def-test relative-extends-1-test (:compile-at :definition-time)
  (let ((t8-output (djula:render-template* +t8+ nil))
        (t13-output (djula:render-template* +t13+ nil)))
    (is (equalp t8-output t13-output))))

(def-test relative-extends-2-test (:compile-at :definition-time)
  (let ((t8-output (djula:render-template* +t8+ nil))
        (t14-output (djula:render-template* +t14+ nil)))
    (is (equalp t8-output t14-output))))

(def-test relative-extends-3-test (:compile-at :definition-time)
  (let ((t8-output (djula:render-template* +t8+ nil))
        (t14-output (djula:render-template* +t14+ nil)))
    (is (equalp t8-output t14-output))))

(def-test relative-extends-4-test (:compile-at :definition-time)
  (let ((t9-output (djula:render-template* +t9+ nil))
        (t16-output (djula:render-template* +t16+ nil)))
    (is (equalp t9-output t16-output))))
