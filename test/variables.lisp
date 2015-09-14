(in-package #:djula-test)

(in-suite djula-test)

(def-test variables-test (:compile-at :definition-time)
  (is (equal '(:truncatechars "30")
             (djula::parse-filter-string "truncatechars:\"30\"")))
  (is (equal '(:truncatechars "30")
             (djula::parse-filter-string "truncatechars:30")))
  (is (equal '(:foo :bar :baz 2)
             (djula::parse-variable-phrase "foo.bar.baz.2")))
  (is (equal '((:foo :bar :baz 2) (:truncatechars "30") (:upper))
             (djula::parse-variable-clause "foo.bar.baz.2 | truncatechars:30 | upper"))))

(def-test apply-keys/indexes-test (:compile-at :definition-time)
  (is (equal 1
             (djula::apply-keys/indexes
              '((:alist-a . 1) (:alist-b . (:plist-a #(3 2 1 0) :plist-b nil)))
              '(:alist-b :plist-a 2)))))

(defclass my-obj ()
  ((name :accessor name
	 :initform "my-obj")))

(defmethod id ((my-obj my-obj))
  (declare (ignore my-obj))
  22)

(defun fid (my-obj)
  (declare (ignore my-obj))
  33)

(def-test variables-accessing-test (:compile-at :definition-time)
  (let ((djula:*catch-template-errors-p* nil))
    
    (let ((template (djula::compile-string "{{foo}}")))
      (is (equalp
	   (djula::render-template* 
	    template nil)
	   ""))
      (is (equalp
	   (djula::render-template* 
	    template nil :foo nil)
	   ""))
      (is (equalp
	   (djula::render-template*
	    template
	    nil
	    :foo "foo")
	   "foo"))
      (is (equalp
	   (djula::render-template*
	    template
	    nil
	    :foo 2)
	   "2")))

    (let ((template (djula::compile-string "{{foo}}{{bar}}")))
      (is (equalp
	   (djula::render-template* 
	    template nil)
	   ""))
      (is (equalp
	   (djula::render-template* 
	    template nil
	    :foo "foo")
	   "foo"))
      (is (equalp
	   (djula::render-template* 
	    template nil
	    :bar "bar")
	   "bar"))
      (is (equalp
	   (djula::render-template* 
	    template nil
	    :foo "foo" :bar "bar")
	   "foobar")))  

    (let ((template (djula::compile-string "{{foo.bar}}")))
      (is (equalp
	   (djula::render-template*
	    template
	    nil)
	   ""))
      (is (equalp
	   (djula::render-template*
	    template
	    nil
	    :foo (list :bar "bar"))
	   "bar")))
    
    ;; Slot accessing
    (is (equalp
	 (let ((djula:*djula-execute-package* :djula-test))
	   (let ((template (djula::compile-string "{{obj.name}}")))
	     (djula:render-template* template nil :obj (make-instance 'my-obj))))
	 "my-obj"))
    
    ;; Method accessing
    (is (equalp
	 (let ((djula:*djula-execute-package* :djula-test))
	   (let ((template (djula::compile-string "{{obj.id}}")))
	     (djula:render-template* template nil :obj (make-instance 'my-obj))))
	 "22"))

    ;; Function accessing
    (is (equalp
	 (let ((djula:*djula-execute-package* :djula-test))
	   (let ((template (djula::compile-string "{{obj.fid}}")))
	     (djula:render-template* template nil :obj (make-instance 'my-obj))))
	 "33"))))

(def-test escaping-test (:compile-at :definition-time)
  (let ((djula:*catch-template-errors-p* nil))
    ;; Test defaults
    (is (equalp
	 (let ((template (djula::compile-string "{{foo}}")))
	   (djula:render-template* template nil :foo "<b>Hello</b>"))
	 "&lt;b&gt;Hello&lt;/b&gt;"))
    (is (equalp
	 (let ((template (djula::compile-string "{{foo | safe}}")))
	   (djula:render-template* template nil :foo "<b>Hello</b>"))
	 "<b>Hello</b>"))
    ;; Auto escape setting
    (let ((djula:*auto-escape* t))
      (is (equalp
	   (let ((template (djula::compile-string "{{foo}}")))
	     (djula:render-template* template nil :foo "<b>Hello</b>"))
	   "&lt;b&gt;Hello&lt;/b&gt;")))
    (let ((djula:*auto-escape* nil))
      (is (equalp
	   (let ((template (djula::compile-string "{{foo}}")))
	     (djula:render-template* template nil :foo "<b>Hello</b>"))
	   "<b>Hello</b>")))
    ;; Safe and auto-escape priorities
    (let ((djula:*auto-escape* t))
      (is (equalp
	   (let ((template (djula::compile-string "{{foo | safe}}")))
	     (djula:render-template* template nil :foo "<b>Hello</b>"))
	   "<b>Hello</b>")))
    (let ((djula:*auto-escape* nil))
      (is (equalp
	   (let ((template (djula::compile-string "{{foo | escape}}")))
	     (djula:render-template* template nil :foo "<b>Hello</b>"))
	   "&lt;b&gt;Hello&lt;/b&gt;")))))
