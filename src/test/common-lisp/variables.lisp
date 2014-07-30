(in-package #:djula-test)

(in-suite djula-test)

(test variables-test
  (is (equal '(:truncatechars "30")
             (djula::parse-filter-string "truncatechars:\"30\"")))
  (is (equal '(:truncatechars "30")
             (djula::parse-filter-string "truncatechars:30")))
  (is (equal '(:foo :bar :baz 2)
             (djula::parse-variable-phrase "foo.bar.baz.2")))
  (is (equal '((:foo :bar :baz 2) (:truncatechars "30") (:upper))
             (djula::parse-variable-clause "foo.bar.baz.2 | truncatechars:30 | upper"))))

(test apply-keys/indexes-test
  (is (equal 1
             (djula::apply-keys/indexes
              '((:alist-a . 1) (:alist-b . (:plist-a #(3 2 1 0) :plist-b nil)))
              '(:alist-b :plist-a 2)))))

(test variables-accessing-test
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
	   "")))  

    (let ((template (djula::compile-string "{{foo.bar}}")))
      (is (equalp
	   (djula::render-template*
	    template
	    nil)
	   ""))
      (is (equalp
	   (djula::render-template*
	    template
	    :foo (list :bar "bar"))
	   "bar")))))
