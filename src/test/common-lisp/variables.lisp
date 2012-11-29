(in-package #:djula-test)

(in-suite djula-test)

(test variables
  (is (equal '(:truncatechars "30")
             (djula::parse-filter-string "truncatechars:\"30\"")))
  (is (equal '(:truncatechars "30")
             (djula::parse-filter-string "truncatechars:30")))
  (is (equal '(:foo :bar :baz 2)
             (djula::parse-variable-phrase "foo.bar.baz.2")))
  (is (equal '((:foo :bar :baz 2) (:truncatechars "30") (:upper))
             (djula::parse-variable-clause "foo.bar.baz.2 | truncatechars:30 | upper"))))

(test apply-keys/indexes
  (is (equal 1
             (djula::apply-keys/indexes
              '((:alist-a . 1) (:alist-b . (:plist-a #(3 2 1 0) :plist-b nil)))
              '(:alist-b :plist-a 2)))))
