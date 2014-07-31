(in-package #:djula-test)

(in-suite djula-test)

(defun filter (name &rest args)
  (apply (get name 'djula::filter) args))

(test filters
  (is (string= "Capfirst" (filter :capfirst "capfirst")))
  (is (string= "cutout"   (filter :cut "cutITout" "IT")))
  (is (string= "default"  (filter :default "" "default")))
  (is (string= "lower"    (filter :lower "LOWER")))
  (is (string= "short..." (filter :truncatechars "short message" 5)))
  (is (string= "UPPER"    (filter :upper "upper")))
  (is (=        6         (filter :length "length")))
  (is (string= "&lt;asdf&gt;" (filter :force_escape "<asdf>")))
  (is (string= "asdf<br />asdf" (filter :linebreaksbr "asdf
asdf")))
  (is (string= "LALA" (filter :lisp "lala" "string-upcase")))
  (is (string= (filter :urlencode "http://www.google.com")
	       "http%3A%2F%2Fwww.google.com"))
  (is (= (filter :add 2 2) 4))
  (is (string= (filter :addslashes "I'm using Djula")
	       "I\\'m using Djula"))
  (is (string=
       (filter :date (encode-universal-time 0 0 0 1 1 2014))
       "2014-01-01"))
  (is (string=
       (filter :time (encode-universal-time 17 17 18 1 1 2014))
       "18:17:17"))
  #+nil(is (string=
       (filter :datetime (encode-universal-time 17 17 18 1 1 2014))
       "2014-01-01T18:17:17.000000-03:00"))
  (is (equalp (filter :join (list "1" "2" "3") ",")
	      "1,2,3"))
  (is (equalp (filter :first (list "a" "b" "c" "d"))
	      "a"))
  (is (equalp (filter :last (list "a" "b" "c" "d"))
	      "d")))

(test apply-filters
  (is (string= "SHORT..."
               (djula::apply-filters "short message" '((:truncatechars 5) (:upper))))))

