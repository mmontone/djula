(in-package #:djula-test)

(in-suite djula-test)

(defun filter (name &rest args)
  (apply (djula::find-filter name) args))

(def-test filters (:compile-at :definition-time)
  (is (string= "Capfirst" (filter :capfirst "capfirst")))
  (is (string= "cutout"   (filter :cut "cutITout" "IT")))
  (is (string= "default"  (filter :default "" "default")))
  (is (string= "lower"    (filter :lower "LOWER")))
  (is (string= "sh..." (filter :truncatechars "short message" 5)))
  (is (string= "Joel i..." (filter :truncatechars "Joel is a slug" 9)))
  (let ((djula:*elision-string* "xx"))
    (is (string= "Joel isxx" (filter :truncatechars "Joel is a slug" 9))))
  (signals djula::template-error
    (filter :truncatechars "Joel is a slug" 2))
  (is (string= "UPPER"    (filter :upper "upper")))
  (is (=        6         (filter :length "length")))
  (is (=        3         (filter :length '("abcd" "ef" "g"))))
  (is (string= "&lt;asdf&gt;" (filter :force-escape "<asdf>")))
  (is (string= "asdf<br />asdf" (filter :linebreaksbr "asdf
asdf")))
  (is (string= "LALA" (filter :lisp "lala" "string-upcase")))
  (is (string= (filter :urlencode "http://www.google.com")
	       "http%3A%2F%2Fwww.google.com"))
  (is (= (filter :add 2 "2") 4))
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
  (is (equalp (filter :join (list "1" "2" "3") ",,")
              "1,,2,,3"))
  (is (equalp (filter :first (list "a" "b" "c" "d"))
              "a"))
  (is (equalp (filter :last (list "a" "b" "c" "d"))
              "d")))

(def-test apply-filters (:compile-at :definition-time)
  (is (string= "SH..."
               (djula::apply-filters "short message" '((:truncatechars 5) (:upper))))))

(def-test regex-filters (:compile-at :definition-time)
  (let ((template
         ;; Extract the date month
         (djula::compile-string  "{{ date | scan:\"(?<=-)[0-9]*\" }}")))
    (is (string= (djula:render-template* template nil :date "2050-10-03")
                 "10")))

  (let ((template
         ;; First word with >= 5 letters
         (djula::compile-string "{{ subject | scan:\"[a-zA-Z]{5,}\"}}")))
    (is (string= 
         (djula:render-template* template nil :subject "foo world")
         "world")))

  (let ((template
         ;; Replace regex
         (djula::compile-string "{{ data | replace:\"hello\" | with:\"bye cruel\"}}")))
    (is (string= (djula:render-template* template nil :data "hello world")
                 "bye cruel world"))))
