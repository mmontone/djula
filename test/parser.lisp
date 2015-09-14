(in-package #:djula-test)

(in-suite djula-test)

(def-test parser (:compile-at :definition-time)
  (is (equalp
       `((:string "
")
         (:parsed-if (:thursday)
                     ((:string "
I never could get the hang of ")
                      (:variable (:variable) (:truncatechars "5"))
                      (:string "
"))
                     nil))
       (djula::process-tokens
        `((:comment " you're going to be late ")
          (:string "
")
          (:unparsed-tag " if Thursday ")
          (:string "
I never could get the hang of ")
          (:unparsed-variable " variable|truncatechars:5 ")
          (:string "
")
          (:unparsed-tag " endif "))))))
