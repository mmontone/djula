(in-package #:djula-test)

(in-suite djula-test)

(test lexer
  (is (equalp
       `((:comment " you're going to be late ")
         (:string "
")
         (:unparsed-tag " if Thursday ")
         (:string "
I never could get the hang of ")
         (:unparsed-variable " variable|truncatechars:5 ")
         (:string "
")
         (:unparsed-tag " endif "))
       (djula::parse-template-string
        "{# you're going to be late #}
{% if Thursday %}
I never could get the hang of {{ variable|truncatechars:5 }}
{% endif %}"))))
