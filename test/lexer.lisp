(in-package #:djula-test)

(in-suite djula-test)

(def-test lexer (:compile-at :definition-time)
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
         (:unparsed-tag " endif ")
	 (:verbatim " this is {{verbatim}} "))
       (djula::parse-template-string
        "{# you're going to be late #}
{% if Thursday %}
I never could get the hang of {{ variable|truncatechars:5 }}
{% endif %}{$ this is {{verbatim}} $}")))
  (is (equalp
       '((:COMMENT " you're going to be late ")
         (:STRING "
")
         (:UNPARSED-TAG " if Thursday ")
         (:STRING "
I never could get the hang of ")
         (:UNPARSED-VARIABLE " variable|truncatechars:5 ")
         (:STRING "
")
         (:UNPARSED-TAG " endif ") (:VERBATIM " this is {{verbatim}} ")
         (:STRING " <p> hai</p"))
       (djula::parse-template-string
        "{# you're going to be late #}
{% if Thursday %}
I never could get the hang of {{ variable|truncatechars:5 }}
{% endif %}{$ this is {{verbatim}} $} <p> hai</p")))
  (is (equalp
       '((:COMMENT " you're going to be late ")
         (:STRING "
")
         (:UNPARSED-TAG " if Thursday ")
         (:STRING "
I never could get the hang of ")
         (:UNPARSED-VARIABLE " variable|truncatechars:5 ")
         (:STRING "
")
         (:UNPARSED-TAG " endif ") (:VERBATIM " this is {{verbatim}} ")
         (:STRING " <p> hai</p ") (:STRING "{{ leave start of tag unclosed"))
       (djula::parse-template-string
        "{# you're going to be late #}
{% if Thursday %}
I never could get the hang of {{ variable|truncatechars:5 }}
{% endif %}{$ this is {{verbatim}} $} <p> hai</p {{ leave start of tag unclosed"))))
