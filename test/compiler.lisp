(in-package #:djula-test)

(in-suite djula-test)

(def-test compiler (:compile-at :definition-time)
  (let ((fn (compile-string
             "{% ifequal day \"Thursday\" %}
I never could get the hang of {{ day }}s.
{% endifequal %}")
            ))
    (is (string= "
I never could get the hang of Thursdays.
"
                 (render-template* fn nil :day "Thursday")))))
