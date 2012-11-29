(in-package #:djula-test)

(in-suite djula-test)

(test compiler
  (let ((fn (djula::compile-template-string
             "{% ifequal day \"Thursday\" %}
I never could get the hang of {{ day }}s.
{% endifequal %}")
          ))
    (is (string= "
I never could get the hang of Thursdays.
"
                 (funcall fn nil :day "Thursday")))))

