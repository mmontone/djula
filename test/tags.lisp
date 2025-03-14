(in-package #:djula-test)

(in-suite djula-test)

(defun tag (name &rest args)
  (let ((fn (apply (djula::find-tag-compiler name) args))
        (djula::*template-arguments* nil))
    (with-output-to-string (s)
      (funcall fn s))))

(defun token (name &rest args)
  (let ((fn (apply (djula::find-token-compiler name) args))
        (djula::*template-arguments* nil))
    (with-output-to-string (s)
      (funcall fn s))))

(def-test static (:compile-at :definition-time)
  (is (string= "/public/js/app.js"
               (progn
                (djula::set-static-url "/public/")
                (let ((fn (apply (djula::find-tag-compiler :static) '("js/app.js"))))
                    (with-output-to-string (stream)
                        (funcall fn stream)))))))

(def-test firstof (:compile-at :definition-time)
  ;; Choice first string found
  (is (string= "first"
               (let ((fn (apply (djula::find-tag-compiler :firstof) '("first" "second"))))
                 (with-output-to-string (stream)
                   (funcall fn stream)))))

  ;; Do not error on non existing vars
  (is (string= "fallback"
               (let ((template (compile-string "{% firstof foo bar \"fallback\" %}")))
                 (djula:render-template* template nil :foobar "<b>Hello</b>"))))

  ;; Choice first non nil var
  (is (string= "bar"
               (let ((template (compile-string "{% firstof foo bar \"fallback\" %}")))
                 (djula:render-template* template nil :foo nil :bar "bar"))))

  ;; Output nothing if no var is non nil
  (is (string= ""
               (let ((template (compile-string "{% firstof foo bar %}")))
                 (djula:render-template* template nil :foo nil :bar nil))))

  ;; Output numbers correctly
  (is (string= "5"
               (let ((template (compile-string "{% firstof foo bar 5 %}")))
                 (djula:render-template* template nil :foo nil :bar nil))))

  ;; Handle empty argument lists correctly
  (is (string= ""
               (let ((template (compile-string "{% firstof %}")))
                 (djula:render-template* template nil)))))

(def-test cycle (:compile-at :definition-time)
  (is (string= "010101"
               (let ((fn (apply (djula::find-tag-compiler :cycle) '(0 1)))
                     (djula::*template-arguments* nil))
                 (with-output-to-string (s)
                   (dotimes (_ 6)
                     (funcall fn s)))))))

(def-test js (:compile-at :definition-time)
  (let ((djula::*accumulated-javascript-strings* nil))
    (is (string= "" (token :parsed-js "http://cdn.sockjs.org/sockjs-0.3.min.js")))
    (is (string= "
<script type='text/javascript' src=\"http://cdn.sockjs.org/sockjs-0.3.min.js\"></script>"
                 (tag :emit-js)))))

(def-test language (:compile-at :definition-time)
  (let ((*current-language* :english))
    (is (string= "" (tag :set-language :lojban)))
    (is (string= (princ-to-string :lojban) (tag :show-language)))))

(def-test logic (:compile-at :definition-time)
  (let ((fn (djula::compile-logical-statement (list :thursday))))
    (let ((djula::*template-arguments* '((:thursday . t))))
      (is (funcall fn)))
    (let ((djula::*template-arguments* '((:thursday . nil))))
      (is (not (funcall fn))))))

(def-test conditional-test (:compile-at :definition-time)
  (let ((template (compile-string "{% if foo %}foo{% else %}bar{% endif %}")))
    (is (equalp
         (djula:render-template* template nil)
         "bar"))
    (is (equalp
         (djula:render-template* template nil :foo t)
         "foo")))
  ;; or
  (let ((template (compile-string "{%if foo or bar %}foo or bar{% else %}not foo or bar{% endif %}")))
    (is (equalp
         (djula:render-template* template nil :foo nil :bar nil)
         "not foo or bar"))
    (is (equalp
         (djula:render-template* template nil :foo t :bar nil)
         "foo or bar")))

  ;; and
  (let ((template (compile-string "{%if foo and bar %}foo and bar{% else %}not foo and bar{% endif %}")))
    (is (equalp
         (djula:render-template* template nil :foo t :bar nil)
         "not foo and bar"))
    (is (equalp
         (djula:render-template* template nil :foo t :bar t)
         "foo and bar")))

  ;; vars
  (let ((template (compile-string "{%if foo.value and bar.length %}foo and bar{% else %}not foo and bar{% endif %}")))
    (is (equalp
         (djula:render-template* template nil :foo (list :value t)
                                              :bar (list :length 2))
         "foo and bar")))

  ;; subexpressions
  (let ((template (compile-string "{%if foo and (bar or baz) %}true{% else %}false{% endif %}")))
    (is (equalp
         (djula:render-template* template nil :foo t
                                              :bar t)
         "true")))

  ;; equality
  (let ((template (compile-string "{%if foo == bar %}true{% else %}false{% endif %}")))
    (is (equalp
         (djula:render-template* template nil
                                 :foo "foo"
                                 :bar "bar")
         "false"))
    (is (equalp
         (djula:render-template* template nil
                                 :foo "foo"
                                 :bar "foo")
         "true")))

  ;; unequality
  (let ((template (compile-string "{%if foo != bar %}true{% else %}false{% endif %}")))
    (is (equalp
         (djula:render-template* template nil
                                 :foo "foo"
                                 :bar "bar")
         "true"))
    (is (equalp
         (djula:render-template* template nil
                                 :foo "foo"
                                 :bar "foo")
         "false")))

  ;; greater and lower
  (let ((template (compile-string "{%if foo > bar %}greater{% else %}lower{% endif %}")))
    (is (equalp
         (djula:render-template* template nil
                                 :foo 44
                                 :bar 33)
         "greater"))
    (is (equalp
         (djula:render-template* template nil
                                 :foo 33
                                 :bar 44)
         "lower")))

  ;; literals
  (let ((template (compile-string "{%if foo > 5 %}greater{% else %}lower{% endif %}")))
    (is (equalp
         (djula:render-template* template nil
                                 :foo 44)
         "greater"))
    (is (equalp
         (djula:render-template* template nil
                                 :foo 1)
         "lower")))

  (let ((template (compile-string "{%if true %}true{% else %}false{% endif %}")))
    (is (equalp
         (djula:render-template* template nil)
         "true")))

  (let ((template (compile-string "{%if false %}false{% else %}true{% endif %}")))
    (is (equalp
         (djula:render-template* template nil)
         "true")))

  (let ((template (compile-string "{%if foo == \"foo\" %}foo{% else %}bar{% endif %}")))
    (is (equalp
         (djula:render-template* template nil
                                 :foo "foo")
         "foo"))
    (is (equalp
         (djula:render-template* template nil
                                 :foo "lala")
         "bar")))

  ;; Empty values
  (let ((template (compile-string "{%if foo %}foo{% else %}bar{% endif %}")))
    (is (equalp
         (djula:render-template* template nil
                                 :foo "")
         "bar"))
    (is (equalp
         (djula:render-template* template nil
                                 :foo #())
         "bar"))
    (is (equalp
         (djula:render-template* template nil
                                 :foo "lala")
         "foo")))

  ;; Expression parsing error
  (signals error
    (compile-string "{%if foo bar %}foo{% else %}bar{% endif %}"))

  (signals error
    (compile-string "{%if foo bar %}foo = bar{% else %}bar{% endif %}"))

  (signals error ;; no endif
    (compile-string "{%if foo %}foo{% else %}bar")))

(def-test elif-test (:compile-at :definition-time)
  (let ((template (compile-string "{% if x > 20 %}more than 20{% elif x < 0 %}less than zero{% else %}greater than zero{% endif %}")))
    (is (equalp (render-template* template nil :x 21) "more than 20"))
    (is (equalp (render-template* template nil :x 1) "greater than zero"))
    (is (equalp (render-template* template nil :x -1) "less than zero"))))

(def-test loop-test (:compile-at :definition-time)
  (let ((djula:*catch-template-errors-p* nil)
        (template (compile-string "<ul>{% for elem in list %}<li>{{elem}}</li>{% endfor %}</ul>")))
    (let ((*strict-mode* t))
      (signals error
        (djula:render-template* template nil)))
    (let ((*strict-mode* nil))
      (is (equalp (render-template* template nil)
                  "<ul></ul>")))
    (is (equalp
         (djula:render-template* template nil :list ())
         "<ul></ul>"))
    (is (equalp
         (djula:render-template* template nil :list (list "foo" "bar"))
         "<ul><li>foo</li><li>bar</li></ul>")))
  (let ((template (compile-string "{% for x in list %}{{forloop.counter}}{% endfor %}")))
    (is (equalp
         (djula:render-template* template nil :list (list 1 2 3))
         "123")))
  (let ((template (compile-string "{% for x in list %}{{forloop.counter0}}{% endfor %}")))
    (is (equalp
         (djula:render-template* template nil :list (list 1 2 3))
         "012")))
  (let ((template (compile-string "{% for x in list %}{{forloop.revcounter}}{% endfor %}")))
    (is (equalp
         (djula:render-template* template nil :list (list 1 2 3))
         "321")))
  (let ((template (compile-string "{% for x in list %}{{forloop.revcounter0}}{% endfor %}")))
    (is (equalp
         (djula:render-template* template nil :list (list 1 2 3))
         "210")))
  (let ((template (compile-string "{% for x in list %}{{x}}{% if not forloop.last %}, {% endif %}{% endfor %}")))
    (is (equalp
         (djula:render-template* template nil :list (list 1 2 3))
         "1, 2, 3")))
  (let ((template (compile-string "{% for x in list %}{% if forloop.first %}<first>{% endif %}{{x}}{% if forloop.first %}</first>{% endif %}{% endfor %}")))
    (is (equalp
         (djula:render-template* template nil :list (list 1 2 3))
         "<first>1</first>23")))
  (let ((template (compile-string "<ul>{% for elem in list %}<li>{{elem}}</li>{% endfor %}</ul>")))
    (is (equalp
         (djula:render-template* template nil :list #())
         "<ul></ul>"))
    (is (equalp
         (djula:render-template* template nil :list #("foo" "bar"))
         "<ul><li>foo</li><li>bar</li></ul>")))
  (let ((template (compile-string "<ul>{% for elem in list %}<li>{{elem}}</li>{% endfor %}</ul>")))
    (is (equalp
         (djula:render-template* template nil :list "")
         "<ul></ul>"))
    (is (equalp
         (djula:render-template* template nil :list "abc")
         "<ul><li>a</li><li>b</li><li>c</li></ul>")))
  (let ((template (compile-string "<ul>{% for (x . y) in list %}<li>{{x}}->{{y}}</li>{% endfor %}</ul>")))
    (is (equalp
         (djula:render-template* template nil :list ())
         "<ul></ul>"))
    (is (equalp
         (djula:render-template* template nil :list '((a . foo) (b . bar)))
         "<ul><li>A->FOO</li><li>B->BAR</li></ul>")))
  (let ((template (compile-string "<ul>{% for (x . y) in list %}<li>{{x}}->{{y}}</li>{% endfor %}</ul>")))
    (is (equalp
         (djula:render-template* template nil :list (make-hash-table))
         "<ul></ul>"))
    (is (member
         (djula:render-template* template nil
                                 :list (let ((table (make-hash-table)))
                                         (setf (gethash 'b table) 'bar)
                                         (setf (gethash 'a table) 'foo)
                                         table))
         `(,(format nil "<ul><li>~a->~a</li><li>~a->~a</li></ul>" 'a 'foo 'b 'bar)
           ,(format nil"<ul><li>~a->~a</li><li>~a->~a</li></ul>" 'b 'bar 'a 'foo))
         :test #'string=))))

(def-test nested-loop-test (:compile-at :definition-time)
  (let ((template (compile-string "{% for list in lists %}<ul>{% for elem in list %}<li>{{elem}}</li>{% endfor %}</ul>{% endfor %}")))
    (is (equalp
         (djula:render-template* template nil
                                 :lists (list (list "foo" "bar")
                                              (list "baz")))
         "<ul><li>foo</li><li>bar</li></ul><ul><li>baz</li></ul>"))))

(def-test empty-loop-test (:compile-at :definition-time)
  (let ((template (compile-string "<ul>{% for item in items %}<li>{{item}}</li>{% empty %}<li>No items found</li>{% endfor %}</ul>")))
    ;; Test with non-empty list
    (is (equalp
         (djula:render-template* template nil :items (list "foo" "bar"))
         "<ul><li>foo</li><li>bar</li></ul>"))
    ;; Test with empty list
    (is (equalp
         (djula:render-template* template nil :items '())
         "<ul><li>No items found</li></ul>"))
    ;; Test with nil (should show empty content)
    (is (equalp
         (djula:render-template* template nil)
         "<ul><li>No items found</li></ul>")))
  
  ;; Test with nested loops and empty
  (let ((template (compile-string "{% for list in lists %}<ul>{% for item in list %}<li>{{item}}</li>{% empty %}<li>Empty inner list</li>{% endfor %}</ul>{% empty %}<p>No lists found</p>{% endfor %}")))
    ;; Test with non-empty outer list, mixed inner lists
    (is (equalp
         (djula:render-template* template nil
                                 :lists (list (list "foo" "bar")
                                              '()))
         "<ul><li>foo</li><li>bar</li></ul><ul><li>Empty inner list</li></ul>"))
    ;; Test with empty outer list
    (is (equalp
         (djula:render-template* template nil :lists '())
         "<p>No lists found</p>"))))

(def-test logical-statements-test (:compile-at :definition-time)
  (let ((template (compile-string "{% if foo and baz %}yes{% else %}no{% endif %}")))
    (is (equalp
         (djula:render-template* template nil)
         "no"))
    (is (equalp
         (djula:render-template* template nil :foo "foo")
         "no"))
    (is (equalp
         (djula:render-template* template nil :foo "foo" :baz "baz")
         "yes")))
  (let ((template (compile-string "{% if foo and not baz %}yes{% else %}no{% endif %}")))
    (is (equalp
         (djula:render-template* template nil)
         "no"))
    (is (equalp
         (djula:render-template* template nil :foo "foo")
         "yes"))
    (is (equalp
         (djula:render-template* template nil :foo "foo" :baz "baz")
         "no")))
  ;; association
  (let ((template (compile-string "{% if foo and (not baz) %}yes{% else %}no{% endif %}")))
    (is (equalp
         (djula:render-template* template nil)
         "no"))
    (is (equalp
         (djula:render-template* template nil :foo "foo")
         "yes"))
    (is (equalp
         (djula:render-template* template nil :foo "foo" :baz "baz")
         "no")))
  ;; numeric comparison
  (let ((template (compile-string "{% if foo > baz %}yes{% else %}no{% endif %}")))
    (is (equalp
         (djula:render-template* template nil :foo 3 :baz 2)
         "yes"))
    (is (equalp
         (djula:render-template* template nil :foo 2 :baz 3)
         "no")))
  ;; Lisp evaluation in ifs doesn't work, could be nice...
  #+nil(let ((template (compile-string "{% if (> foo baz) | lisp %}yes{% else %}no{% endif %}")))
         (is (equalp
              (djula:render-template* template nil :foo 3 :baz 2)
              "yes"))
         (is (equalp
              (djula:render-template* template nil :foo 2 :baz 3)
              "no"))))

(defun print-hello ()
  "Hello!!")

(def-test lisp-tag-test (:compile-at :definition-time)
  (let ((djula:*catch-template-errors-p* nil))
    ;; Simple lisp expression
    (let ((template (compile-string "{% lisp (+ 33 44)%}")))
      (is (equalp
           (djula:render-template* template nil)
           "77")))
    ;; Function not in the correct package
    (signals template-error
      (let ((djula:*template-package* :cl-user))
        (let ((template (compile-string "{% lisp (print-hello)%}")))
          (is (equalp
               (djula:render-template* template nil)
               (print-hello))))))
    ;; Fix this. Fails on the first run for some reason. Works afterwards.
    ;; Set the lisp package for accessing the function
    #+nil(let ((template (compile-string "{% set-package djula-test %}{% lisp (print-hello)%}")))
           (is (equalp
                (djula:render-template* template nil)
                (print-hello))))
    ;; Set a template variable
    (let ((template (compile-string "{% set hello = (+ 4 5) %}{{ hello }}")))
      (is (equalp
           (djula:render-template* template nil)
           "9")))))

(def-test comment-test (:compile-at :definition-time)
  (let ((template (compile-string "Hello{% comment %}This is a comment {% endcomment %}")))
    (is (equalp
         (djula:render-template* template nil)
         "Hello")))
  (let ((template (compile-string "{# This is a comment #}Hello")))
    (is (equalp
         (djula:render-template* template nil)
         "Hello"))))

(def-test ifequal-test (:compile-at :definition-time)
  (let ((template (compile-string "{% ifequal foo bar %}yes{% else %}no{% endifequal %}")))
    (is (equalp
         (djula:render-template* template nil :foo "foo" :bar "bar")
         "no"))
    (is (equalp
         (djula:render-template* template nil :foo "foo" :bar "foo")
         "yes"))
    (is (equalp
         (djula:render-template* template nil :foo 4 :bar 4)
         "yes")))
  (let ((template (compile-string "{% ifequal foo.x bar.y %}yes{% else %}no{% endifequal %}")))
    (is (equalp
         (djula:render-template* template nil :foo '(:x "foo") :bar '(:y "bar"))
         "no"))
    (is (equalp
         (djula:render-template* template nil :foo '(:x "foo") :bar '(:y "foo"))
         "yes"))
    (is (equalp
         (djula:render-template* template nil :foo '(:x 4) :bar '(:y 4))
         "yes")))
  (let ((template (compile-string "{% ifequal foo.x 22 %}yes{% else %}no{% endifequal %}")))
    (is (equalp
         (djula:render-template* template nil :foo '(:x 22))
         "yes"))))

(def-test ifnotequal-test (:compile-at :definition-time)
  (let ((template (compile-string "{% ifnotequal foo bar %}yes{% else %}no{% endifnotequal %}")))
    (is (equalp
         (djula:render-template* template nil :foo "foo" :bar "bar")
         "yes"))
    (is (equalp
         (djula:render-template* template nil :foo "foo" :bar "foo")
         "no"))
    (is (equalp
         (djula:render-template* template nil :foo 4 :bar 4)
         "no"))))

(def-test verbatim-test (:compile-at :definition-time)
  (let ((template (compile-string "{$ this is {{verbatim}} $}")))
    (is (equalp
         (djula:render-template* template nil)
         " this is {{verbatim}} ")))
  (let ((template (compile-string "{$ {% ifequal foo bar %}yes{% else %}no{% endifequal %} $}")))
    (is
     (equalp
      (djula:render-template* template nil)
      " {% ifequal foo bar %}yes{% else %}no{% endifequal %} "))))

(def-test autoescape-test (:compile-at :definition-time)
  (let ((djula:*catch-template-errors-p* nil))
    (is (equalp
         (let ((template (compile-string "{% autoescape on %}{{foo}}{% endautoescape %}")))
           (djula:render-template* template nil :foo "<b>Hello</b>"))
         "&lt;b&gt;Hello&lt;/b&gt;"))
    (is (equalp
         (let ((template (compile-string "{% autoescape off %}{{foo}}{% endautoescape %}")))
           (djula:render-template* template nil :foo "<b>Hello</b>"))
         "<b>Hello</b>"))
    (is (equalp
         (let ((template (compile-string "{% autoescape on %}{{foo | safe}}{% endautoescape %}")))
           (djula:render-template* template nil :foo "<b>Hello</b>"))
         "<b>Hello</b>"))
    (is (equalp
         (let ((template (compile-string "{% autoescape off %}{{foo | escape}}{% endautoescape %}")))
           (djula:render-template* template nil :foo "<b>Hello</b>"))
         "&lt;b&gt;Hello&lt;/b&gt;"))

    (is (equalp
         (let ((template (compile-string "{% autoescape yes %}{{foo}}{% endautoescape %}")))
           (djula:render-template* template nil :foo "<b>Hello</b>"))
         "&lt;b&gt;Hello&lt;/b&gt;"))
    (is (equalp
         (let ((template (compile-string "{% autoescape no %}{{foo}}{% endautoescape %}")))
           (djula:render-template* template nil :foo "<b>Hello</b>"))
         "<b>Hello</b>"))
    (is (equalp
         (let ((template (compile-string "{% autoescape yes %}{{foo | safe}}{% endautoescape %}")))
           (djula:render-template* template nil :foo "<b>Hello</b>"))
         "<b>Hello</b>"))
    (is (equalp
         (let ((template (compile-string "{% autoescape no %}{{foo | escape}}{% endautoescape %}")))
           (djula:render-template* template nil :foo "<b>Hello</b>"))
         "&lt;b&gt;Hello&lt;/b&gt;"))

    ;; Invalid argument
    (signals template-error
      (compile-string "{% autoescape nope %}{{foo | escape}}{% endautoescape %}"))
    (signals template-error
      (compile-string "{% autoescape true %}{{foo | escape}}{% endautoescape %}"))
    (signals template-error
      (compile-string "{% autoescape %}{{foo | escape}}{% endautoescape %}"))))

(def-test ifchanged-test (:compile-at :definition-time)
  (let ((template (compile-string "start {% for i in data %}{% ifchanged i %}[changed] {% endifchanged %}{{ i }} {% endfor %}end")))
    (is (equalp (djula:render-template* template nil :data '(1 2))
                "start [changed] 1 [changed] 2 end"))
    (is (equalp (djula:render-template* template nil :data '(1 1))
                "start [changed] 1 1 end"))
    (is (equalp (djula:render-template* template nil :data '(1 1))
                "start [changed] 1 1 end"))))
