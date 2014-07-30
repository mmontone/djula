(in-package #:djula-test)

(in-suite djula-test)

(defun tag (name &rest args)
  (let ((fn (apply (or (get name 'djula::tag-compiler)
                       (get name 'djula::token-compiler))
                   args))
        (*template-arguments* nil))
    (with-output-to-string (s)
      (funcall fn s))))

(test cycle
  (is (string= "010101"
               (let ((fn (apply (get :cycle 'djula::tag-compiler) '(0 1)))
                     (djula::*template-arguments* nil))
                 (with-output-to-string (s)
                   (dotimes (_ 6)
                     (funcall fn s)))))))

(test js
  (let ((djula::*accumulated-javascript-strings* nil))
    (is (string= "" (tag :parsed-js "http://cdn.sockjs.org/sockjs-0.3.min.js")))
    (is (string= "
<script type='text/javascript' src=\"http://cdn.sockjs.org/sockjs-0.3.min.js\"></script>"
               (tag :emit-js)))))

(test language
  (let ((djula::*current-language* :english))
    (is (string= "" (tag :set-language :lojban)))
    (is (string= "LOJBAN" (tag :show-language)))))

(test logic
  (let ((fn (djula::compile-logical-statement (list "Thursday"))))
    (let ((djula::*template-arguments* '((:thursday . t))))
      (is (funcall fn)))
    (let ((djula::*template-arguments* '((:thursday . nil))))
      (is (not (funcall fn))))))

(test conditional-test
  (let ((template (djula::compile-string "{% if foo %}foo{% else %}bar{% endif %}")))
    (is (equalp
	 (djula:render-template* template nil)
	 "bar"))
    (is (equalp
	 (djula:render-template* template nil :foo t)
	 "foo"))))

(test loop-test
  (let ((template (djula::compile-string "<ul>{% for elem in list %}<li>{{elem}}</li>{% endfor %}</ul>")))
    (is (equalp
	 (djula:render-template* template nil)
	 "<ul></ul>"))
    (is (equalp
	 (djula:render-template* template nil :list (list "foo" "bar"))
	 "<ul><li>foo</li><li>bar</li></ul>"))))
