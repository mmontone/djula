(in-package #:djula-test)

(in-suite djula-test)

(def-test variables-test (:compile-at :definition-time)
  (is (equal '(:truncatechars "30")
             (djula::parse-filter-string "truncatechars:\"30\"")))
  (is (equal '(:truncatechars "30")
             (djula::parse-filter-string "truncatechars:30")))
  (is (equal '(:foo :bar :baz 2)
             (djula::parse-variable-phrase "foo.bar.baz.2")))
  (is (equal '((:foo :bar :baz 2) (:truncatechars "30") (:upper))
             (djula::parse-variable-clause "foo.bar.baz.2 | truncatechars:30 | upper"))))

(def-test apply-keys/indexes-test (:compile-at :definition-time)
  (is (equal 1
             (djula::apply-keys/indexes
              '((:alist-a . 1) (:alist-b . (:plist-a #(3 2 1 0) :plist-b nil)))
              '(:alist-b :plist-a 2)))))

(defclass my-obj ()
  ((name :accessor name
         :initform "my-obj")))

(defmethod id ((my-obj my-obj))
  (declare (ignore my-obj))
  22)

(defun fid (my-obj)
  (declare (ignore my-obj))
  33)

(def-test variables-accessing-test (:compile-at :definition-time)
  (let ((djula:*catch-template-errors-p* nil))
    (let ((template (compile-string "{{foo}}")))
      (let ((*strict-mode* t))
        (signals error
          (render-template*
           template nil)))
      (let ((*strict-mode* nil))
        (is (equalp (render-template* template nil)
                    "")))
      (is (equalp
           (render-template*
            (compile-string "{% if foo %}{{foo}}{% endif %}")
            nil)
           ""))
      (is (equalp
           (render-template*
            template nil :foo nil)
           ""))
      (is (equalp
           (render-template*
            template
            nil
            :foo "foo")
           "foo"))
      (is (equalp
           (render-template*
            template
            nil
            :foo 2)
           "2")))


    (let ((template (compile-string "{{foo}}{{bar}}"))
          (*strict-mode* t))
      (signals error
        (render-template*
         template nil))
      (signals error
        (render-template*
         template nil
         :foo "foo"))
      (signals error
        (render-template*
         template nil
         :bar "bar"))
      (is (equalp
           (render-template*
            template nil
            :foo "foo" :bar "bar")
           "foobar")))

    (let ((template (compile-string "{% if foo %}{{foo}}{% endif %}{% if bar %}{{bar}}{% endif %}")))
      (equalp
       (render-template*
        template nil)
       "")
      (equalp
       (render-template*
        template nil
        :foo "foo")
       "foo")
      (equalp
       (render-template*
        template nil
        :bar "bar")
       "bar")
      (is (equalp
           (render-template*
            template nil
            :foo "foo" :bar "bar")
           "foobar")))

    (let ((template (compile-string "{{foo.bar}}"))
          (*strict-mode* t))
      (signals error
        (render-template*
         template
         nil))
      (is (equalp
           (render-template*
            template
            nil
            :foo (list :bar "bar"))
           "bar")))

    ;; Slot accessing
    (is (equalp
         (let ((djula:*template-package* :djula-test))
           (let ((template (compile-string "{{obj.name}}")))
             (djula:render-template* template nil :obj (make-instance 'my-obj))))
         "my-obj"))

    ;; Method accessing
    (is (equalp
         (let ((djula:*template-package* :djula-test))
           (let ((template (compile-string "{{obj.id}}")))
             (djula:render-template* template nil :obj (make-instance 'my-obj))))
         "22"))

    ;; Function accessing
    (is (equalp
         (let ((djula:*template-package* :djula-test))
           (let ((template (compile-string "{{obj.fid}}")))
             (djula:render-template* template nil :obj (make-instance 'my-obj))))
         "33"))))

(def-test escaping-test (:compile-at :definition-time)
  (let ((djula:*catch-template-errors-p* nil))
    ;; Test defaults
    (is (equalp
         (let ((template (compile-string "{{foo}}")))
           (djula:render-template* template nil :foo "<b>Hello</b>"))
         "&lt;b&gt;Hello&lt;/b&gt;"))
    (is (equalp
         (let ((template (compile-string "{{foo | safe}}")))
           (djula:render-template* template nil :foo "<b>Hello</b>"))
         "<b>Hello</b>"))
    ;; Auto escape setting
    (let ((djula:*auto-escape* t))
      (is (equalp
           (let ((template (compile-string "{{foo}}")))
             (djula:render-template* template nil :foo "<b>Hello</b>"))
           "&lt;b&gt;Hello&lt;/b&gt;")))
    (let ((djula:*auto-escape* nil))
      (is (equalp
           (let ((template (compile-string "{{foo}}")))
             (djula:render-template* template nil :foo "<b>Hello</b>"))
           "<b>Hello</b>")))
    ;; Safe and auto-escape priorities
    (let ((djula:*auto-escape* t))
      (is (equalp
           (let ((template (compile-string "{{foo | safe}}")))
             (djula:render-template* template nil :foo "<b>Hello</b>"))
           "<b>Hello</b>")))
    (let ((djula:*auto-escape* nil))
      (is (equalp
           (let ((template (compile-string "{{foo | escape}}")))
             (djula:render-template* template nil :foo "<b>Hello</b>"))
           "&lt;b&gt;Hello&lt;/b&gt;")))))

(def-test default-template-arguments (:compile-at :definition-time)
  (let ((djula:*catch-template-errors-p* nil)
        (fn (compile-string "{{foo}}")))
    (let ((*strict-mode* t))
      (signals error
        (render-template* fn nil)))
    (let ((*strict-mode* nil))
      (is (string= (render-template* fn nil) "")))
    (is (string= "foo"
                 (render-template* fn nil :foo "foo")))
    (setf (getf djula:*default-template-arguments* :foo) "foo")
    (is (string= "foo"
                 (render-template* fn nil))
        "Default arguments are rendered")
    (is (string= "bar"
                 (render-template* fn nil :foo "bar"))
        "Passed arguments have priority over default arguments")
    (setf djula:*default-template-arguments* nil)))
