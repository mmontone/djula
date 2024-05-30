(in-package #:djula-test)

(in-suite djula-test)

(defun throw-error ()
  (error "Error!!"))

(defun access-error (x)
  (declare (ignore x))
  (error "Error!!"))

(def-test catch-errors-test (:compile-at :definition-time)
  (let ((djula:*template-package* :djula-test))

    (let ((djula:*catch-template-errors-p* t))
      (let ((template (compile-string "{% lisp (throw-error) %}")))
        (finishes (djula:render-template* template nil))))

    (let ((djula:*catch-template-errors-p* nil))
      (let ((template (compile-string "{% lisp (throw-error) %}")))
        (signals error (djula:render-template* template nil))))

    (let ((djula:*catch-template-errors-p* t))
      (let ((template (compile-string "{{ obj.access-error }}")))
        (finishes (djula:render-template* template nil :obj 'foo))))

    (let ((djula:*catch-template-errors-p* nil))
      (let ((template (compile-string "{{ obj.access-error }}")))
        (signals error (djula:render-template* template nil :obj 'foo))))))
