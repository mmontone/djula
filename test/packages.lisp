(in-package #:common-lisp-user)

(defpackage #:djula-test
  (:use #:common-lisp
        #:djula
        #:1am)
  (:export #:run-djula-tests))

(in-package #:djula-test)

(defmacro def-suite (name)
  (declare (ignore name)))

(defmacro in-suite (name)
  (declare (ignore name)))

(defun debug! (&rest args)
  (declare (ignore args))
  (run))

(defmacro def-test (name (&rest args) &body body)
  (declare (ignore args))
  `(test ,name ,@body))

(def-suite djula-test)

(defun run-djula-tests ()
  (debug! 'djula-test))
