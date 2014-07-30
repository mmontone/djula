(in-package #:common-lisp-user)

(defpackage #:djula-test
  (:use #:common-lisp
        #:djula
        #:fiveam)
  (:export #:run-djula-tests))

(in-package #:djula-test)

(def-suite djula-test)

(defun run-djula-tests ()
  (debug! 'djula-test))
