(in-package #:common-lisp-user)

(defpackage #:djula-test
  (:use #:common-lisp
        #:djula
        #:fiveam)
  (:export #:run-all-tests))

(in-package #:djula-test)

(def-suite djula-test)

(defun run-all-tests ()
  (run! 'djula-test))
