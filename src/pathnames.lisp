;; Code from pathname-utils library, slightly modified.
;; ABCL does not parse current-directory pathnames as needed by Djula.
;; (parse-namestring "./foo") is parsed to #p"foo". Current directory information is lost.
;; This is aimed at fixing this for ABCL on Linux at least.

(defpackage :portable-pathnames
  (:use :cl)
  (:export :parse-unix-namestring))

(in-package :portable-pathnames)

(defun parse-unix-namestring (namestring &key (as :file) junk-allowed)
  (if (string= "" namestring)
      #p""
      (let ((base (case (char namestring 0)
                    (#\~ '(:home :absolute))
                    (#\/ '(:absolute))
                    (T '(:relative))))
            (buf (make-string-output-stream))
            (name NIL)
            (type NIL))
        (flet ((push-file ()
                 (let* ((leftover (get-output-stream-string buf))
                        (dot (position #\. leftover :from-end T)))
                   (when (string/= "" leftover)
                     (case dot
                       ((0 NIL) (setf name leftover))
                       (T (setf name (subseq leftover 0 dot)
                                type (subseq leftover (1+ dot))))))))
               (push-dir ()
                 (let* ((dirname (get-output-stream-string buf)))
                   (cond ((string= "" dirname))
                         ((string= "." dirname)
                          (push "." base))
                         ((string= ".." dirname) (push :back base))
                         (T (push dirname base))))))
          (loop for i from (if (eql :relative (first base)) 0 1) below (length namestring)
                for char = (char namestring i)
                do (case char
                     (#\/ (push-dir))
                     (#\Nul (unless junk-allowed
                              (cerror "Ignore the character" "Illegal character ~c in namestring:~%  ~a"
                                      char namestring)))
                     (T (write-char char buf)))
                finally (ecase as
                          (:file (push-file))
                          (:directory (push-dir)))))
        (make-pathname :name name :type type :directory (unless (equal base '(:relative)) (reverse base))))))
