;;;; locale.lisp

#|

This file helps create, update and maintain cl-locale translation dictionary files
of a djula project. It does not depend on the code of cl-locale but certainly
depends on the format of the cl-locale files (currently an association list).

|#

(in-package #:djula.locale)

(alexandria:define-constant +whitespace+
  (remove-duplicates
   (coerce (list #\Space #\Tab #\Linefeed #\Return #\Newline #\Page
                 #.(code-char 160)) ; No-Break-Space
           'string))
  :test #'string=
  :documentation "Whitespace characters.")

(defun trim-whitespace (string)
  "STRING without whitespace at ends."
  (string-trim +whitespace+ string))

(defun parse-template-string (string)
  (djula::parse-template-string string))

(defun string-string-p (s)
  "test if s is a string that contains a string representation"
  (and (stringp s)
       (eq #\" (alexandria:first-elt s))
       (eq #\" (alexandria:last-elt s))))

(defun string-translate-strings (string)
  "given a djula html template string, find all the substrings to be translated"
  (remove-duplicates
   (let ((*read-eval* nil))
     (loop for l in (parse-template-string string)
                 for x = (first l)
                 for y = (trim-whitespace (second l))
                 when (and (eq x :unparsed-translation)
                           (string-string-p y))
                   collect (read-from-string y)))  
   :test #'string=))

(defun file-template-translate-strings (file)
  "given a djula html template file, find all the strings to be translated"
  (string-translate-strings (alexandria:read-file-into-string file)))


(defun directory-translate-strings (dir &key (recurse nil))
  "given a directory of djula html template files find all the strings to be translated"
  (sort
   (remove-duplicates
    (append (mapcan #'file-template-translate-strings
                    (uiop:directory-files  dir "*.html"))
            (when recurse
              (mapcan #'directory-translate-strings
                      (uiop:subdirectories dir))))
    :test #'string=)
   #'string-lessp))


(defun locale-list (message-file translate-strings)
  "return an augmented dictionary of a cl-locale message file with the translate strings.
 Does not update the file."
  (let ((dictionary
          (with-open-file (s message-file :direction :input)
            (read s))))
    (sort (dolist (s translate-strings dictionary)
            (unless (assoc s dictionary :test #'string=)
              (push (cons s "") dictionary)))
          #'string-lessp
          :key #'car)))


(defun alter-pathname (pathname &rest options)
  " alter pathname as specified in the options "
  (apply 'make-pathname :defaults (pathname  pathname) options))

(defun backup-file (file)
  "backup the file by copying it from filename.ext to filename-n.ext
 where n is the next available number."
  (when (uiop:file-exists-p file)
   (let ((name (pathname-name file))
         (n 0)
         (bfile nil))
     (loop while (or (null bfile)
                     (uiop:file-exists-p bfile))
           do (setf bfile
                    (alter-pathname file :name (format nil "~a-~a" name n)))
              (incf n))
     (uiop:copy-file file bfile)
     bfile)))

(defun update-locale-list (message-file translate-strings)
  "update a cl-locale message file with the list of translate strings"
  (backup-file message-file)
  (let ((dict (locale-list message-file translate-strings)))
    (with-open-file (o message-file :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
      (format o "(~%~{ ~s~%~})~%" dict))
    dict))

(defun update-project (template-dir locale-dir)
  "update a djula project informing the template directory and the directory
of subdirectories holding the cl-locale dictionary files"
  (let ((strings (directory-translate-strings template-dir :recurse t))
        (count 0))
    (dolist (dir (uiop:subdirectories locale-dir) count)
      (let ((message-file (merge-pathnames "message.lisp" dir)))
        (update-locale-list message-file strings)
        (print message-file)
        (incf count)))))

(defun update-caveman-project (project)
  "update the cl-locale dictionary files with the djula translate strings.
Project should coincide with project (asdf) name of the caveman project.
This assumes the project uses the standard caveman2 directory structure.
It does not depend on any caveman source code and
caveman or your project need not be loaded. "
  (let* ((root-dir (asdf:system-source-directory project))
         (locale-dir (merge-pathnames #P"i18n/" root-dir) )
         (template-dir (merge-pathnames #P"templates/" root-dir)))
    (update-project template-dir locale-dir)))



