(in-package #:djula)

(defun process-token (token rest-token-list)
  (destructuring-bind (name . args) token
    (let ((f (find-token-processor name)))
      (if (null f)
          ;; XXX: Move this to find-token-processor, as the 'default' token
          ;; processor.
          ;; If it is not a processor, then just collect the token
          (cons token (process-tokens rest-token-list))
          ;; else, we apply the processor
          (progn
            (handler-case
                (apply f rest-token-list args)
              (template-error (e1)
                ;; Parse errors can be reported by substituting a simple string
                ;; token.
                (if (and *catch-template-errors-p*
                         (not *fancy-error-template-p*))
                    (cons
                     (list :string
                           (princ-to-string e1))
                     (process-tokens rest-token-list))
                    (error e1)))
              (error (e2)
                ;; Parse errors can be reported by substituting a simple string
                ;; token.
                (let ((msg (template-error-string* e2 "There was an error processing the token ~A" token)))
                  (if (and *catch-template-errors-p*
                           (not *fancy-error-template-p*))
                      (cons
                       (list :string
                             msg)
                       (process-tokens rest-token-list))
                      (template-error msg))))))))))

(defun process-tokens (tokens)
  (when tokens
    (process-token (first tokens) (rest tokens))))

(def-token-processor :comment (comment-string) rest
  ":COMMENT tokens are removed by PROCESS-TOKENS"
  (process-tokens rest))

;;; TODO: I suspect this is a false optimization.
(def-token-processor :string (string) unprocessed
  "adjacent :STRING tokens are concatenated together by PROCESS-TOKENS as a small optimization"
  (let ((processed (process-tokens unprocessed)))
    (if (or (null processed)
	    (not (eql (caar processed) :string)))
	`((:string ,string) ,@processed)
	`((:string ,(format nil "~A~A" string (second (first processed))))
	  ,@(rest processed)))))
