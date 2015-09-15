(in-package #:djula)

;; access library patches. we need to know if the value was accessed, regardless of
;; the result being nil. so, access should return if it the object was actually accessed
;; as a second value, regardless of result being nil. This is a problem with CL,
;; not having a real boolean datatype.

(defgeneric plist-val (id list &key test key)
  (:documentation "get a value out of a plist based on its key")
  (:method (id list &key (test #'access::equalper) (key #'identity))
    (iter (for (k v) on list by #'cddr)
	  (let ((found (funcall test (funcall key k) id)))
	    (if found
		(return-from plist-val (values v found)))))))

(defgeneric do-access  (o k &key type test key skip-call?)
  (:method ((o list) k &key type test key skip-call?)
    (declare (ignore skip-call?))
    (if (or (eql type :alist)
            (and (null type) (consp (first o))))
        ;;alist
	(let ((assoc (assoc k o :test test :key key)))
	  (values (cdr assoc)
		  (and assoc t)))
        ;;plist
	(plist-val k o :test test :key key)))

  (:method ((o array) k &key type test key skip-call?)
    (declare (ignore type test key skip-call?))
    (if (< k (length o))
	(values (aref o key) t)))

  (:method ((o hash-table) k &key type test key skip-call?)
    (declare (ignore type test key skip-call?))
    (multiple-value-bind (res found) (gethash k o)
      (if found
          (values res found)
          (awhen (ignore-errors (string k))
            (gethash it o)))))
  
  (:method (o  k &key type test key skip-call?)
    ;; not specializing on standard-object here
    ;; allows this same code path to work with conditions (in sbcl)
    (let ((actual-slot-name (access::has-slot? o k)))
      (cond
        ;; same package as requested, must be no accessor so handle slots
        ((eql actual-slot-name k)
         (when (slot-boundp o k)
           (values (slot-value o k) t)))

        ;; lets recheck for an accessor in the correct package
        (actual-slot-name
         (access o actual-slot-name :type type :test test :key key
                                    :skip-call? skip-call?))
        ))))

(defun access (o k &key type (test #'access::equalper) (key #'identity)
                   skip-call?)
  "Access plists, alists, arrays, hashtables and clos objects
   all through the same interface

   skip-call, skips trying to call "
  ;; make these easy to have the same defaults everywhere
  (unless test (setf test #'access::equalper))
  (unless key (setf key #'identity))
  (multiple-value-bind (res called)
      (unless skip-call?
        ;; lets suppress the warning if it is just being called through access
        (access::call-if-applicable o k :warn-if-not-a-fn? nil))
    (if called
        (values res t)
        (do-access o k :test test :key key :type type))))
