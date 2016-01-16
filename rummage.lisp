(in-package #:grab-bag)

(defstruct (rummage (:include bag))
  (predicate (error "predicate is mandatory to start rummaging")
	     :type (function (t) boolean)))

(defun rummage (bag predicate)
  (let ((r (make-rummage :predicate predicate)))
    (labels ((on-added (bag item)
	       )
	     (on-removed (bag item)
	       )))
    (add-on-added-callback bag #'on-added)
    (add-on-removed-callback bag #'on-added)))

(defun stop-rummaging (rummage)
  )
