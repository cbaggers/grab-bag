(in-package #:grab-bag)

(defun rummage (bag predicate)
  (let ((r (make-rummage :predicate predicate)))
    (%add-rummager bag r)
    r))

(defun stop-rummaging (rummage)
  ())

(defun %parent-on-added (parent item)
  )

(defun %parent-on-removed (parent item)
  )
