(in-package #:grab-bag)

;; # NOTES FOR TYPED VERSION
;;
;; - take a function for comparison so #'add-item doesnt need arg
;; - add fast-find

(defstruct bag

  ;; the master array of items in the bag
  (items (%make-items-array) :type (array t (*)))

  ;; a list of empty slots in the above array
  (holes-in-items nil :type list)

  ;; a holeless array with the same elements as items
  (item-cache (%make-item-cache) :type (array t (*)))

  ;; quick lookup for cache state
  (cache-invalid-p nil :type boolean)

  ;; called when item added
  (added-callbacks (%make-callback-array)
		   :type (array (function (bag t) t) (*)))

  ;; called when item removed
  (removed-callbacks (%make-callback-array)
		     :type (array (function (bag t) t) (*))))

;;----------------------------------------------------------------------

(defun %make-items-array ()
  (make-array 0 :adjustable t :fill-pointer 0))

(defun %make-item-cache (&optional items)
  (if items
      (let ((l (length items)))
	(make-array l :adjustable t :fill-pointer l
		    :initial-contents items))
      (make-array 0 :adjustable t :fill-pointer 0)))

(defun %make-callback-array ()
  (make-array 0 :element-type '(function (bag t) t)
	      :adjustable t :fill-pointer 0))

;;----------------------------------------------------------------------

(defun bag! () (make-bag))

(defun %fix-up-cache (bag)
  (setf (bag-item-cache bag)
	(%make-item-cache
	 (loop :for item :across (bag-items bag)
	    :when (not (null item)) :collect item)))
  (setf (bag-cache-invalid-p bag) nil)
  bag)

;;----------------------------------------------------------------------

(defun get-items (bag)
  (when (bag-cache-invalid-p bag)
    (%fix-up-cache bag))
  (bag-item-cache bag))

;;----------------------------------------------------------------------

(defun add-item (bag item &optional (test #'eq))
  (unless (find item (bag-items bag) :test test)
    (if (bag-holes-in-items bag)
	(let ((i (pop (bag-holes-in-items bag))))
	  (setf (aref (bag-items bag) i) item))
	(vector-push-extend item (bag-items bag)))
    (if (bag-cache-invalid-p bag)
	(%fix-up-cache bag)
	(vector-push-extend item (bag-item-cache bag))))
  (loop :for callback :across (bag-added-callbacks bag) :do
     (funcall callback bag item))
  bag)

;;----------------------------------------------------------------------

(defun remove-item (bag item &optional (test #'eq))
  (let ((pos (position item (bag-items bag) :test test)))
    (if pos
	(progn
	  (setf (aref (bag-items bag) pos) nil
		(bag-item-cache bag) (%make-item-cache)
		(bag-cache-invalid-p bag) t)
	  (push pos (bag-holes-in-items bag))
	  (loop :for callback :across (bag-removed-callbacks bag) :do
	     (funcall callback bag item))
	  bag)
	(error "No item ~s in bag ~s" item bag))))

(defun remove-all (bag)
  (let ((items (bag-items bag)))
    (setf (bag-items bag) (%make-items-array)
	  (bag-item-cache bag) (%make-item-cache)
	  (bag-holes-in-items bag) nil
	  (bag-cache-invalid-p bag) nil
	  (bag-added-callbacks bag) (%make-callback-array)
	  (bag-removed-callbacks bag) (%make-callback-array))
    (loop :for callback :across (bag-removed-callbacks bag) :do
       (loop :for item :across items :do
	  (funcall callback bag item))))
  bag)

;;----------------------------------------------------------------------
