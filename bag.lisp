(in-package #:grab-bag)

(defconstant +release-mode+ nil)

;; # NOTES FOR TYPED VERSION
;;
;; - take a function for comparison so #'add-item doesnt need arg
;; - add fast-find

(defstruct %bag

  ;; the master array of items in the bag
  (items (%make-items-array) :type (array t (*)))

  ;; a holeless array with the same elements as items
  (item-cache (%make-item-cache) :type (array t (*)))

  ;; an array where the nth element hold the index into the
  ;; item-cache where you can find the nth element of 'items
  ;; this is used to optimize removing from the cache without
  ;; having to mark it dirty and regenerate.
  (items-to-cache-indices (%make-item-to-cache-array) :type (array fixnum (*)))

  ;; a list of empty slots in the items array
  ;; this is used in add-item to avoid extending the array if
  ;; elements are avaiable
  (holes-in-items nil :type list)

  ;; called when item added
  (added-callbacks (%make-callback-array)
		   :type (array (function (%bag t) t) (*)))

  ;; called when item removed
  (removed-callbacks (%make-callback-array)
		     :type (array (function (%bag t) t) (*)))

  ;; ammount to extend internal arrays by when resizing
  (extension 100 :type fixnum))

(defstruct (rummage (:include %bag))
  (predicate (error "predicate is mandatory to start rummaging")
	     :type (function (t) boolean)))

(defstruct (bag (:include %bag))
  (rummagers (make-array 0 :element-type 'rummage :adjustable t
			 :fill-pointer 0)
	     :type (array rummage (*))))

;;----------------------------------------------------------------------

(defun %make-items-array ()
  (make-array 0 :adjustable t :fill-pointer 0))

(defun %make-item-cache (&optional items)
  (if items
      (let ((l (length items)))
	(make-array l :adjustable t :fill-pointer l
		    :initial-contents items))
      (make-array 0 :adjustable t :fill-pointer 0)))

(defun %make-item-to-cache-array ()
  (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer 0))

(defun %make-callback-array ()
  (make-array 0 :element-type '(function (%bag t) t)
	      :adjustable t :fill-pointer 0))

;;----------------------------------------------------------------------

(defun bag! (&optional (min-extension 100))
  (make-bag :extension min-extension))

(defun destroy-bag (bag)
  (remove-all bag)
  (setf (%bag-added-callbacks bag) (%make-callback-array)
	(%bag-removed-callbacks bag) (%make-callback-array))
  nil)


;;----------------------------------------------------------------------

(defun get-items (bag)
  (%bag-item-cache bag))

;;----------------------------------------------------------------------

(defun add-item (bag item &optional (test #'eq))
  (unless (find item (%bag-items bag) :test test)
    (let ((extension (%bag-extension bag)))
      (if (%bag-holes-in-items bag)
	  (let ((i (pop (%bag-holes-in-items bag))))
	    (setf (aref (%bag-items bag) i) item
		  (aref (%bag-items-to-cache-indices bag) i) item))
	  (progn
	    (vector-push-extend (length (bag-item-cache bag))
				(bag-items-to-cache-indices bag))
	    (vector-push-extend item (%bag-items bag) extension)))
      (vector-push-extend item (%bag-item-cache bag) extension)))
  (loop :for rummager :across (bag-rummagers bag) :do
     (%parent-on-added bag item))
  (loop :for callback :across (%bag-added-callbacks bag) :do
     (funcall callback bag item))
  bag)

;;----------------------------------------------------------------------

(defun remove-item (bag item &optional (test #'eq))
  (let ((pos (position item (%bag-items bag) :test test)))
    (if pos
	(progn
	  (setf (aref (%bag-items bag) pos) nil)
	  (%remove-item-from-cache bag pos)
	  (push pos (%bag-holes-in-items bag))
	  (loop :for rummager :across (bag-rummagers bag) :do
	     (%parent-on-removed bag item))
	  (loop :for callback :across (%bag-removed-callbacks bag) :do
	     (funcall callback bag item))
	  bag)
	(error "No item ~s in bag ~s" item bag))))

(defun %remove-item-from-cache (bag item-pos)
  ;; swap with last element and (1- fill-pointer)
  (let* ((item-cache (%bag-item-cache bag))
	 (last-pos (1- (length item-cache)))
	 (last-cache-item (aref item-cache last-pos))

	 (index-cache (%bag-items-to-cache-indices bag))
	 (cache-remove-pos (aref index-cache item-pos))

	 ;; find the position in the item list that the last-cache-item
	 (item-moved-pos (position last-cache-item (%bag-items bag))))
    ;; take the last item from the cache and place it in the slot
    ;; where the item being removed was
    (setf (aref item-cache cache-remove-pos) last-cache-item)

    ;; update the item->cache index for the moved item
    (setf (aref index-cache last-pos) (aref index-cache item-moved-pos))
    (unless +release-mode+
      (setf (aref index-cache item-pos) -1))

    ;; the item cache and item->cache map now need shortening
    (decf (fill-pointer item-cache)))
  bag)


(defun remove-all (bag)
  (let ((items (%bag-items bag)))
    (setf (%bag-items bag) (%make-items-array)
	  (%bag-item-cache bag) (%make-item-cache)
	  (%bag-holes-in-items bag) nil
	  (%bag-added-callbacks bag) (%make-callback-array)
	  (%bag-removed-callbacks bag) (%make-callback-array))
    (loop :for rummager :across (bag-rummagers bag) :do
       (loop :for item :across items :do
	  (%parent-on-added bag item)))
    (loop :for callback :across (%bag-removed-callbacks bag) :do
       (loop :for item :across items :do
	  (funcall callback bag item))))
  bag)

;;----------------------------------------------------------------------

(defun add-on-added-callback (bag callback)
  (vector-push-extend callback (%bag-added-callbacks bag))
  bag)

(defun add-on-removed-callback (bag callback)
  (vector-push-extend callback (%bag-removed-callbacks bag))
  bag)

(defun %add-rummager (bag rummager)
  (vector-push-extend rummager (bag-rummagers bag))
  bag)

(defun %remove-rummager (bag rummager)
  (vector-push-extend rummager (bag-rummagers bag))
  bag)

;;----------------------------------------------------------------------

(defun rummage (bag predicate)
  (let ((r (make-rummage :predicate predicate)))
    (%add-rummager bag r)
    r))

(defun stop-rummaging (rummage)
  )

(defun %parent-on-added (parent item)
  )

(defun %parent-on-removed (parent item)
  )
