(in-package #:grab-bag)

;; # TODO FOR TYPED VERSION
;;
;; - add and profile fast-find and fast-position

(defconstant +release-mode+ nil)

(defstruct %rummager-base)

(defmacro def-typed-bag (element-type &body eraser-instance)
  (assert (and (symbolp element-type)
	       (not (null element-type))
	       (= (length eraser-instance) 1)))
  (labels ((symb (&rest args) (values (intern (format nil "~{~a~}" args)))))
    (let* ((bag-type (symb '%typed-bag-for- element-type))
	   (rummager-type (symb element-type '-rummager))
	   (added-callbacks (symb bag-type '-added-callbacks))
	   (extension (symb bag-type '-extension))
	   (holes-in-items (symb bag-type '-holes-in-items))
	   (item-cache (symb bag-type '-item-cache))
	   (items (symb bag-type '-items))
	   (items-to-cache-indices (symb bag-type '-items-to-cache-indices))
	   (removed-callbacks (symb bag-type '-removed-callbacks))
	   (rummagers (symb bag-type '-rummagers))
	   (predicate (symb rummager-type '-predicate))
	   (add-item (symb 'add-item-to- element-type '-bag))
	   (remove-item (symb 'remove-item-from- element-type '-bag))
	   (remove-all (symb 'remove-all-from- element-type '-bag))
	   (init (symb 'bag-of- element-type '!))
	   (start-rummaging (symb 'rummage- element-type '-bag))
	   (stop-rummaging (symb 'stop- element-type '-rummaging))
	   (rummager-bag (symb rummager-type '-bag))
	   (rummager-parent (symb rummager-type '-parent))
	   (eraser (gensym "eraser")))
      `(progn
	 (defstruct ,bag-type
	   (items
	    (make-array 0 :element-type '(or ,element-type null) :adjustable t
			:fill-pointer 0)
	    :type (array (or ,element-type null) (*)))
	   (item-cache
	    (make-array 0 :element-type '(or ,element-type null) :adjustable t
			:fill-pointer 0)
	    :type (array (or ,element-type null) (*)))
	   (items-to-cache-indices
	    (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer 0)
	    :type (array fixnum (*)))
	   (holes-in-items nil :type list)
	   (added-callbacks
	    (make-array 0 :element-type '(function (,bag-type ,element-type) t)
			:adjustable t :fill-pointer 0)
	    :type (array (function (,bag-type ,element-type) t) (*)))
	   (removed-callbacks
	    (make-array 0 :element-type '(function (,bag-type ,element-type) t)
				:adjustable t :fill-pointer 0)
	    :type (array (function (,bag-type ,element-type) t) (*)))
	   (extension 100 :type fixnum)
	   (rummagers (error "rummage array missing")
		      :type (array %rummager-base (*))))

	 (defstruct (,rummager-type (:include %rummager-base))
	   (predicate (error "predicate is mandatory to start rummaging")
		      :type (function (,element-type) boolean))
	   (bag (error "bag missing") :type ,bag-type)
	   (parent nil :type (or ,bag-type null)))

	 (defmethod print-object ((object ,bag-type) stream)
	   (format stream ,(format nil "#<BAG-OF-~s (~~s)>" element-type)
		   (length (,item-cache object))))

	 (defmethod print-object ((object ,rummager-type) stream)
	   (format stream ,(format nil "#<RUMMAGER-FOR-BAGS-OF-~s (~~s) (~~s)>"
				   element-type)
		   (length (,item-cache (,rummager-bag object)))
		   (,predicate object)))
	 (let ((,eraser (let ((tmp ,(first eraser-instance)))
			  (assert (typep tmp ',element-type)) tmp)))
	   (labels ((%make-items-array ()
		      (make-array 0 :element-type '(or ,element-type null) :adjustable t
				  :fill-pointer 0))
		    (%make-item-cache ()
		      (make-array 0 :element-type '(or ,element-type null) :adjustable t
				  :fill-pointer 0))
		    (%make-callback-array ()
		      (make-array 0 :element-type '(function (,bag-type ,element-type) t)
				  :adjustable t :fill-pointer 0))

		    (%remove-item-from-cache (bag pos-in-item-array)
		      (declare (,bag-type bag) (fixnum pos-in-item-array))
		      (let* ((item-cache (,item-cache bag))
			     (index-cache (,items-to-cache-indices bag))

			     (pos-of-last-thing-in-item-cache
			      (1- (length item-cache)))
			     (pos-of-thing-to-be-removed-in-item-cache
			      (aref index-cache pos-in-item-array)))
			;; 0
			(setf (aref (,items bag) pos-in-item-array) nil)
			(list pos-of-last-thing-in-item-cache
			      pos-of-thing-to-be-removed-in-item-cache)

			(unless (= pos-of-last-thing-in-item-cache
				   pos-of-thing-to-be-removed-in-item-cache)
			  (let* ((last-thing-in-item-cache
				  (aref item-cache
					pos-of-last-thing-in-item-cache))
				 (pos-in-item-array-of-last~thing~in~item~cache
				  (position last-thing-in-item-cache
					    (,items bag))))
			    ;; 1
			    (setf (aref item-cache
					pos-of-thing-to-be-removed-in-item-cache)
				  last-thing-in-item-cache)
			    ;; 2
			    (setf (aref index-cache
					pos-in-item-array-of-last~thing~in~item~cache)
				  (aref index-cache pos-in-item-array))))
			(unless +release-mode+
			  (setf (aref index-cache pos-in-item-array) -1))
			(setf (aref item-cache pos-of-last-thing-in-item-cache) ,eraser)
			(decf (fill-pointer item-cache))
			bag))

		    (%add-rummager (bag rummager)
		      (declare (,bag-type bag) (,rummager-type rummager))
		      (unless (find rummager (,rummagers bag) :test #'eq)
			(vector-push-extend rummager (,rummagers bag))
			(setf (,rummager-parent rummager) bag)
			(loop :for item :across (,item-cache bag) :do
			   (%parent-on-added rummager item)))
		      bag)
		    (%remove-rummager (rummager)
		      (declare (,rummager-type rummager))
		      (let ((bag (,rummager-parent rummager)))
			(declare (,bag-type bag))
			(setf (,rummagers bag)
			      (delete rummager (,rummagers bag) :count 1))
			(setf (,rummager-parent rummager) nil)
			bag))
		    (%parent-on-added (rummager item)
		      (declare (,rummager-type rummager) (,element-type item))
		      (when (funcall (,predicate rummager) item)
			(,add-item (,rummager-bag rummager) item)))
		    (%parent-on-removed (rummager item)
		      (declare (,rummager-type rummager) (,element-type item))
		      (,remove-item (,rummager-bag rummager) item nil)))

	     (defun ,init (&optional (min-extension 100))
	       (declare (fixnum min-extension))
	       (,(symb 'make- bag-type) :extension min-extension
		 :rummagers (make-array 0 :element-type ',rummager-type
					:adjustable t :fill-pointer 0)))

	     (defun ,(symb 'destroy- element-type '-bag) (bag)
	       (declare (,bag-type bag))
	       (,remove-all bag)
	       (setf (,added-callbacks bag) (%make-callback-array)
		     (,removed-callbacks bag) (%make-callback-array))
	       nil)

	     (defun ,(symb 'get-items-from- element-type '-bag) (bag)
	       (declare (,bag-type bag))
	       (,item-cache bag))

	     (defun ,add-item (bag item)
	       (declare (,bag-type bag) (,element-type item))
	       (unless (find item (,items bag) :test #'eq)
		 (let ((extension (,extension bag)))
		   (if (,holes-in-items bag)
		       (let ((i (pop (,holes-in-items bag))))
			 (setf (aref (,items bag) i) item)
			 (setf (aref (,items-to-cache-indices bag) i)
			       (length (,item-cache bag))))
		       (progn
			 (vector-push-extend (length (,item-cache bag))
					     (,items-to-cache-indices bag))
			 (vector-push-extend item (,items bag) extension)))
		   (vector-push-extend item (,item-cache bag) extension)))
	       (loop :for rummager :across (,rummagers bag) :do
		  (%parent-on-added rummager item))
	       (loop :for callback :across (,added-callbacks bag) :do
		  (funcall callback bag item))
	       bag)

	     (defun ,remove-item (bag item &optional (error-if-missing t))
	       (declare (,bag-type bag) (,element-type item)
			(boolean error-if-missing))
	       (let ((pos (position item (,items bag) :test #'eq)))
		 (if pos
		     (progn
		       (%remove-item-from-cache bag pos)
		       (push pos (,holes-in-items bag))
		       (loop :for rummager :across (,rummagers bag) :do
			  (%parent-on-removed rummager item))
		       (loop :for callback :across (,removed-callbacks bag) :do
			  (funcall callback bag item))
		       bag)
		     (when error-if-missing
		       (error "No item ~s in bag ~s" item bag)))))

	     (defun ,remove-all (bag)
	       (declare (,bag-type bag))
	       (let ((items (,items bag)))
		 (setf (,items bag) (%make-items-array)
		       (,item-cache bag) (%make-item-cache)
		       (,holes-in-items bag) nil
		       (,added-callbacks bag) (%make-callback-array)
		       (,removed-callbacks bag) (%make-callback-array))
		 (loop :for rummager :across (,rummagers bag) :do
		    (loop :for item :across items :do
		       (%parent-on-removed rummager item)))
		 (loop :for callback :across (,removed-callbacks bag) :do
		    (loop :for item :across items :do
		       (funcall callback bag item))))
	       bag)

	     (defun ,(symb 'add-on- element-type '-added-callback) (bag callback)
	       (declare (,bag-type bag)
			((function (,bag-type ,element-type) t) callback))
	       (vector-push-extend callback (,added-callbacks bag))
	       bag)

	     (defun ,(symb 'add-on- element-type '-removed-callback) (bag callback)
	       (declare (,bag-type bag)
			((function (,bag-type ,element-type) t) callback))
	       (vector-push-extend callback (,removed-callbacks bag))
	       bag)

	     (defun ,(symb 'remove-on- element-type '-added-callback) (bag callback)
	       (declare (,bag-type bag)
			((function (,bag-type ,element-type) t) callback))
	       (setf (,removed-callbacks bag)
		     (delete callback (,removed-callbacks bag) :count 1))
	       bag)

	     (defun ,(symb 'remove-on- element-type '-removed-callback) (bag callback)
	       (declare (,bag-type bag)
			((function (,bag-type ,element-type) t) callback))
	       (setf (,removed-callbacks bag)
		     (delete callback (,removed-callbacks bag) :count 1))
	       bag)

	     (defun ,start-rummaging (bag-or-rummager predicate)
	       (declare	((function (,element-type) boolean) predicate))
	       (etypecase bag-or-rummager
		 (,rummager-type
		  (,start-rummaging (,rummager-bag bag-or-rummager)
				    predicate))
		 (,bag-type
		  (let ((r (,(symb 'make- rummager-type)
			     :predicate predicate
			     :parent bag-or-rummager
			     :bag (,init))))
		    (%add-rummager bag-or-rummager r)
		    r))))

	     (defun ,stop-rummaging (rummage)
	       (declare (,rummager-type rummage))
	       (%remove-rummager rummage))))))))
