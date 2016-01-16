(in-package #:grab-bag)

;; this monster is made from the un-typed bag
;; edit that first and move changes here.

;; # TODO FOR TYPED VERSION
;;
;; - add and profile fast-find and fast-position
;; - make remove-item handled 1 item and 0 items

(defconstant +release-mode+ nil)

(defmacro def-typed-bag (element-type &body eraser-instance)
  (assert (and (symbolp element-type)
	       (not (null element-type))
	       (= (length eraser-instance) 1)))
  (labels ((symb (&rest args) (values (intern (format nil "~{~a~}" args)))))
    (let* ((root-type (symb '%%root-bag-type-for- element-type))
	   (bag-type (symb '%typed-bag-for- element-type))
	   (rummager-type (symb element-type '-rummager))
	   (added-callbacks (symb root-type '-added-callbacks))
	   (extension (symb root-type '-extension))
	   (holes-in-items (symb root-type '-holes-in-items))
	   (item-cache (symb root-type '-item-cache))
	   (items (symb root-type '-items))
	   (items-to-cache-indices (symb root-type '-items-to-cache-indices))
	   (parent (symb root-type '-parent))
	   (removed-callbacks (symb root-type '-removed-callbacks))
	   (rummagers (symb bag-type '-rummagers))
	   (predicate (symb rummager-type '-predicate))
	   (add-item (symb 'add-item-to- element-type '-bag))
	   (remove-item (symb 'remove-item-from- element-type '-bag))
	   (remove-all (symb 'remove-all-from- element-type '-bag))
	   (eraser (gensym "eraser")))
      `(progn
	 (defstruct ,root-type
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
	    (make-array 0 :element-type '(function (,root-type ,element-type) t)
			:adjustable t :fill-pointer 0)
	    :type (array (function (,root-type ,element-type) t) (*)))
	   (removed-callbacks
	    (make-array 0 :element-type '(function (,root-type ,element-type) t)
				:adjustable t :fill-pointer 0)
	    :type (array (function (,root-type ,element-type) t) (*)))
	   (extension 100 :type fixnum)
	   (parent nil :type (or ,root-type null)))

	 (defstruct (,rummager-type (:include ,root-type))
	   (predicate (error "predicate is mandatory to start rummaging")
		      :type (function (,element-type) boolean)))

	 (defstruct (,bag-type (:include ,root-type))
	   (rummagers (make-array 0 :element-type ',rummager-type :adjustable t
				  :fill-pointer 0)
		      :type (array ,rummager-type (*))))

	 (defmethod print-object ((object ,bag-type) stream)
	   (format stream ,(format nil "#<BAG-OF-~s (~~s)>" element-type)
		   (length (,item-cache object))))

	 (defmethod print-object ((object ,rummager-type) stream)
	   (format stream ,(format nil "#<RUMMAGER-FOR-BAGS-OF-~s (~~s) (~~s)>"
				   element-type)
		   (length (,item-cache object))
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
		      (make-array 0 :element-type '(function (,root-type ,element-type) t)
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
			(setf (,parent rummager) bag)
			(loop :for item :across (,item-cache bag) :do
			   (%parent-on-added rummager item)))
		      bag)
		    (%remove-rummager (bag rummager)
		      (declare (,bag-type bag) (,rummager-type rummager))
		      (setf (,rummagers bag) (delete rummager (,rummagers bag) :count 1))
		      (setf (,parent rummager) nil)
		      bag)
		    (%parent-on-added (rummager item)
		      (declare (,rummager-type rummager) (,element-type item))
		      (when (funcall (,predicate rummager)
				     item)
			(,add-item rummager item)))
		    (%parent-on-removed (rummager item)
		      (declare (,rummager-type rummager) (,element-type item))
		      (,remove-item rummager item nil)))

	     (defun ,(symb 'bag-of- element-type '!) (&optional (min-extension 100))
	       (declare (fixnum min-extension))
	       (,(symb 'make- bag-type) :extension min-extension))

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
			((function (,root-type ,element-type) t) callback))
	       (vector-push-extend callback (,added-callbacks bag))
	       bag)

	     (defun ,(symb 'add-on- element-type '-removed-callback) (bag callback)
	       (declare (,bag-type bag)
			((function (,root-type ,element-type) t) callback))
	       (vector-push-extend callback (,removed-callbacks bag))
	       bag)

	     (defun ,(symb 'remove-on- element-type '-added-callback) (bag callback)
	       (declare (,bag-type bag)
			((function (,root-type ,element-type) t) callback))
	       (setf (,removed-callbacks bag)
		     (delete callback (,removed-callbacks bag) :count 1))
	       bag)

	     (defun ,(symb 'remove-on- element-type '-removed-callback) (bag callback)
	       (declare (,bag-type bag)
			((function (,root-type ,element-type) t) callback))
	       (setf (,removed-callbacks bag)
		     (delete callback (,removed-callbacks bag) :count 1))
	       bag)

	     (defun ,(symb 'rummage- element-type '-bag ) (bag predicate)
	       (declare (,bag-type bag)
			((function (,element-type) boolean) predicate))
	       (let ((r (,(symb 'make- rummager-type) :predicate predicate)))
		 (%add-rummager bag r)
		 r))

	     (defun ,(symb 'stop- element-type '-rummaging) (bag rummage)
	       (declare (,bag-type bag) (,rummager-type rummage))
	       (%remove-rummager bag rummage))))))))
