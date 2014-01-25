;;;;
;;;; Copyright (c) 2006 Hazen P. Babcock
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining a copy 
;;;; of this software and associated documentation files (the "Software"), to 
;;;; deal in the Software without restriction, including without limitation the 
;;;; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or 
;;;; sell copies of the Software, and to permit persons to whom the Software is 
;;;; furnished to do so, subject to the following conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be included in 
;;;; all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
;;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
;;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS 
;;;; IN THE SOFTWARE.
;;;;
;;;;
;;;; cl-plplot specific macros
;;;;
;;;; hazen 6/06
;;;;

(in-package #:cl-plplot)

(defun my-read-from-string (a-string a-class &optional another-string another-class)
  "Wraps read-from-string."
  (read-from-string (concatenate 'string a-string (string a-class) 
				 (when another-string another-string)
				 (when another-class (string another-class)))))

(defmacro def-plplot-class (name parents &optional slots (documentation "..."))
  "Easy class creation."
  `(defclass ,name ,parents
     ,(when slots
	    (mapcar #'(lambda (item)
			(if (listp item)
			    (let ((name (car item))
				  (val (cadr item)))
			      `(,name :initarg ,(intern (string name) :keyword) :accessor ,name :initform ,val))
			    `(,item :initarg ,(intern (string item) :keyword) :accessor ,item :initform nil)))
		    slots))
     (:documentation ,documentation)))

(defmacro def-edit-method (class arguments &optional (documentation "..."))
  "Easy object edit method creation."
  (let ((m-name (my-read-from-string "edit-" class))
	(a-class (my-read-from-string "a-" class)))
    `(progn
       (defgeneric ,m-name (,class &key ,@arguments))
;       (defmethod ,m-name ((,a-class ,class) &key ,@arguments)
       (defmethod ,m-name ((,a-class ,class) &key ,@(mapcar #'(lambda (x) `(,x *cl-plplot-null*)) arguments))
	 ,documentation
	 ,@(mapcar #'(lambda (x) `(unless (eq ,x *cl-plplot-null*) (setf (,x ,a-class) ,x))) arguments)))))

(defmacro new-object-defun (class arguments &optional (documentation "..."))
  "Easy object creation function creation."
  (let ((f-name (my-read-from-string "new-" class)))
    `(defun ,f-name ,arguments
       ,documentation
       (make-instance ',class
		      ,@(let ((arg-list nil))
			     (dolist (x arguments)
			       (let ((tmp (cond ((and (symbolp x) (not (equal x '&key))) x)
						((listp x) (car x)))))
				 (when tmp
				   (setf arg-list (append arg-list (list (intern (string tmp) :keyword))))
				   (setf arg-list (append arg-list (list tmp))))))
			     arg-list)))))

(defmacro def-add-remove-methods (class class-slot another-class)
  "Easy addition and removal of 'sub-objects' from objects."
  (let ((a-class (my-read-from-string "a-" class))
	(a-another-class (my-read-from-string "a-" another-class))
	(add-name (my-read-from-string "add-" another-class "-to-" class))
	(remove-name (my-read-from-string "remove-" another-class "-from-" class)))
    `(progn
       (defgeneric ,add-name (,class ,another-class))
       (defmethod ,add-name ((,a-class ,class) (,a-another-class ,another-class))
	 ,(concatenate 'string (string add-name) ", ADDS " (string a-another-class) " TO " (string a-class) ".")
	 (setf (,class-slot ,a-class) (append (,class-slot ,a-class) (list ,a-another-class))))
       (defgeneric ,remove-name (,class &optional ,another-class))
       (defmethod ,remove-name ((,a-class ,class) &optional ,a-another-class)
	 ,(concatenate 'string (string remove-name) ", DESTRUCTIVELY REMOVES " (string a-another-class) " FROM " (string a-class) ". IF "
		       (string a-another-class) " IS NOT SPECIFIED THEN THE LAST " (string another-class) " IS REMOVED.")
	 (setf (,class-slot ,a-class) (if ,a-another-class
					  (remove ,a-another-class (,class-slot ,a-class))
					  (butlast (,class-slot ,a-class))))))))


