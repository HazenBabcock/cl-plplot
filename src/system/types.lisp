;;;;
;;;; All of the type definition used in cl-plplot-system.
;;;;
;;;; hazen 01/14
;;;;

(in-package #:cl-plplot-system)


;;;
;;; helper functions and macros.
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun pl-defctype-fn-name (pl-type name)
    (read-from-string (concatenate 'string (string pl-type) "-" name))))

(defmacro pl-defctype (pl-type cffi-type &key from-c to-c)
  (let ((from-c-fn-name (pl-defctype-fn-name pl-type "from-c"))
	(to-c-fn-name (pl-defctype-fn-name pl-type "to-c")))
    `(progn
       (defun ,from-c-fn-name (x)
	 ,from-c)
       (defun ,to-c-fn-name (x)
	 ,to-c)
       (defctype ,pl-type (:wrapper
			   ,cffi-type
			   :from-c ,from-c-fn-name
			   :to-c ,to-c-fn-name))
       (export (quote ,pl-type)))))


;;;
;;; types
;;;

;; boolean type
(pl-defctype plbool :int
	     :from-c (not (zerop x))
	     :to-c (if x 1 0))

;; char type
(pl-defctype plchar :char
	     :from-c (code-char x)
	     :to-c (char-code x))

;; pldata type (used for pltr-data structures)
(pl-defctype pldata :pointer
	     :from-c (declare (ignore x))
	     :to-c (if x x (null-pointer)))

;; floating point type
(pl-defctype plflt :double
	     :from-c (coerce x 'double-float)
	     :to-c (coerce x 'double-float))

;; function callback type
(pl-defctype plfunc :pointer
	     :from-c (declare (ignore x))
	     :to-c (if x (get-callback x) (null-pointer)))

;; integer type
(pl-defctype plint :int
	     :from-c (round x)
	     :to-c (round x))

;; string type
(defctype plstr :string)
(export 'plstr)

;; unicode-type
(pl-defctype plunicode :uint32
	     :from-c (round x)
	     :to-c (round x))


;;;
;;; Array types
;;;

(defparameter array-types ())

;;
;; base one-dimensional array type
;;
(defclass pl-pointer ()
  ((c-pointer
    :initform nil
    :accessor c-pointer)
   (size
    :initform 0
    :accessor size)))

(defgeneric pl-foreign-free (instance))
(defmethod pl-foreign-free ((instance pl-pointer))
  "Free the c-pointer of a pl-pointer instance."
  (when (> (size instance) 0)
    (foreign-free (c-pointer instance))))

(defgeneric pl-from-foreign (instance))
(defmethod pl-from-foreign ((instance pl-pointer))
  "Convert the c-pointer of a pl-pointer to lisp array of value."
  (cond
    ((> (size instance) 1)
     (let ((lisp-array (make-array (size instance) :element-type (lisp-type instance)))
	   (c-pointer (c-pointer instance))
	   (c-type (c-type instance)))
       (dotimes (i (size instance))
	 (setf (aref lisp-array i)
	       (convert-from-foreign (mem-aref c-pointer c-type i) c-type)))
       lisp-array))
    ((= (size instance) 1)
     (convert-from-foreign (mem-aref (c-pointer instance) (c-type instance)) (c-type instance)))))

(defgeneric pl-to-foreign (array-or-integer instance))
(defmethod pl-to-foreign (array-or-integer (instance pl-pointer))
  "Convert a lisp array or integer to a pl-pointer.
     1. Array - A foreign version of the array is created and stored in c-pointer.
     2. Integer - A empty foreign array is created with a size of integer.
     3. Nil - A null pointer."
  (if array-or-integer
      (cond
	((arrayp array-or-integer)
	 (if (= (length (array-dimensions array-or-integer)) 1)
	     ; one dimensional array
	     (let ((len (length array-or-integer))
		   (c-type (c-type instance)))
	       (setf (size instance) len)
	       (setf (c-pointer instance) (foreign-alloc c-type :count len))
	       (let ((temp-ptr (c-pointer instance)))
		 (dotimes (i len)
		   (setf (mem-aref temp-ptr c-type i) (aref array-or-integer i)))))
	     ; two-dimensional array
	     (let* ((sx (array-dimension array-or-integer 0))
		    (sy (array-dimension array-or-integer 1))
		    (len (* sx sy))
		    (c-type (c-type instance)))
	       (setf (size instance) len)
	       (setf (c-pointer instance) (foreign-alloc c-type :count len))
	       (let ((temp-ptr (c-pointer instance)))
		 (dotimes (i sx)
		   (dotimes (j sy)
		     (setf (mem-aref temp-ptr c-type (+ (* i sy) j)) (aref array-or-integer i j))))))))
	((integerp array-or-integer)
	 (progn
	   (setf (size instance) array-or-integer)
	   (setf (c-pointer instance) (foreign-alloc (c-type instance) :count array-or-integer))))
	((eql array-or-integer 'null)
	 (progn
	   (format t "'null as pointer is deprecated, use nil instead.~%")
	   (setf (size instance) 0)
	   (setf (c-pointer instance) (null-pointer))))
	(t
	 (format t "Invalid type for pl-to-foreign ~a~%" (type-of array-or-integer))))
      (progn
	(setf (size instance) 0)
	(setf (c-pointer instance) (null-pointer))))
  instance)


;; boolean array type
(defctype *plbool :pointer)
(export '*plbool)
(pushnew '*plbool array-types)

(defclass pl-pointer-bool (pl-pointer)
  ((c-type
    :initform 'plbool
    :reader c-type)
   (lisp-type
    :initform 'boolean
    :reader lisp-type)))

(defun make-*plbool (array-or-integer)
  (pl-to-foreign array-or-integer (make-instance 'pl-pointer-bool)))


;; floating point array type
(defctype *plflt :pointer)
(export '*plflt)
(pushnew '*plflt array-types)

(defclass pl-pointer-float (pl-pointer)
  ((c-type
    :initform 'plflt
    :reader c-type)
   (lisp-type
    :initform 'double-float
    :reader lisp-type)))

(defun make-*plflt (array-or-integer)
  (pl-to-foreign array-or-integer (make-instance 'pl-pointer-float)))


;; integer array type
(defctype *plint :pointer)
(export '*plint)
(pushnew '*plint array-types)

(defclass pl-pointer-integer (pl-pointer)
  ((c-type
    :initform 'plint
    :reader c-type)
   (lisp-type
    :initform 'fixnum
    :reader lisp-type)))

(defun make-*plint (array-or-integer)
  (pl-to-foreign array-or-integer (make-instance 'pl-pointer-integer)))


;; unicode array type
(defctype *plunicode :pointer)
(export '*plunicode)
(pushnew '*plunicode array-types)

(defclass pl-pointer-unicode (pl-pointer)
  ((c-type
    :initform 'plunicode
    :reader c-type)
   (lisp-type
    :initform 'fixnum
    :reader lisp-type)))

(defun make-*plunicode (array-or-integer)
  (pl-to-foreign array-or-integer (make-instance 'pl-pointer-unicode)))


;;
;; Array of strings type. 
;;
;; This is somewhere between a 1D and a 2D array type and has its own
;; special methods. Also it can only be used to take strings from Lisp to C.
;;
(defctype *plstr :pointer)
(export '*plstr)
(pushnew '*plstr array-types)

(defclass pl-pointer-string (pl-pointer)
  ())

(defmethod pl-foreign-free ((instance pl-pointer-string))
  "Free the c-pointer of a pl-pointer-string instance."
  (when (> (size instance) 0)
    (dotimes (i (size instance))
      (foreign-string-free (mem-aref (c-pointer instance) :pointer i)))
    (foreign-free (c-pointer instance))))

(defmethod pl-to-foreign (list-of-strings (instance pl-pointer-string))
  (if list-of-strings
      (let ((list-len (length list-of-strings)))
	(setf (c-pointer instance) (foreign-alloc :pointer :count list-len))
	(dotimes (i list-len)
	  (setf (mem-aref (c-pointer instance) :pointer i)
		(foreign-string-alloc (elt list-of-strings i))))
	(setf (size instance) list-len))
      (progn 
	(setf (size instance) 0)
	(setf (c-pointer instance) (null-pointer))))
  instance)

(defun make-*plstr (list-of-strings)
  (pl-to-foreign list-of-strings (make-instance 'pl-pointer-string)))


;;
;; base two-dimensional array type
;;
(defclass pl-ptr-ptr ()
  ((c-pointer
    :initform nil
    :accessor c-pointer)
   (size-x
    :initform 0
    :accessor size-x)
   (size-y
    :initform 0
    :accessor size-y)))



(defmethod pl-foreign-free ((instance pl-ptr-ptr))
  "Free the c-pointer of a pl-pointer instance."
  (when (and (> (size-x instance) 0) (> (size-y instance) 0))
    (dotimes (i (size-x instance))
      (foreign-free (mem-aref (c-pointer instance) :pointer i)))
    (foreign-free (c-pointer instance))))

(defmethod pl-from-foreign ((instance pl-ptr-ptr))
  "Convert the c-pointer of a pl-pointer to lisp array of value."
  (cond
    ((or (> (size-x instance) 1) (> (size-y instance) 1))
     (let ((lisp-array (make-array (list (size-x instance) (size-y instance)) :element-type (lisp-type instance)))
	   (c-type (c-type instance)))
       (dotimes (i (size-x instance))
	 (let ((temp-pointer (mem-aref (c-pointer instance) :pointer i)))
	   (dotimes (j (size-y instance))
	     (setf (aref lisp-array i j)
		   (convert-from-foreign (mem-aref temp-pointer c-type j) c-type)))))
       lisp-array))
    ((and (= (size-x instance) 1) (= (size-y instance) 1))
     (convert-from-foreign 
      (mem-aref (mem-aref (c-pointer instance) :pointer) (c-type instance)) (c-type instance)))))

(defmethod pl-to-foreign (array-or-list (instance pl-ptr-ptr))
  "Convert a lisp array or integer to a pl-pointer.
     1. Array - A foreign version of the array is created and stored in c-pointer.
     2. List - A empty foreign array is created with size given by list.
     3. Nil - A null pointer is created."
  (if array-or-list
      (cond
	((arrayp array-or-list)
	 (let ((size-x (array-dimension array-or-list 0))
	       (size-y (array-dimension array-or-list 1))
	       (c-type (c-type instance)))
	   (setf (size-x instance) size-x)
	   (setf (size-y instance) size-y)
	   (setf (c-pointer instance) (foreign-alloc :pointer :count size-x))
	   (dotimes (i size-x)
	     (let ((temp-pointer (foreign-alloc c-type :count size-y)))
	       (setf (mem-aref (c-pointer instance) :pointer i) temp-pointer)
	       (dotimes (j size-y)
		 (setf (mem-aref temp-pointer c-type j) (aref array-or-list i j)))))))
	((listp array-or-list)
	 (progn
	   (setf (size-x instance) (elt array-or-list 0))
	   (setf (size-y instance) (elt array-or-list 1))
	   (setf (c-pointer instance) (foreign-alloc :pointer :count (size-x instance)))
	   (dotimes (i (size-x instance))
	     (setf (mem-aref (c-pointer instance) :pointer i)
		   (foreign-alloc (c-type instance) :count (size-y instance))))))
	((eql array-or-list 'null)
	 (progn
	   (format t "'null as pointer is deprecated, use nil instead.~%")
	   (setf (size-x instance) 0)
	   (setf (size-y instance) 0)
	   (setf (c-pointer instance) (null-pointer))))
	(t
	 (format t "Invalid type for pl-to-foreign ~a~%" (type-of array-or-list))))
      (progn
	(setf (size-x instance) 0)
	(setf (size-y instance) 0)
	(setf (c-pointer instance) (null-pointer))))
  instance)


;; two-dimensional floating point array type
(defctype **plflt :pointer)
(export '**plflt)
(pushnew '**plflt array-types)

(defclass pl-ptr-ptr-float (pl-ptr-ptr)
  ((c-type
    :initform 'plflt
    :reader c-type)
   (lisp-type
    :initform 'double-float
    :reader lisp-type)))

(defun make-**plflt (array-or-list)
  (pl-to-foreign array-or-list (make-instance 'pl-ptr-ptr-float)))



;;;;
;;;; Copyright (c) 2014 Hazen P. Babcock
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
