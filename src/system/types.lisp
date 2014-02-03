;;;;
;;;; All of the type definition used in cl-plplot-system.
;;;;
;;;; hazen 01/14
;;;;

(in-package #:cl-plplot-system)

;(defctype plbool :boolean "PLplot boolean type")
;(defctype plchar :char "PLplot character type")
;(defctype plflt :double "PLplot floating point type")
;(defctype plint :int "PLplot fixed point type")
;(defctype plunicode :uint32 "PLplot unicode character type")


;;; helper functions and macros.

(defun pl-defctype-fn-name (pl-type name)
  (read-from-string (concatenate 'string (string pl-type) "-" name)))

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
			   :to-c ,to-c-fn-name)))))


;;; types

;; boolean type
(pl-defctype plbool :int
	     :from-c (not (zerop x))
	     :to-c (if x 1 0))

;; char type
(pl-defctype plchar :char
	     :from-c (char-code x)
	     :to-c (code-char x))

;; floating point type
(pl-defctype plflt :double
	     :from-c (coerce x 'double-float)
	     :to-c (coerce x 'double-float))

;; integer type
(pl-defctype plint :int
	     :from-c (round x)
	     :to-c (round x))

;; string type
(defctype plstr :string)

;; unicode-type
(pl-defctype plunicode :uint32
	     :from-c (round x)
	     :to-c (round x))


;;; Array types

;; base array type
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
     2. Integer - A empty foreign array is created with a size of integer."
  (if array-or-integer
      (cond
	((arrayp array-or-integer)
	 (let ((len (length array-or-integer))
	       (c-type (c-type instance)))
	   (setf (size instance) len)
	   (setf (c-pointer instance) (foreign-alloc c-type :count len))
	   (let ((c-pointer (c-pointer instance)))
	     (dotimes (i len)
	       (setf (mem-aref c-pointer c-type i) 
		     (convert-to-foreign (aref array-or-integer i) c-type))))))
	((integerp array-or-integer)
	 (setf (size instance) array-or-integer)
	 (setf (c-pointer instance) (foreign-alloc (c-type instance) :count array-or-integer)))
	(t
	 (format t "Invalid type for pl-to-foreign ~a~%" (type-of array-or-integer))))
      (progn
	(setf (size instance) 0)
	(setf (c-pointer instance) (null-pointer))))
  instance)


;; floating point array type
(defctype *plflt :pointer)

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

(defclass pl-pointer-integer (pl-pointer)
  ((c-type
    :initform 'plint
    :reader c-type)
   (lisp-type
    :initform 'fixnum
    :reader lisp-type)))

(defun make-*plint (array-or-integer)
  (pl-to-foreign array-or-integer (make-instance 'pl-pointer-integer)))


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
