;;;;
;;;; All of the type definition used in cl-plplot-system.
;;;;
;;;; hazen 01/14
;;;;

(in-package #:cl-plplot-system)


;;; Simple types

(defctype plbool :boolean "PLplot boolean type")
(defctype plchar :char "PLplot character type")
(defctype plflt :double "PLplot floating point type")
(defctype plint :int "PLplot fixed point type")
(defctype plunicode :uint32 "PLplot unicode character type")


;;; Array types

;; base array type
(define-foreign-type pl-pointer ()
  ((size
    :initform 0
    :accessor size))
  (:actual-type :pointer))

(defmethod free-translated-object (c-array (instance pl-pointer) param)
  (declare (ignore param))
  (foreign-free c-array))

(defmethod translate-from-foreign (c-array (instance pl-pointer))
  (let ((lisp-array (make-array (size instance) :element-type (lisp-type instance))))
    (dotimes (i (size instance))
      (setf (aref lisp-array i) 
	    (funcall (conversion-function instance) (mem-aref c-array (c-type instance) i))))
    lisp-array))

(defmethod translate-to-foreign (lisp-array (instance pl-pointer))
  (let* ((len (length lisp-array))
	 (c-array (foreign-alloc (c-type instance) :count len)))
    (setf (size instance) len)
    (dotimes (i len)
      (setf (mem-aref c-array (c-type instance) i) 
	    (funcall (conversion-function instance) (aref lisp-array i))))
    c-array))

;; integer array type
(define-foreign-type pl-integer-pointer (pl-pointer)
  ((c-type
    :initform :int
    :reader c-type)
   (conversion-function 
    :initform #'(lambda (x) (round x))
    :reader conversion-function)
   (lisp-type
    :initform 'fixnum
    :reader lisp-type))
  (:actual-type :pointer))

(define-parse-method *plint ()
  (make-instance 'pl-integer-pointer))


;; floating point array type
(define-foreign-type pl-float-pointer (pl-pointer)
  ((c-type
    :initform :double
    :reader c-type)
   (conversion-function 
    :initform #'(lambda (x) (coerce x 'double-float))
    :reader conversion-function)
   (lisp-type
    :initform 'double-float
    :reader lisp-type))
  (:actual-type :pointer))

(define-parse-method *plflt ()
  (make-instance 'pl-float-pointer))


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
