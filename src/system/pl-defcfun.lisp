;;;;
;;;;
;;;; Functions & macros to facilitate wrapping of calls to the plplot API,
;;;; 
;;;; The goal of all this macrology is to have :
;;;;
;;;; (pl-defcfun ("c_plsym" plsym) :void 
;;;;     "Plot a glyph at the specified points."
;;;;   (n plint (length x) (= (length x) (length y)))
;;;;   (x *plflt)
;;;;   (y *plflt)
;;;;   (code plint))
;;;;
;;;; Expand into (progn statements removed):
;;;;
;;;; (DEFCFUN ("c_plsym" C-PLSYM)
;;;;     :VOID
;;;;   (N PLINT)
;;;;   (X *PLFLT)
;;;;   (Y *PLFLT)
;;;;   (CODE PLINT))
;;;;
;;;; (DEFUN PLSYM (X Y CODE)
;;;;   "Plot a glyph at the specified points."
;;;;   (LET ((N (LENGTH X)) (#:G946 (MAKE-*PLFLT X)) (#:G948 (MAKE-*PLFLT Y)))
;;;;     (IF (AND (= (LENGTH X) (LENGTH Y)))
;;;;         (UNWIND-PROTECT
;;;;             (LET ((RETURN-VALUE
;;;;                    (C-PLSYM N (C-POINTER #:G946) (C-POINTER #:G948) CODE)))
;;;;               (DECLARE (IGNORE RETURN-VALUE))
;;;;               (LET ()
;;;;                 (VALUES)))
;;;;           (PROGN (PL-FOREIGN-FREE #:G946) (PL-FOREIGN-FREE #:G948)))
;;;;         (FORMAT T "Input array sizes do not match in ~a!~%"
;;;;                 (STRING 'PLSYM)))))
;;;; (EXPORT 'PLSYM))
;;;;
;;;; More complicated function definitions with different type are also supported.
;;;;
;;;; hazen 01/14
;;;;

(in-package #:cl-plplot-system)


;;; The function argument classes.
;(defun c-var-name (var-name)
;  (read-from-string (concatenate 'string "c-" (string var-name))))

 
; Base class of an argument.
(defclass arg ()
  ((c-name
    :initform (gensym)
    :reader c-name)
   (form
    :initarg :form
    :reader form)
   (name
    :initarg :name
    :reader name)))

(defgeneric defcfun-arg (instance))
(defmethod defcfun-arg ((instance arg))
  `(,(elt (form instance) 0) ,(elt (form instance) 1)))

(defgeneric defcfun-call-arg (instance))
(defmethod defcfun-call-arg ((instance arg))
  `,(car (form instance)))

(defgeneric wrapper-arg (instance))
(defmethod wrapper-arg ((instance arg))
  `,(car (form instance)))

(defgeneric wrapper-check (instance))
(defmethod wrapper-check ((instance arg)))

(defgeneric wrapper-free (instance))
(defmethod wrapper-free ((instance arg)))

(defgeneric wrapper-from-foreign (instance))
(defmethod wrapper-from-foreign ((instance arg)))

(defgeneric wrapper-return (instance))
(defmethod wrapper-return ((instance arg)))

(defgeneric wrapper-vars (instance))
(defmethod wrapper-vars ((instance arg)))


; Pointer (array) argument.
(defclass pointer-arg (arg)
  ((in/out
    :initarg :in/out
    :reader in/out)
   (r-name
    :initform (gensym)
    :reader r-name)))

(defmethod defcfun-call-arg ((instance pointer-arg))
  `(c-pointer ,(c-name instance)))

(defmethod wrapper-arg ((instance pointer-arg))
  (when (eql (in/out instance) :in)
    (call-next-method instance)))

(defmethod wrapper-free ((instance pointer-arg))
  `(pl-foreign-free ,(c-name instance)))

(defmethod wrapper-from-foreign ((instance pointer-arg))
  (when (eql (in/out instance) :out)
    `(,(r-name instance) (pl-from-foreign ,(c-name instance)))))

(defmethod wrapper-return ((instance pointer-arg))
  (when (eql (in/out instance) :out)
    `,(r-name instance)))

(defmethod wrapper-vars ((instance pointer-arg))
  (let ((make-fn-name (read-from-string (concatenate 'string "make-" (string (elt (form instance) 1))))))
    `(,(c-name instance) (,make-fn-name ,(if (eql (in/out instance) :in)
					     (name instance)
					     (elt (form instance) 2))))))
       

; Size of pointer / array argument.
(defclass size-arg (arg) ())

(defmethod wrapper-arg ((instance size-arg))
  (when (not (elt (form instance) 2))
    (call-next-method instance)))

(defmethod wrapper-check ((instance size-arg))
  `,(elt (form instance) 3))

(defmethod wrapper-vars ((instance size-arg))
  (when (elt (form instance) 2)
  `(,(name instance) ,(elt (form instance) 2))))


;;; Argument and argument list processing functions.

(defun make-arg (form)
  "Given an argument form this returns an instance of the class that represents this argument."
  (let ((arg-name (elt form 0))
	(arg-type (elt form 1))
	(len (length form)))
    (cond
      ((member arg-type array-types)
       (make-instance 'pointer-arg 
		      :form form
		      :name arg-name
		      :in/out (if (= len 3) :out :in)))
      ((and (eql arg-type 'plint) (= len 4))
       (make-instance 'size-arg
		      :form form
		      :name arg-name))
      (t
       (make-instance 'arg
		      :form form
		      :name arg-name)))))
       
(defun make-args (forms)
  "Given an argument list, returns a list of argument instances."
  (let ((arg-list nil))
    (dolist (form forms)
      (push (make-arg form) arg-list))
    arg-list))

(defun make-forms (arg-list accessor)
  "Returns the appropriate form depending on the accessor function."
  (let ((defcfun-args nil))
    (dolist (arg arg-list)
      (when (funcall accessor arg)
	(push (funcall accessor arg) defcfun-args)))
    defcfun-args))


;;; The pl-defcfun macro.

(defun should-wrap? (args)
  "Based on the argument list, determines if the function is complicated enough to need a special wrapper."
  (dolist (arg args)
    (when (> (length arg) 2)
      (return t))))

(defmacro pl-defcfun (name returns doc-string &body args)
  "Function creation macro, wraps defcfun to handle most styles of function call in the plplot library"
  (let* ((c-name (car name))
	 (lisp-name (cadr name))
	 (wrapped-name (read-from-string (concatenate 'string "c-" (string lisp-name)))))
    (if (should-wrap? args)
	;
	; Create a wrapped version of the function to make calling it from Lisp more lispy.
	;
	(let ((arg-list (make-args args)))
	  `(progn
	     (defcfun (,c-name ,wrapped-name) ,returns 
	       ,@(make-forms arg-list #'defcfun-arg))
	     (defun ,lisp-name ,(make-forms arg-list #'wrapper-arg)
	       ,doc-string
	       (let ,(make-forms arg-list #'wrapper-vars)
		 (if (and ,@(make-forms arg-list #'wrapper-check))
		     (unwind-protect
			  (let ((return-value (,wrapped-name ,@(make-forms arg-list #'defcfun-call-arg))))
			    (declare (ignore return-value))
			    (let ,(make-forms arg-list #'wrapper-from-foreign)
			      (values ,@(make-forms arg-list #'wrapper-return))))
		       (progn
			 ,@(make-forms arg-list #'wrapper-free)))
		     (format t "Input array sizes do not match in ~a!~%" (string (quote ,lisp-name))))))
	     (export (quote ,lisp-name))))
	;
	; This also wraps the function, but only so that we can include a doc-string.
	;
	(let ((arg-list (mapcar #'(lambda(x) (car x)) args)))
	  `(progn
	     (defcfun (,c-name ,wrapped-name) ,returns ,@args)
	     (defun ,lisp-name ,arg-list
	       ,doc-string
	       (,wrapped-name ,@arg-list))
	     (export (quote ,lisp-name)))))))


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
