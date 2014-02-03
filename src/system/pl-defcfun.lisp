;;;;
;;;;
;;;; Functions & macros to facilitate wrapping of calls to the plplot API,
;;;; 
;;;; The goal of all this macrology is to have :
;;;;
;;;; (pl-defcfun ("foo" foo) :void
;;;; 	         "The PLplot foo function."
;;;;	         (x plint)
;;;;	         (y *plint)
;;;;	         (z *plflt)
;;;;	         (n plint ((length x) y z)))
;;;;
;;;; Expand into (progn statements removed) :
;;;;
;;;; (DEFCFUN ("foo" C-FOO) :VOID
;;;;   (X PLINT)
;;;;   (Y *PLINT)
;;;;   (Z *PLFLT)
;;;;   (N PLINT))
;;;;
;;;; (DEFUN FOO (X Y Z)
;;;;   "The PLplot foo function."
;;;;   (LET ((N (LENGTH Y)))
;;;;     (IF (AND (= (LENGTH Y) (LENGTH Z)))
;;;;         (LET ((RETURN-VALUE (C-FOO X Y Z N)))
;;;;           (DECLARE (IGNORE RETURN-VALUE))
;;;;           (VALUES))
;;;;         (FORMAT T "Input array sizes do not match!~%"))))
;;;;
;;;; (EXPORT 'FOO))
;;;; (pl-defcfun ("foo" foo) :int 
;;;;             (x plint)
;;;;             (y *plint) 
;;;;             (z *plflt) 
;;;;             (n plint 
;;;;
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

(defmethod wrapper-arg ((instance pointer-arg))
  (when (eql (in/out instance) :in)
    (call-next-method instance)))

(defmethod wrapper-free ((instance pointer-arg))
  `(pl-foreign-free ,(c-var-name (name instance))))

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

(defmethod wrapper-arg ((instance size-arg)))

(defmethod wrapper-check ((instance size-arg))
  (let ((ck-list (list '=))
	(elt-2 (elt (form instance) 2)))
    (dotimes (i (1- (length elt-2)))
      (push `,(substitute (elt elt-2 (1+ i)) 'x (car elt-2)) ck-list))
    (nreverse ck-list)))


;;; Argument and argument list processing functions.

(defun make-arg (form)
  "Given an argument form this returns an instance of the class that represents this argument."
  (let ((arg-name (elt form 0))
	(arg-type (elt form 1))
	(len (length form)))
    (cond
      ((or (eql arg-type '*plint) (eql arg-type '*plflt))
       (make-instance 'pointer-arg 
		      :form form
		      :name arg-name
		      :in/out (if (= len 3) :out :in)))
      ((and (eql arg-type 'plint) (= len 3))
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

;(defun make-defcfun-args (arg-list)
;  "Returns the args list in a form suitable for use with defcfun."
;  (let ((defcfun-args nil))
;    (dolist (arg arg-list)
;      (push (defcfun-arg arg) defcfun-args))
;    defcfun-args))

;(defun make-defcfun-call-args (arg-list)
;  "Returns the args list in a form suitable for calling the function created by defcfun."
;  (let ((defcfun-call-args nil))
;    (dolist (arg arg-list)
;      (push (defcfun-call-arg arg) defcfun-call-args))
;    defcfun-call-args))

;(defun make-wrapper-args (args)
;  "Returns the args list as they will appear in the wrapper function."
;  (let ((wrapper-args nil))
;    (dolist (arg args)
;      (when (= (length arg) 2)
;	(push (car arg) wrapper-args)))
;    (nreverse wrapper-args)))

;(defun make-wrapper-check (args)
;  "Parse args list to determine what checks need to be made on array lengths."
;  (let ((wrapper-check (list 'and)))
;    (dolist (arg args)
;      (when (= (length arg) 3)
;	(let ((elt-2 (elt arg 2)))
;	  (when (listp elt-2)
;	    (let ((ck-list (list '=)))
;	      (dotimes (i (1- (length elt-2)))
;		(push `,(substitute (elt elt-2 (1+ i)) 'x (car elt-2)) ck-list))
;	      (push (nreverse ck-list) wrapper-check))))))
;    (nreverse wrapper-check)))

;(defun make-wrapper-free (args)
;  "Parse args list to free foreign storage of local variables."
;  (let ((wrapper-free nil))
;    (dolist (arg args)
;      t)))

;(defun make-wrapper-return (args)
;  "Parse args list to make local variables that will be returned."
;  (let ((wrapper-return nil))
;    (dolist (arg args)
;      (when (and (= (length arg) 3)
;		 (numberp (elt arg 2)))
;	(push (car arg) wrapper-return)))
;    (nreverse wrapper-return)))

;(defun make-wrapper-vars (args)
;  "Parse args list to make local variables needed in the wrapper function."
;  (let ((wrapper-vars nil))
;    (dolist (arg args)
;      (when (= (length arg) 3)
;	(let ((elt-2 (elt arg 2)))
;	  (cond
;	    ((numberp elt-2)
;	     (push (list (car arg) `(make-array ,elt-2)) wrapper-vars))
;	    ((listp elt-2)
;	     (push (list (car arg) `,(substitute (elt elt-2 1) 'x (car elt-2))) wrapper-vars))))))
;    (nreverse wrapper-vars)))

(defmacro pl-defcfun (name returns doc-string &body args)
  "Function creation macro, wraps defcfun to handle most styles of function call in the plplot library"
  (let* ((c-name (car name))
	 (lisp-name (cadr name))
	 (wrapped-name (read-from-string (concatenate 'string "c-" (string lisp-name)))))
    (if (should-wrap? args)
	;
	; Create a wrapped version of the function to make calling it from Lisp more lispy.
	;
	`(progn
	   (defcfun (,c-name ,wrapped-name) ,returns 
	     ,@(make-defcfun-args args))
	   (defun ,lisp-name ,(make-wrapper-args args)
	     ,doc-string
	     (let ,(make-wrapper-vars args)
	       (if ,(make-wrapper-check args)
		   (let ((return-value (,wrapped-name ,@(make-defcfun-call-args args))))
		     (declare (ignore return-value))
		     (progn
		       ,@(free-wrapper-vars args))
		     (values ,@(make-wrapper-return args)))
		   (format t "Input array sizes do not match!~%"))))
	   (export (quote ,lisp-name)))
	;
	; This also wraps the function, but only so that we can include a doc-string.
	;
	`(progn
	   (defcfun (,c-name ,wrapped-name) ,returns ,@args)
	   (defun ,lisp-name ,(make-wrapper-args args)
	     ,doc-string
	     (,wrapped-name ,@(make-defcfun-call-args args)))
	   (export (quote ,lisp-name))))))

(defun should-wrap? (args)
  "Based on the argument list, determines if the function is complicated enough to need a special wrapper."
  (dolist (arg args)
    (when (> (length arg) 2)
      (return t))))

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
