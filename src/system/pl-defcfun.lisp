;;;;
;;;;
;;;; Functions & macros to facilitate wrapping of calls to the plplot API,
;;;; 
;;;; The goal of all this macrology is to have :
;;;;
;;;; (pl-defcfun ("foo" foo) :int 
;;;;             (x plint)
;;;;             (y *plint n) 
;;;;             (z *plflt n) 
;;;;             (n plint))
;;;;
;;;; Expand into (progn statements removed) :
;;;; 
;;;; (DEFCFUN ("foo" C-FOO) :INT                   ;; "raw" cffi function
;;;;    (X PLINT) 
;;;;    (Y *PLINT) 
;;;;    (Z *PLFLT) 
;;;;    (N PLINT))
;;;;
;;;; (EXPORT 'C-FOO (PACKAGE-NAME *PACKAGE*))))
;;;;
;;;; (DEFUN FOO (X Y Z)                            ;; "more friendly" lisp function
;;;;    (LET ((C-Y (MAKE-PTR Y :INT #'(LAMBDA (X) (ROUND X))))
;;;;          (C-Z (MAKE-PTR Z :DOUBLE #'(LAMBDA (X) (COERCE X 'DOUBLE-FLOAT))))
;;;;          (N (LENGTH Z)))
;;;;      (UNWIND-PROTECT
;;;;          (C-FOO (FUNCALL #'(LAMBDA (X) (ROUND X)) X)
;;;;                 C-Y
;;;;                 C-Z
;;;;                 (FUNCALL #'(LAMBDA (X) (ROUND X)) N))
;;;;        (PROGN 
;;;;           (FOREIGN-FREE C-Y) 
;;;;           (FOREIGN-FREE C-Z)))))
;;;; 
;;;; (EXPORT 'FOO (PACKAGE-NAME *PACKAGE*))))
;;;;
;;;;
;;;; More complicated function definitions with different type are also supported.
;;;;
;;;; hazen 01/14
;;;;

(in-package #:cl-plplot-system)

(defun make-defcfun-args (args)
  "Returns the args list in a format suitable for calling the function created by defcfun."
  (let ((defcfun-args nil))
    (dolist (arg args)
      (push (list (car arg) (cadr arg)) defcfun-args))
    (nreverse defcfun-args)))

(defun make-wrapper-args (args)
  "Returns the args list as they will appear in the wrapper function."
  (let ((wrapper-args nil))
    (dolist (arg args)
      (when (= (length arg) 2)
	(push (car arg) wrapper-args)))
    (nreverse wrapper-args)))

(defun make-wrapper-check (args)
  "Parse args list to determine what checks need to be made on array lengths."
  (let ((wrapper-check (list 'and)))
    (dolist (arg args)
      (when (= (length arg) 3)
	(let ((elt-2 (elt arg 2)))
	  (when (listp elt-2)
	    (let ((ck-list (list '=)))
	      (dotimes (i (1- (length elt-2)))
		(push `,(substitute (elt elt-2 (1+ i)) 'x (car elt-2)) ck-list))
	      (push (nreverse ck-list) wrapper-check))))))
    (nreverse wrapper-check)))

(defun make-wrapper-return (args)
  "Parse args list to make local variables that will be returned."
  (let ((wrapper-return nil))
    (dolist (arg args)
      (when (and (= (length arg) 3)
		 (numberp (elt arg 2)))
	(push (car arg) wrapper-return)))
    (nreverse wrapper-return)))

(defun make-wrapper-vars (args)
  "Parse args list to make local variables needed in the wrapper function."
  (let ((wrapper-vars nil))
    (dolist (arg args)
      (when (= (length arg) 3)
	(let ((elt-2 (elt arg 2)))
	  (cond
	    ((numberp elt-2)
	     (push (list (car arg) `(make-array ,elt-2)) wrapper-vars))
	    ((listp elt-2)
	     (push (list (car arg) `,(substitute (elt elt-2 1) 'x (car elt-2))) wrapper-vars))))))
    (nreverse wrapper-vars)))

(defmacro pl-defcfun (name returns &rest args)
  "Function creation macro, wraps defcfun to handle most styles of function call in the plplot library"
  (let ((c-name (car name))
	(lisp-name (cadr name)))
    (export lisp-name)
    (if (should-wrap? args)
	(let ((wrapped-name (list (car name)
				  (read-from-string (concatenate 'string "c-" (string lisp-name))))))
	  `(defun ,lisp-name ,(make-wrapper-args args)
	     (let ,(make-wrapper-vars args)
	       (if ,(make-wrapper-check args)
		   (progn
		     (defcfun ,wrapped-name ,returns ,@(make-defcfun-args args))
		     (values ,@(make-wrapper-return args)))
		   (format t "Input array sizes do not match!~%")))))
	`(defcfun (,c-name ,lisp-name) ,returns ,@args))))

(defun should-wrap? (args)
  "Based on the argument list, determines if the function is complicated enough to need a wrapper."
  (format t "sw~%")
  (dolist (arg args)
    (when (> (length arg) 2)
      (format t "swt~%")
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
