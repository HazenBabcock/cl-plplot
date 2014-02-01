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

(defun parse-args (args)
  "Parses an argument list as part of the process of figuring out how to wrap a function."
  (let ((length-args (make-hash-table :test 'equal)))
    (dolist (arg args)
      (when (> (length arg) 2)
	(if (listp (elt arg 2))
	    (dolist (var (elt arg 2))
	      (setf (gethash var length-args) (car arg))
  (let ((wrapper-fn-args (list))
	(defcfun-call-args (list))
	(defcfun-args (list)))
    (dolist (arg args)
      (

(defmacro pl-defcfun (name returns &rest args)
  "Function creation macro, wraps defcfun to handle most styles of function call in the plplot library"
  (let ((c-name (car name))
	(lisp-name (cadr name)))
    (export lisp-name)
    (if (should-wrap args)
	(let ((wrapped-name (list name
				  (read-from-string (concatenate 'string "c-" (string lisp-name))))))
	  (export wrapped-name)
	  `(defcfun ,wrapped-name ,returns ,@args))
	`(defcfun (,c-name ,lisp-name) ,returns ,@args))))

(defun should-wrap (args)
  "Based on the argument list, determines if the function is complicated enough to need a wrapper."
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
