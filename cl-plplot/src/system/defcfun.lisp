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
;;;; hazen 3/06
;;;;

(in-package #:cl-plplot-system)


;;
;; Setup the form containing the list of types and their initializers & destroyers
;;
;; *type-forms* is an associated list containing functions that generate the forms
;;   necessary to create, coerce, pass, return & free a particular type.
;;
;; A *type-forms* entry for a simple type (i.e. single element & only passed in) contains:
;;   (type-name conversion-function)
;;
;; A *type-forms* entry for a "complex" type (everything else) contains:
;;   (type-name arg-function creation-function passing-function returning-function freeing-function)
;;
;; These functions will be passed the form associated with a particular variable.
;; For example, if we have the form for the variable x as (x *plint n), then
;; creation-function will get '(x *plint n) as its argument. Creation-function 
;; will use this form to generate a form to create the variable, or return nil
;; if no creation form is necessary. For the above example it might return:
;; ((c-x (make-ptr x :int #'(lambda (x) (round x))))
;;  (n (length x)))
;;
;; Expected behaviors:
;;  arg-function : Returns a form containing variables that do *NOT* need to be included
;;     in the lisp function call. This might be because this is an output variable or because
;;     this variable is only used to pass in the size of another variable. Can be nil.
;;
;;  create-function : Returns a form that creates the "c" variable that will be passed to the
;;     c function created by cffi's defcfun macro. Can be nil. May optionally initialize other
;;     variables such as those that are used to pass in the variable sizes, etc...
;;
;;  passing-function : Returns a form or symbol to be used in the call to the c function.
;;
;;  returning-function : Returns a form that creates a lisp object from the "c" variable that 
;;     will be returned as a result of the function call. Can be nil.
;;
;;  freeing-function : Returns a form that will take care of cleaning up the "c" variable
;;     when the function call exits. Can be nil.
;;

(defvar *type-forms* nil)

(defun name-cat (name1 name2 &rest more-names)
  "returns the concatenation of a and b as a symbol"
  (let ((final (concatenate 'string (string name1) (string name2))))
    (dolist (name more-names)
      (setf final (concatenate 'string final (string name))))
    (read-from-string final)))

;; FIXME: 
;;  Paraphrasing Norvig, "If you are using eval you are probably doing something wrong".
;;  How do you do this without using eval???

(defun add-simple-type (type-name cffi-type conv-fn)
  "Adds a simple type to *type-forms*, i.e. a type that we do little to no handling of"
  (eval `(defctype ,type-name ,cffi-type))
  (push (list type-name conv-fn) *type-forms*))

(defun add-type (type-name cffi-type args-fn create-fn pass-fn return-fn free-fn)
  "Adds type to *type-forms* list of types"
  (eval `(defctype ,type-name ,cffi-type))
  (push (list type-name args-fn create-fn pass-fn return-fn free-fn) *type-forms*))

(defun c-name (args)
  "Returns a symbol that is used as a temporary variable name for passing
   a lisp argument to a c function"
  (name-cat "c-" (car args)))
;    (read-from-string (concatenate 'string "c-" (string (car args)))))

;; Functions for adding functions for standard types to *type-forms*

(defconstant p-in 1)
(defconstant p-in-out 0)
(defconstant p-out -1)

(defun in-out? (args)
  "Returns whether this argument is passed in, passed out or both"
  (cond
    ((and (> (length args) 2)
	  (numberp (elt args 2)))
     p-out)
    ((and (> (length args) 2)
	  (string= "IN-OUT" (string (elt args 2))))
     p-in-out)
    (t p-in)))

(defun ptr-name-dir (args)
  "Returns pointer name & direction"
  (values (c-name args)
	  (in-out? args)))

(defun std-args (args)
  "Standard arg-function"
  (let ((tmp (in-out? args)))
    (cond
      ((= tmp p-in) `,(elt args 2))
      ((= tmp p-out) `,(elt args 0)))))

(defun std-creating (cffi-type convf-in)
  "Returns standard creating-function"
  (labels ((create-fn (args)
	     (multiple-value-bind (name dir) (ptr-name-dir args)
	       (cond
		 ((= dir p-in)
		  `((,(elt args 2) (if (eq ,(elt args 0) 'null)
				       0
				       (length ,(elt args 0))))
		    (,name (if (eq ,(elt args 0) 'null)
			       (null-pointer)
			       (make-ptr ,(elt args 0) ,cffi-type ,convf-in)))))

;		  `((,(elt args 2) (length ,(elt args 0)))
;		    (,name (make-ptr ,(elt args 0) ,cffi-type ,convf-in))))

		 ((= dir p-in-out)
		  `(,name (make-ptr (vector ,(elt args 0)) ,cffi-type ,convf-in)))
		 ((= dir p-out)
		  `(,name (foreign-alloc ,cffi-type :count ,(elt args 2))))))))
    #'create-fn))

(defun std-passing (args)
  "Standard passing-function"
  `,(c-name args))

(defun std-returning (lisp-type cffi-type convf-out)
  "Returns standard creating-function"
  (labels ((return-fn (args)
	     (multiple-value-bind (name dir) (ptr-name-dir args)
	       (cond
		 ((= dir p-in-out)
		  `(funcall ,convf-out (mem-aref ,name ,cffi-type 0)))
		 ((= dir p-out)
		  (if (= 1 (elt args 2))
		      `(funcall ,convf-out (mem-aref ,name ,cffi-type 0))
		      `(get-ptr ,name ,lisp-type ,cffi-type ,convf-out ,(elt args 2))))))))
    #'return-fn))

(defun std-freeing (args)
  "Standard freeing-function"
;  `(foreign-free ,(c-name args)))
  (multiple-value-bind (name dir) (ptr-name-dir args)
    (declare (ignore name))
    (cond
      ((= dir p-in)
       `(unless (eq ,(elt args 0) 'null)
	  (foreign-free ,(c-name args))))
      (t
       `(foreign-free ,(c-name args))))))

(defun ptr-name (x)
  "Returns name of pointer to type x (used for 'standard' types)"
  (name-cat "*" x))
;    (read-from-string (concatenate 'string "*" (string x))))

(defun get-ptr (c-arr lisp-type cffi-type conv-func length)
  "Copies a C array into a newly created lisp vector"
  (let ((l-vec (make-array length :element-type lisp-type)))
    (dotimes (i length)
      (setf (aref l-vec i) (funcall conv-func (mem-aref c-arr cffi-type i))))
    l-vec))

(defun make-ptr (arr cffi-type conv-func)
  "Copies a lisp vector into a newly created C array"
  (let* ((n (length arr))
	 (c-arr (foreign-alloc cffi-type :count n)))
    (dotimes (i n)
      (setf (mem-aref c-arr cffi-type i) (funcall conv-func (aref arr i))))
    c-arr))

(defun make-matrix (lisp-mat)
  "Creates a two-dimensional c array, initializes with lisp matrix"
  (let* ((x-dim (array-dimension lisp-mat 0))
	 (y-dim (array-dimension lisp-mat 1))
	 (c-mat (foreign-alloc :pointer :count x-dim)))
    (dotimes (x x-dim)
      (let ((cur (foreign-alloc :double :count y-dim)))
	(setf (mem-aref c-mat :pointer x) cur)
	(dotimes (y y-dim)
	  (setf (mem-aref cur :double y) (coerce (aref lisp-mat x y) 'double-float)))))
    c-mat))

(export 'make-matrix (package-name *package*))

(defun free-matrix (c-mat dims)
  "Frees a two-dimensional c array"
  (dotimes (x (car dims))
    (foreign-free (mem-aref c-mat :pointer x)))
  (foreign-free c-mat))

(export 'free-matrix (package-name *package*))

(defun add-std-type (type-name cffi-type lisp-type convf-in convf-out &optional (want-arrays t))
  "Creates items in the *type-forms* list for 'standard' types, i.e. those things like 
   integers, floats, etc.. which are all created & returned & destroyed in the same way.
   If want-arrays is true, the a type of name *type-name will also be created. This type
   can be used for passing in/out arrays of the values of this type"
  (add-simple-type type-name 
		   cffi-type
		   #'(lambda (args)
		       `(funcall ,convf-in ,(elt args 0))))
  (when want-arrays
    (add-type (ptr-name type-name) 
	      :pointer 
	      #'std-args 
	      (std-creating cffi-type convf-in) 
	      #'std-passing
	      (std-returning lisp-type cffi-type convf-out)
	      #'std-freeing)))

;; Initialization of *type-forms* (alphabetical order by type name)

(setf *type-forms* nil)

(add-std-type 'plbool :int 'fixnum 
	      '#'(lambda(x) (if x 1 0)) 
	      '#'(lambda (x) (if (= x 0) nil t)))

(add-std-type 'plchar :char 'character 
	      '#'(lambda(x) (char-code x)) 
	      '#'(lambda(x) (code-char x)))

(add-type 'plfunc :pointer
	  #'(lambda (args)
	      `,(elt args 0))
	  #'(lambda (args) (declare (ignore args)) nil)
	  #'(lambda (args)
	      `(callback ,(elt args 0)))
	  #'(lambda (args) (declare (ignore args)) nil)
	  #'(lambda (args) (declare (ignore args)) nil))

(add-std-type 'plflt :double 'double-float 
	      '#'(lambda(x) (coerce x 'double-float))
	      '#'(lambda(x) (coerce x 'double-float)))

(add-type '**plflt :pointer
	  #'(lambda (args)
	      `,(elt args 2))
	  #'(lambda (args)
	      `((,(car (elt args 2)) (array-dimension ,(elt args 0) 0))
		(,(cadr (elt args 2)) (array-dimension ,(elt args 0) 1))
		(,(c-name args) (make-matrix ,(elt args 0)))))
	  #'std-passing
	  #'(lambda (args)
	      (declare (ignore args))
	      nil)
	  #'(lambda (args)
	      `(free-matrix ,(c-name args) (list ,(car (elt args 2)) ,(cadr (elt args 2))))))

(add-std-type 'plint :int 'fixnum 
	      '#'(lambda(x) (round x))
	      '#'(lambda(x) (round x)))

(add-simple-type 'plpointer :pointer
		 #'(lambda(x)
		     `,(elt x 0)))

(add-type 'plstr :pointer
	  #'(lambda (args)
	      (when (= p-out (in-out? args))
		`,(elt args 0)))
	  #'(lambda (args)
	      (if (= p-in (in-out? args))
		  `(,(c-name args) (foreign-string-alloc ,(elt args 0)))
		  `(,(c-name args) (foreign-alloc :char :count ,(elt args 2)))))
	  #'std-passing
	  #'(lambda (args)
	      (when (= p-out (in-out? args))
		`(foreign-string-to-lisp ,(c-name args))))
	  #'(lambda (args)
	      `(foreign-string-free ,(c-name args))))

(add-std-type 'plunicode :uint32 'fixnum 
	      '#'(lambda (x) (round x))
	      '#'(lambda (x) (round x)))


;;
;; Create the form that specifies a function call
;;

;; Helper functions

(defun args-flatten (lst)
  "Flattens list lst"
  (let ((out nil))
    (labels ((fl (cur)
	       (when cur
		 (if (listp cur)
		     (progn
		       (fl (car cur))
		       (fl (cdr cur)))
		     (push cur out)))))
      (fl lst))
    out))

(defun setup-flatten (lst)
  "Flattens setup list lst, as necessary"
  (let ((out nil))
    (dolist (obj lst)
      (if (listp (car obj))
	  (dolist (s-obj obj)
	    (push s-obj out))
	  (push obj out)))
    (reverse out)))

;; Generation of different parts of the function call

(defun lisp-arguments (lst)
  "Creates the list of arguments that the lisp function will take"
  (let ((setup (args-flatten
		(remove nil
			(mapcar #'(lambda (x)
				    (let ((type-info (assoc (elt x 1) *type-forms*)))
				      (when (= (length type-info) 6)
					(funcall (elt type-info 1) x))))
				lst))))
	(passed (mapcar #'(lambda (x)
			    (car x))
			lst)))
    (dolist (elem setup)
      (when (member elem passed :test #'equal)
	(setf passed (remove elem passed))))
    passed))

(defun lisp-setup (lst)
  "Creates the code to setup on the lisp side for the c function call"
  (remove-duplicates 
   (setup-flatten
    (remove nil
	    (mapcar #'(lambda (x)
			(let ((type-info (assoc (elt x 1) *type-forms*)))
			    (when (= (length type-info) 6)
			      (funcall (elt type-info 2) x))))
		    lst)))
   :test #'(lambda (x y) 
	     (equal (car x) (car y)))))

(defun lisp-call (lst)
    "Creates the code on the lisp side to call the c function"
    (mapcar #'(lambda (x)
		(let ((type-info (assoc (elt x 1) *type-forms*)))
		  (if (= (length type-info) 6)
		      (funcall (elt type-info 3) x)
		      (funcall (elt type-info 1) x))))
	    lst))

(defun lisp-return (lst)
  "Creates the code to return variable values if necessary"
  (remove nil
	  (mapcar #'(lambda (x)
		      (let ((type-info (assoc (elt x 1) *type-forms*)))
			(when (= (length type-info) 6)
			  (funcall (elt type-info 4) x))))
		  lst)))

(defun lisp-cleanup (lst)
  "Creates the code necessary to clean up after the function call"
  (remove nil
	  (mapcar #'(lambda (x)
		      (let ((type-info (assoc (elt x 1) *type-forms*)))
			(when (= (length type-info) 6)
			  (funcall (elt type-info 5) x))))
		  lst)))

;; Sews together the forms generated above to create the final function call

(defun %pl-defcfun (cffi-name lisp-name fn-returns args)
  "Creates the lisp wrapper code for calling the lisp 
     function created by defcfun & called cffi-name"
  (let ((setup (lisp-setup args))
	(retrn (lisp-return args)))
    `(progn
       (defun ,lisp-name
	   ,(lisp-arguments args)
	 ,(if setup
	      `(let ,setup
		 (unwind-protect
		      ,(if retrn
			   (if (string= "VOID" (string fn-returns))
			       `(progn
				  (,cffi-name ,@(lisp-call args))
				  (values ,@retrn))
			       `(values (,cffi-name ,@(lisp-call args)) ,@retrn))
			   `(,cffi-name ,@(lisp-call args)))
		   (progn ,@(lisp-cleanup args))))
	      `(,cffi-name ,@(lisp-call args))))
       (export ',lisp-name (package-name *package*)))))

(defun cffi-defcfun (c-name lisp-name returns args)
  "Creates defcfun macro call. The defcfun macro is used to create 
     the call to the c library function as well as its lisp counterpart"
  `(progn
     (defcfun (,c-name ,lisp-name) ,returns
       ,@(mapcar #'(lambda (x)
		     `(,(elt x 0) ,(elt x 1)))
		 args))
     (export ',lisp-name (package-name *package*))))

(defmacro pl-defcfun (name returns &rest args)
  "Function creation macro, wraps defcfun to handle most
   styles of function call in the plplot library"
;  (let ((cffi-name (read-from-string (concatenate 'string "c-" (string (elt name 1))))))
  (let ((cffi-name (name-cat "c-" (elt name 1))))
    `(progn
       ,(cffi-defcfun (elt name 0) cffi-name returns args)
       ,(%pl-defcfun cffi-name (elt name 1) returns args))))
