;;;;
;;;; Miscellaneous functions & macros that are used in the cl-plot API
;;;;
;;;; hazen 02/14
;;;;

(in-package #:cl-plplot-system)


(defun pl-length (list-or-null)
  "Deals with legacy code where the user might have used 'null instead of nil."
  (if (symbolp list-or-null)
      0
      (length list-or-null)))

;      (progn
;	(format t " length is ~a~%" (length list-or-null))
;	(length list-or-null))
 ;     (progn
;	(format t " null~%")
;	0)))
  
;;;
;;; These are for dealing with passing in the plcgrid structure, which is then passed 
;;; to various coordinate transformation functions (i.e. pltr0, pltr1 & pltr2).
;;;

;(defcstruct plcgrid
;  (x plpointer)
;  (y plpointer)
;  (z plpointer)
;  (nx plint)
;  (ny plint)
;  (nz plint))

;(defun check-size (a mx my)
;  "checks that p-d has the right dimensions"
;  (and (or (= (array-dimension a 0) mx)
;	   (= (array-dimension a 0) (1+ mx)))
;       (or (= (array-dimension a 1) my)
;	   (= (array-dimension a 1) (1+ my)))))

;(defun init-plcgrid (pdx pdy mx my)
;  "initializes plcgrid given user supplied pdx & pdy, if necessary"
;  (cond 
;    ((equal (pl-get-pltr-fn) #'pltr0)
;     (values (null-pointer) 'empty-p))
;    ((equal (pl-get-pltr-fn) #'pltr1)
;     (if (and (vectorp pdx)
;	      (vectorp pdy)
;	      (= (length pdx) mx)
;	      (= (length pdy) my))
;	 (let ((tmp (foreign-alloc 'plcgrid)))
;	   (with-foreign-slots ((x y nx ny) tmp plcgrid)
;	     (setf x (make-ptr pdx 'plflt #'(lambda(x) (coerce x 'double-float))))
;	     (setf y (make-ptr pdy 'plflt #'(lambda(x) (coerce x 'double-float))))
;	     (setf nx (length pdx))
;	     (setf ny (length pdy)))
;	   (values tmp 'vector-p))
;	 (progn
;	   (format t "Array dimensions are wrong for pdx or pdy in init-plcgrid~%")
;	   (values nil nil))))
;    ((equal (pl-get-pltr-fn) #'pltr2)
;     (if (and (arrayp pdx)
;	      (arrayp pdy)
;	      (check-size pdx mx my)
;	      (check-size pdy mx my))
;	 (let ((tmp (foreign-alloc 'plcgrid)))
;	   (with-foreign-slots ((x y nx ny) tmp plcgrid)
;	     (setf x (make-matrix pdx))
;	     (setf y (make-matrix pdy))
;	     (setf nx (array-dimension pdx 0))
;	     (setf ny (array-dimension pdx 1)))
;	   (values tmp 'matrix-p))
;	 (progn
;	   (format t "Matrix dimensions are wrong for pdx or pdy in init-plcgrid~%")
;	   (values nil nil))))
;    ; this has fewer safeguards...
;    (t (cond
;	 ((and (vectorp pdx)
;	       (vectorp pdy))
;	  (let ((tmp (foreign-alloc 'plcgrid)))
;	    (with-foreign-slots ((x y nx ny) tmp plcgrid)
;	      (setf x (make-ptr pdx 'plflt #'(lambda(x) (coerce x 'double-float))))
;	      (setf y (make-ptr pdy 'plflt #'(lambda(x) (coerce x 'double-float))))
;	      (setf nx (length pdx))
;	      (setf ny (length pdy)))
;	    (values tmp 'vector-p)))
;	 ((and (arrayp pdx)
;	       (arrayp pdy))
;	  (let ((tmp (foreign-alloc 'plcgrid)))
;	    (with-foreign-slots ((x y nx ny) tmp plcgrid)
;	      (setf x (make-matrix pdx))
;	      (setf y (make-matrix pdy))
;	      (setf nx (array-dimension pdx 0))
;	      (setf ny (array-dimension pdy 0)))
;	    (values tmp 'matrix-p)))
;	 ((not pdx)
;	  (values (null-pointer) 'empty-p))
;	 (t
;	  (values pdx 'user-p))))))
;
;(defun free-plcgrid (p-grid type)
;  "frees the plcgrid structure, if necessary"
;  (cond
;    ((equal type 'vector-p)
;     (progn
;       (with-foreign-slots ((x y) p-grid plcgrid)
;	 (foreign-free x)
;	 (foreign-free y))
;       (foreign-free p-grid)))
;    ((equal type 'matrix-p)
;     (progn
;       (with-foreign-slots ((x y nx ny) p-grid plcgrid)
;	 (let ((dims (list nx ny)))
;	   (free-matrix x dims)
;	   (free-matrix y dims)))
;       (foreign-free p-grid)))
;    (t nil)))
;
;(defmacro with-plcgrid ((plc-grid pdx pdy mx my) &body body)
;  (let ((type (gensym)))
;    `(multiple-value-bind (,plc-grid ,type) (init-plcgrid ,pdx ,pdy ,mx ,my)
;       (when ,plc-grid
;	 ,@body
;	 (free-plcgrid ,plc-grid ,type)))))


;;;
;;; Some plplot functions require callbacks. These callbacks are inside closures so that user 
;;; can more easily provide their own callback functions. This macro is for making the closures.
;;;

;(defmacro callback-closure (fname default returns &rest variables)
;  "Encloses a callback function in a closure so that the 
;   user can substitute the function of their own choosing"
;  (let ((var-name (name-cat "my-" fname)))
;    `(let ((,var-name ,default))
;       (defcallback ,fname ,returns ,variables
;	 (funcall ,var-name ,@(mapcar #'(lambda(x) (car x)) variables)))
;       (defun ,(name-cat "pl-set-" fname) (new-fn)
;	 (setf ,var-name new-fn))
;       (defun ,(name-cat "pl-reset-" fname) ()
;	 (setf ,var-name ,default))
;       (defun ,(name-cat "pl-get-" fname) ()
;	 ,var-name)
;       (export ',(name-cat "pl-set-" fname) (package-name *package*))
;       (export ',(name-cat "pl-reset-" fname) (package-name *package*))
;       (export ',(name-cat "pl-get-" fname) (package-name *package*)))))


;;;
;;; Some plplot functions need a pointer even if they aren't going to do anything with it. This 
;;; function wraps CFFI's null-pointer so that user doesn't have to load CFFI just to pass a 
;;; null pointer
;;;

(defun pl-null-pointer ()
  (null-pointer))

(export 'pl-null-pointer)

;;;
;;; These are the structures for interfacing with the plf... functions
;;; in PLplot. There are also some macros to make things a little easier.
;;;

(defcstruct plfgrid2
  (f :pointer)
  (nx :int)
  (ny :int))

(defun lisp-data-to-foreign (lisp-data)
  (if (= (length (array-dimensions lisp-data)) 2)
      (make-**plflt lisp-data)
      (make-*plflt lisp-data)))

(defun create-grid (c-data size-x size-y)
  (let ((ptr (foreign-alloc '(:struct plfgrid2))))
    (with-foreign-slots ((f nx ny) ptr (:struct plfgrid2))
      (setf f c-data)
      (setf nx size-x)
      (setf ny size-y))
;    (setf (foreign-slot-value ptr '(:struct plfgrid2) 'f) c-data
;	  (foreign-slot-value ptr '(:struct plfgrid2) 'nx) size-x
;	  (foreign-slot-value ptr '(:struct plfgrid2) 'ny) size-y)
;  (let ((ptr (foreign-alloc 'plfgrid2)))
;    (setf (foreign-slot-value ptr 'plfgrid2 'f) c-data
;	  (foreign-slot-value ptr 'plfgrid2 'nx) size-x
;	  (foreign-slot-value ptr 'plfgrid2 'ny) size-y)
    ptr))
  
(export 'create-grid)

(defmacro with-foreign-grid ((lisp-data grid size-x size-y) &body body)
  (let ((c-data (gensym)))
    `(let* ((,c-data (lisp-data-to-foreign ,lisp-data))
	    (,grid (create-grid (c-pointer ,c-data) ,size-x ,size-y)))
       (unwind-protect
	    ,@body
	 (progn
	   (pl-foreign-free ,c-data)
	   (foreign-free ,grid))))))

(export 'with-foreign-grid)

(defmacro with-foreign-matrix ((lisp-matrix foreign-matrix) &body body)
  (let ((c-data (gensym)))
    `(let* ((,c-data (make-**plflt ,lisp-matrix))
	    (,foreign-matrix (c-pointer ,c-data)))
       (unwind-protect
	    ,@body
	 (pl-foreign-free ,c-data)))))

(export 'with-foreign-matrix)


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
