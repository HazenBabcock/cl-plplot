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
;;;; Examples that demonstrate some plplot graphs that can be made
;;;; with the cl-plplot-system package.
;;;;
;;;; hazen 3/06
;;;;


(defpackage :system-examples
  (:use :common-lisp
	:cl-plplot-system))

(in-package :system-examples)

(defparameter gdev "aqt")  ; set this to the appropriate plplot device for your system


;;; Helper functions

(defun my-make-array (dims)
  (make-array dims :initial-element 0.0 :element-type 'float))

(defun example-func-1 (x y)
  (- (* x x) (* y y) (* (sin (* 7 x)) (* (cos (* 7 y))))))

(defun example-func-2 (x y)
  (let ((z (+ (expt (1- x) 2) (* 100 (expt (- y (expt x 2)) 2)))))
    (if (> z 0)
	(log z)
	0.0)))
  
(defun example-matrix (sx sy fn)
  (let ((mat (my-make-array (list sx sy)))
	(dx (/ 2 sx))
	(dy (/ 2 sy)))
    (dotimes (x sx)
      (dotimes (y sy)
	(setf (aref mat x y) (funcall fn (1- (* dx x)) (1- (* dy y))))))
    mat))

(defun make-levels (levels min max)
  (let ((clevels (my-make-array levels)))
    (dotimes (i levels)
      (setf (aref clevels i) (+ min (/ (* (- max min) (+ 0.5 i)) levels))))
    clevels))

(defun codes-to-string (&rest codes)
  (let ((string (make-array (length codes) :element-type 'character)))
    (dotimes (i (length codes))
      (setf (char string i) (code-char (elt codes i))))
    string))


;;; Examples

;; A 2D plot

(defun plot ()
  (plsdev gdev)
  (plinit)
  (plcol0 1)
  (plwid 2)
  (plenv 0 6 0 36 0 0)
  (plcol0 2)
  (pllab "(x)" "(y)" "y = x#u2")
  (let ((xs (my-make-array 6))
	(ys (my-make-array 6))
	(x (my-make-array 60))
	(y (my-make-array 60)))
    (dotimes (i 6)
      (setf (aref xs i) i)
      (setf (aref ys i) (* i i)))
    (plcol0 4)
    (plpoin xs ys 9)
    (dotimes (i 60)
      (let ((tmp (* 0.1 i)))
	(setf (aref x i) tmp)
	(setf (aref y i) (* tmp tmp))))
    (plcol0 3)
    (plline x y))
  (plend))

;; Contour plot of data

(defun contour-plot ()
  (plsdev gdev)
  (plinit)
  (plenv 0 24 0 44 0 0)
  (plcont (example-matrix 35 45 #'example-func-1) 1 35 1 45 (make-levels 20 -1.0 1.0))
  (plcol0 1)
  (plbox "bcnst" 0 0 "bcnstv" 0 0)
  (plcol0 2)
  (pllab "x" "y" "Contour Plot (Data)")
  (plend))

;; Contour plot of a function

(defun fn-contour-plot ()
  (plsdev gdev)
  (plinit)
  (plenv 0 24 0 44 0 0)
  (pl-set-feval-fn #'(lambda (x y p)
		       (declare (ignore p))
		       (coerce (example-func-1 (1- (/ x 17)) (1- (/ y 22))) 'double-float)))
  (plfcont (pl-null-pointer) 35 45 1 35 1 45 (make-levels 20 -1.0 1.0))
  (pl-reset-feval-fn)
  (plcol0 1)
  (plbox "bcnst" 0 0 "bcnstv" 0 0)
  (plcol0 2)
  (pllab "x" "y" "Contour Plot (Function)")
  (plend))

;; Shade plot

(defun shade-plot ()
  (plsdev gdev)
  (plinit)
  (plenv -1 1 -1 1 0 0)
  (plshades (example-matrix 35 45 #'example-func-1) -1 1 -1 1 (make-levels 20 -1.0 1.0) 2 1 1 nil)
  (plcol0 1)
  (plbox "bcnst" 0 0 "bcnstv" 0 0)
  (plcol0 2)
  (pllab "x" "y" "Shade Plot")
  (plend))

;; 3D surface plot

(defun 3D-plot ()
  (plsdev gdev)
  (plinit)
  (pladv 0)
  (plvpor 0 1 0 0.9)
  (plwind -1 1 -0.9 1.1)
  (plscmap1n 256)
  (plscmap1l 1 (vector 0.0 1.0) (vector 0.2 1) (vector 0.2 1) (vector 0.2 1) (vector nil nil))
  (plw3d 1 1 1 -1.5 1.5 -0.5 1.5 -5 6.5 60 30)
  (plmtex "t" 1 0.5 0.5 "3D plot example")
  (plbox3 "bnstu" "x axis" 0 0 "bnstu" "y axis" 0 0 "bcdmnstuv" "z axis" 0 0)
  (plsurf3d (make-levels 40 -1.5 1.5) (make-levels 40 -1.5 1.5) (example-matrix 40 40 #'example-func-2) 0 (make-levels 2 -1 1))
  (plend))

;; Unicode labels, a nice feature of plplot (strings are utf8 encoded).
;; YMMV depending on the capabilities of the driver itself and of the 
;; fonts that are available to the driver.

(defun unicode ()
  (plsdev gdev)
  (plinit)
  (pladv 0)
  (plvpor 0 1 0 1)
  (plwind 0 1 0 1)
  (plschr 0 4)
  (plptex 0.5 0.5 1.0 0.0 0.5 (codes-to-string 72 97 115 195 174 116 195 174))
  (plend))
