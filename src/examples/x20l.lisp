;;;;
;;;; PLplot example 20
;;;; 
;;;; Note: This example requires the cl-png package 
;;;; to read in the image data. 
;;;;
;;;; https://github.com/ljosa/cl-png
;;;;
;;;; hazen 02/14
;;;;

(in-package :plplot-examples)

(defstruct stretch
  xmin
  xmax
  ymin
  ymax
  stretch)

(defun example20 (&optional (dev default-dev))
  (plsdev dev)
  (labels ((my-pltr (x y s)
	     (let ((x0 (* 0.5 (+ (stretch-xmin s) (stretch-xmax s))))
		   (y0 (* 0.5 (+ (stretch-ymin s) (stretch-ymax s))))
		   (dy (* 0.5 (- (stretch-ymax s) (stretch-ymin s)))))
	       (values (+ x0 (* (- x0 x)
				(- 1.0 (* (stretch-stretch s)
					  (cos (* (/ (- y y0) dy) pi 0.5))))))
		       y)))
	   (read-img ()
	     (let* ((img-rgb (let ((file-name (format nil "~A/src/examples/lena.png"
						      (asdf::system-source-directory :plplot-examples))))
			       (with-open-file (input file-name
						      :element-type '(unsigned-byte 8)
						      :external-format :binary)
				 (png:decode input))))
		    (nx (array-dimension img-rgb 0))
		    (ny (array-dimension img-rgb 1))
		    (img (make-float-array (list ny nx))))
	       (dotimes (i nx)
		 (dotimes (j ny)
		   (setf (aref img  j (- nx i 1)) (aref img-rgb i j 0))))
	       img)))
    (plinit)

    ; sombrero-like demo
    (let* ((xdim 260)
	   (ydim 220)
	   (x (make-float-array xdim))
	   (y (make-float-array ydim))
	   (z (make-float-array (list xdim ydim))))
      (dotimes (i xdim)
	(setf (aref x i) (* 2.0 i (/ 3.14159 (- xdim 1.0)))))
      (dotimes (i ydim)
	(setf (aref y i) (* 3.0 i (/ 3.14159 (- ydim 1.0)))))
      (dotimes (i xdim)
	(dotimes (j ydim)
	  (let ((r (+ (sqrt (+ (* (aref x i) (aref x i))
			       (* (aref y j) (aref y j))))
		      1.0e-3)))
	    (setf (aref z i j) (/ (sin r) r)))))
      (plcol0 2)
      (plenv 0.0 (* 2.0 3.14159) 0.0 (* 3.0 3.14150) 1 -1)
      (pllab "No, an amplitude clipped \"sombrero\"" "" "Saturn?")
      (plptex 2 2 3 4 0 "Transparent image")
      (plimage z 0.0 (* 2.0 3.14159) 0.0 (* 3.0 3.14159) 0.05 1.0
	       0.0 (* 2.0 3.14159) 0.0 (* 3.0 3.14159)))

    ;lena demos
    (let* ((lena (read-img))
	   (width (array-dimension lena 0))
	   (height (array-dimension lena 1)))
      (plscmap1n 255)
      (plscmap1l t (vector 0.0 1.0) (vector 0.0 1.0) (vector 0.0 1.0) (vector 0.0 1.0) nil)

      ; demo1
      (plenv 1.0 width 1.0 height 1 -1)
      (pllab "" " " "Lena...")
      (plimage lena 1.0 width 1.0 height 0.0 0.0 1 width 1 height)

      ; demo2
      (pladv 0)
      (plimage lena 1.0 width 1.0 height 0.0 0.0 200 330 220 280)

      ; demo3
      (plenv 200 330 220 280 1 -1)
      (plimage lena 1.0 width 1.0 height 0.0 0.0 200 330 220 280)

      ; demo4
      (multiple-value-bind (img-min img-max) (min-max lena)
	(plcol0 2)
	(plenv 0 width 0 height 1 -1)
	(pllab "" "" "Reduced dynamic range image example")
	(plimagefr lena 
		   0 width 0 height 0.0 0.0 
		   (+ img-min (* 0.25 img-max)) (- img-max (* img-max 0.25)) 
		   nil nil)
      
      ; demo5
      ;
      ; Note that for images the user defined grid needs to be
      ; +1 larger in x and y dimensions than the image or you
      ; will get a memory fault.
	(plenv 0 width 0 height 1 -1)
	(pllab "" "" "Distorted image example")
	(let ((stretch (make-stretch :xmin 0
				     :xmax width
				     :ymin 0
				     :ymax height
				     :stretch 0.5))
	      (cgrid-x (make-float-array (list (1+ width) (1+ height))))
	      (cgrid-y (make-float-array (list (1+ width) (1+ height)))))
	  (dotimes (i (1+ width))
	    (dotimes (j (1+ height))
	      (multiple-value-bind (xx yy) (my-pltr i j stretch)
		(setf (aref cgrid-x i j) xx
		      (aref cgrid-y i j) yy))))
	  (with-pltr-data (pltr-data cgrid-x cgrid-y)
	    (plimagefr lena 
		       0 width 0 height 0 0 
		       img-min img-max 
		       'pltr2-callback
		       pltr-data))))))

  (plend1))

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
