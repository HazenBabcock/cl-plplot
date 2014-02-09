;;;;
;;;; PLplot example 14
;;;;
;;;; hazen 02/14
;;;;

(in-package :plplot-examples)

(let ((tr))
  (defun x14-pltr (x y tx ty)
    (let ((tmpx (+ (* (aref tr 0) x)
		   (* (aref tr 1) y)
		   (aref tr 2)))
	  (tmpy (+ (* (aref tr 3) x)
		   (* (aref tr 4) y)
		   (aref tr 5))))
      (setf (cffi:mem-aref tx 'plflt) (cffi:convert-to-foreign tmpx 'plflt)
	    (cffi:mem-aref ty 'plflt) (cffi:convert-to-foreign tmpy 'plflt))))
  (defun x14-set-tr (new-tr)
    (setf tr new-tr)))

(cffi:defcallback x14-pltr-callback :void ((x plflt) (y plflt) (tx *plflt) (ty *plflt) (pltr-data :pointer))
  (declare (ignore pltr-data))
  (x14-pltr x y tx ty))


(defun example14 (&optional (dev default-dev))
  (labels ((plot1 (xoff xscale yoff yscale)
	     (let ((x (make-float-array 60))
		   (y (make-float-array 60))
		   (xs (make-float-array 6))
		   (ys (make-float-array 6)))
	       (dotimes (i 60)
		 (setf (aref x i) (+ xoff (* xscale (/ (+ i 1.0) 60.0)))
		       (aref y i) (+ yoff (* yscale (expt (aref x i) 2.0)))))
	       (let ((xmin (aref x 0))
		     (xmax (aref x 59))
		     (ymin (aref y 0))
		     (ymax (aref y 59)))
		 (dotimes (i 6)
		   (setf (aref xs i) (aref x (+ (* i 10) 3))
			 (aref ys i) (aref y (+ (* i 10) 3))))
		 (plcol0 1)
		 (plenv xmin xmax ymin ymax 0 0)
		 (plcol0 6)
		 (pllab "(x)" "(y)" "#frPLplot Example 1  - y=x#u2")
		 (plcol0 9)
		 (plpoin xs ys 9)
		 (plcol0 4)
		 (plline x y)
		 (plflush))))
	   
	   (plot2 ()
	     (let ((x (make-float-array 100))
		   (y (make-float-array 100)))
	       (plcol0 1)
	       (plenv -2.0 10.0 -0.4 1.2 0 1)
	       (plcol0 2)
	       (pllab "(x)" "sin(x)/x" "#frPLplot Example 1 - Sinc Function")
	       (dotimes (i 100)
		 (setf (aref x i) (/ (- i 19.0) 6.0)
		       (aref y i) 1.0)
		 (when (/= (aref x i) 0.0)
		   (setf (aref y i) (/ (sin (aref x i)) (aref x i)))))
	       (plcol0 3)
	       (plline x y)
	       (plflush)))

	   (plot3 ()
	     (let ((x (make-float-array 101))
		   (y (make-float-array 101))
		   (mark0 (make-int-array 1 0))
		   (space0 (make-int-array 1 0))
		   (mark1 (make-int-array 1 1500))
		   (space1 (make-int-array 1 1500)))
	       (pladv 0)
	       (plvsta)
	       (plwind 0.0 360.0 -1.2 1.2)
	       (plcol0 1)
	       (plbox "bcnst" 60.0 2 "bcnstv" 0.2 2)
	       (plstyl 1 mark1 space1)
	       (plcol0 2)
	       (plbox "g" 30.0 0 "g" 0.2 0)
	       (plstyl 0 mark0 space0)
	       (plcol0 3)
	       (pllab "Angle (degrees)" "sine" "#frPLplot Example 1 - Sine function")
	       (dotimes (i 101)
		 (setf (aref x i) (* 3.6 i)
		       (aref y i) (sin (/ (* (aref x i) 3.14159) 180.0))))
	       (plcol0 4)
	       (plline x y)
	       (plflush)))
	   
	   (plot4 ()
	     (let ((x0 (make-float-array 361))
		   (y0 (make-float-array 361))
		   (x (make-float-array 361))
		   (y (make-float-array 361)))
	       (let ((dtr (/ 3.14158 180.0)))
		 (dotimes (i 361)
		   (setf (aref x0 i) (cos (* dtr i))
			 (aref y0 i) (sin (* dtr i))))
		 (plenv -1.3 1.3 -1.3 1.3 1 -2)
		 (dotimes (i 10)
		   (dotimes (j 361)
		     (setf (aref x j) (* 0.1 (+ i 1.0) (aref x0 j))
			   (aref y j) (* 0.1 (+ i 1.0) (aref y0 j))))
		   (plline x y))
	       
		 (plcol0 2)
		 (dotimes (i 12)
		   (let* ((theta (* i 30.0))
			  (dx (cos (* dtr theta)))
			  (dy (sin (* dtr theta))))
		     (pljoin 0.0 0.0 dx dy)
		     (let ((text (write-to-string (round theta))))
		       (if (>= dx -0.00001)
			   (plptex dx dy dx dy -0.15 text)
			   (plptex dx dy (- dx) (- dy) 1.15 text)))))
		 (dotimes (i 361)
		   (let ((r (sin (* dtr 5.0 i))))
		     (setf (aref x i) (* (aref x0 i) r)
			   (aref y i) (* (aref y0 i) r)))))
	       (plcol0 3)
	       (plline x y)
	       (plcol0 4)
	       (plmtex "t" 2.0 0.5 0.5 "#frPLplot Example 3 - r(#gh)=sin 5#gh")
	       (plflush)))

	   (plot5 ()
	     (let* ((xpts 35)
		    (ypts 46)
		    (xspa (/ 2.0 (- xpts 1.0)))
		    (yspa (/ 2.0 (- ypts 1.0)))
		    (tr (vector xspa 0.0 -1.0 0.0 yspa -1.0))
		    (clevel (vector -1.0 -0.8 -0.6 -0.4 -0.2 0.0 0.2 0.4 0.6 0.8 1.0))
		    (z (make-float-array (list xpts ypts)))
		    (w (make-float-array (list xpts ypts)))
		    (mark (make-int-array 1 1500))
		    (space (make-int-array 1 1500)))
	       (dotimes (i xpts)
		 (let ((xx (/ (- i (/ xpts 2.0)) (/ xpts 2.0))))
		   (dotimes (j ypts)
		     (let ((yy (- (/ (- j (/ ypts 2.0)) (/ ypts 2.0)) 1.0)))
		       (setf (aref z i j) (- (* xx xx) (* yy yy))
			     (aref w i j) (* 2.0 xx yy))))))
	       (plenv -1.0 1.0 -1.0 1.0 0 0)
	       (plcol0 2)
	       (x14-set-tr tr)
	       (plcont z 1 xpts 1 ypts clevel 'x14-pltr-callback nil)
	       (plstyl 1 mark space)
	       (plcol0 3)
	       (plcont w 1 xpts 1 ypts clevel 'x14-pltr-callback nil)
	       (plcol0 1)
	       (pllab "X Coordinate" "Y Coordinate" "Streamlines of flow")
	       (plflush))))

    ;;main
    (plsdev dev)
    (multiple-value-bind (fam num bmax) (plgfam)
      (plsetopt "geometry" "500x410+100+200")
      (plssub 2 2)
      (plinit)

      (plsstrm 1)
      (plsetopt "geometry" "500x410+650+200")
      (plspause nil)
      (plsdev dev)
      (plsfam fam num bmax)
      (plsetopt "fflen" "2")
      (plinit))

    (plsstrm 0)
    (plot1 0.0 6.0 0.0 1.0)

    (plot1 0.0 1.0 0.0 1.0e+6)

    (plsyax 2 0)
    (plot1 0.0 1.0 0.0 1.0e-6)

    (plsyax 5 0)
    (plot1 0.0 1.0 0.0185 0.0014)

    (plsstrm 1)
    (plot4)
    (pleop)

    (plsstrm 0)
    (plot2)
    (plot3)

    (plsstrm 1)
    (plot5)
    (pleop)

    (plsstrm 0)
    (pleop))

  (plsstrm 0)
  (plend1)
  (plsstrm 1)
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
