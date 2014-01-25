;;;;
;;;; PLplot example 32
;;;;
;;;; hazen 08/10
;;;;

(in-package :plplot-examples)

(defun example32 (&optional (dev default-dev))
  (labels ((plfbox (x y25 y50 y75 lw uw)
	     (let* ((spacing 0.4)
		    (xmin (+ x (/ spacing 2.0)))
		    (xmax (+ x 1.0 (- (/ spacing 2.0))))
		    (px (vector xmin xmin xmax xmax xmin))
		    (py (vector y25 y75 y75 y25 y25)))

	       ; box
	       (plpsty 0)
	       (plfill px py)
	       (plcol0 1)
	       (pllsty 1)
	       (plline px py)
	       
	       ; median
	       (pllsty 1)
	       (plline (vector xmin xmax) (vector y50 y50))

	       ; lower whisker
	       (let ((xmid (/ (+ xmin xmax) 2.0))
		     (xwidth (- xmax xmin)))
		 (pllsty 2)
		 (plline (vector xmid xmid) (vector lw y25))
		 (pllsty 1)
		 (plline (vector (- xmid (/ xwidth 4.0)) (+ xmid (/ xwidth 4.0)))
			 (vector lw lw)))

	       ; upper whisker
	       (let ((xmid (/ (+ xmin xmax) 2.0))
		     (xwidth (- xmax xmin)))
		 (pllsty 2)
		 (plline (vector xmid xmid) (vector y75 uw))
		 (pllsty 1)
		 (plline (vector (- xmid (/ xwidth 4.0)) (+ xmid (/ xwidth 4.0)))
			 (vector uw uw))))))
    (let ((y25 (vector 0.984 0.980 0.976 0.975 0.973
		       0.967 0.974 0.954 0.987 0.991))
	  (y50 (vector 0.994 0.999 1.035 0.995 1.002
		       0.997 1.034 0.984 1.007 1.017))
	  (y75 (vector 1.054 1.040 1.066 1.025 1.043
		       1.017 1.054 1.004 1.047 1.031))
	  (lw (vector 0.964 0.950 0.926 0.955 0.963
		      0.937 0.944 0.924 0.967 0.941))
	  (uw (vector 1.071 1.062 1.093 1.045 1.072
		      1.067 1.085 1.024 1.057 1.071))
	  (outx (vector 3.5 6.5))
	  (outy (vector 0.89 1.09)))
      (plsdev dev)
      (plinit)
      (pladv 0)
      (plvsta)
      (let ((x0 1.0))
	(plwind x0 (+ x0 10.0) 0.85 1.15)
	(plcol0 1)
	(plbox "bc" 1.0 0 "bcgnst" 0.0 0)
	(pllab "Group" "Value" "#frPLplot Example 32")
	(dotimes (i 10)
	  (plcol1 (/ i 9.0))
	  (plfbox (+ x0 i) (aref y25 i) (aref y50 i) (aref y75 i) (aref lw i) (aref uw i))
	  (plmtex "b" 1.0 (- (* (+ i 1) 0.1) 0.05) 0.5
		  (format nil "~A" (round (+ x0 i))))))
      (plpoin outx outy 22)
      (plend1))))

;;;;
;;;; Copyright (c) 2010 Hazen P. Babcock
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
