;;;;
;;;; PLplot example 25
;;;;
;;;; hazen 07/10
;;;;

(in-package :plplot-examples)

(defun example25 (&optional (dev default-dev))
  (plsdev dev)
  (let ((xextreme (vector (vector -120 -120 -120 -80 -220 -20 -20 -80 20)
			  (vector 120 120 120 80 -120 20 20 80 120)))
	(yextreme (vector (vector -120 20 -20 -20 -120 -120 -20 -80 -120)
			  (vector 120 120 120 120 120 120 20 80 120))))
    (plssub 3 3)
    (plinit)
    (dotimes (k 2)
      (dotimes (j 4)
	(let ((x0 (cond
		    ((= j 0) (vector 0 -100 0 100))
		    ((= j 1) (vector 100 0 -100 0))
		    ((= j 2) (vector -100 -100 80 -100 -100 -80 0 80 100 100))
		    (t (vector 100 100 80 0 -80 -100 -100 80 -100 -100))))
	      (y0 (cond
		    ((= j 0) (vector -100 0 100 0))
		    ((= j 1) (vector 0 100 0 -100))
		    ((= j 2) (vector -100 -80 0 80 100 100 80 100 100 -100))
		    (t (vector -100 100 100 80 100 100 80 0 -80 -100)))))
	  (dotimes (i 9)
	    (pladv 0)
	    (plvsta)
	    (plwind (aref (aref xextreme 0) i)
		    (aref (aref xextreme 1) i)
		    (aref (aref yextreme 0) i)
		    (aref (aref yextreme 1) i))
	    (plcol0 2)
	    (plbox "bc" 1.0 0 "bcnv" 10.0 0)
	    (plcol0 1)
	    (plpsty 0)
	    (if (= k 0)
		(plfill x0 y0)
		(plgradient x0 y0 45.0))
	    (plcol0 2)
	    (pllsty 1)
	    (plline x0 y0)))))
    (plend1)))

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
