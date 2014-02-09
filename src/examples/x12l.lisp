;;;;
;;;; PLplot example 12
;;;;
;;;; hazen 07/10
;;;;

(in-package :plplot-examples)

(defun example12 (&optional (dev default-dev))
  (plsdev dev)
  (plinit)
  (let ((pos (vector 0.0 0.25 0.5 0.75 1.0))
	(red (vector 0.0 0.25 0.5 1.0 1.0))
	(green (vector 1.0 0.5 0.5 0.5 1.0))
	(blue (vector 1.0 1.0 0.5 0.25 0.0))
	(y0 (vector 5 15 12 24 28 30 20 8 12 3)))
    (pladv 0)
    (plvsta)
    (plwind 1980.0 1990.0 0.0 35.0)
    (plbox "bc" 1.0 0 "bcnv" 10.0 0)
    (plcol0 2)
    (pllab "Year" "Widget Sales (millions)" "#frPLplot Example 12")
    (plscmap1l t pos red green blue nil)
    (labels ((plfbox (x0 y0)
	       (let ((x (vector x0 x0 (+ x0 1.0) (+ x0 1.0)))
		     (y (vector 0.0 y0 y0 0.0)))
		 (plfill x y)
		 (plcol0 1)
		 (pllsty 1)
		 (plline x y))))
      (dotimes (i 10)
	(plcol1 (/ i 9.0))
	(plpsty 0)
	(plfbox (+ 1980.0 i) (aref y0 i))
	(plptex (+ 1980.0 i 0.5) (+ (aref y0 i) 1.0) 1.0 0.0 0.5 (write-to-string (aref y0 i)))
	(plmtex "b" 1.0 (- (* (+ i 1) 0.1) 0.05) 0.5 (write-to-string (+ 1980 i))))))

  (plend1))

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
