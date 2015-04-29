;;;;
;;;; PLplot example 17
;;;;
;;;; hazen 07/10
;;;;

(in-package :plplot-examples)

(defun example17 (&optional (dev default-dev))
  (plsdev dev)
  (plinit)
  (pladv 0)
  (plvsta)
  (let ((id (plstripc "bcnst" "bcnstv" 0.0 10.0 0.3 -0.1 0.1 0.0 0.25 1 1 1 3
		      (vector 2 3 4 5)
		      (vector 2 3 4 5)
		      (vector "sum" "sin" "sin*noi" "sin+noi")
		      "t" "" "Strip chart demo"))
	(y1 0.0)
	(y2 0.0)
	(y3 0.0)
	(y4 0.0)
	(dt 0.1))
    (dotimes (n 1000)
      (sleep 0.01)
      (let ((time (* n dt))
	    (noise (- (plrandd) 0.5)))
	(setf y1 (+ y1 noise)
	      y2 (sin (/ (* time pi) 18.0))
	      y3 (* y2 noise)
	      y4 (+ y2 (/ noise 3.0)))
	(when (/= (mod n 2) 0)
	  (plstripa id 0 time y1))
	(when (/= (mod n 3) 0)
	  (plstripa id 1 time y2))
	(when (/= (mod n 4) 0)
	  (plstripa id 2 time y3))
	(when (/= (mod n 5) 0)
	  (plstripa id 3 time y4))))
    (plstripd id))
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
