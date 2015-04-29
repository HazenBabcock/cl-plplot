;;;;
;;;; PLplot example 18
;;;;
;;;; hazen 07/10
;;;;

(in-package :plplot-examples)

(defun example18 (&optional (dev default-dev))
  (plsdev dev)
  (plinit)
  (let ((opt (vector 1 0 1 0))
	(alt (vector 20.0 35.0 50.0 65.0))
	(az (vector 30.0 40.0 50.0 60.0)))
    (labels ((test-poly (k)
	       (let ((draw (vector (vector 1 1 1 1)
				   (vector 1 0 1 0)
				   (vector 0 1 0 1)
				   (vector 1 1 0 0))))
		 (pladv 0)
		 (plvpor 0.0 1.0 0.0 0.9)
		 (plwind -1.0 1.0 -0.9 1.1)
		 (plcol0 1)
		 (plw3d 1.0 1.0 1.0 -1.0 1.0 -1.0 1.0 -1.0 1.0 (aref alt k) (aref az k))
		 (plbox3 "bnstu" "x axis" 0.0 0
			 "bnstu" "y axis" 0.0 0
			 "bcdmnstuv" "z axis" 0.0 0)
		 (plcol0 2)
		 (labels ((theta (a)
			    (/ (* pi 2.0 a) 20.0))
			  (phi (a)
			    (/ (* pi a) 20.1)))
		   (dotimes (i 20)
		     (dotimes (j 20)
		       (plpoly3 (vector (* (sin (phi j)) (cos (theta i)))
					(* (sin (phi (+ j 1))) (cos (theta i)))
					(* (sin (phi (+ j 1))) (cos (theta (+ i 1))))
					(* (sin (phi j)) (cos (theta (+ i 1))))
					(* (sin (phi j)) (cos (theta i))))
				(vector (* (sin (phi j)) (sin (theta i)))
					(* (sin (phi (+ j 1))) (sin (theta i)))
					(* (sin (phi (+ j 1))) (sin (theta (+ i 1))))
					(* (sin (phi j)) (sin (theta (+ i 1))))
					(* (sin (phi j)) (sin (theta i))))
				(vector (cos (phi j))
					(cos (phi (+ j 1)))
					(cos (phi (+ j 1)))
					(cos (phi j))
					(cos (phi j)))
				(aref draw k)
				1))))
		 (plcol0 3)
		 (plmtex "t" 1.0 0.5 0.5 "unit radius sphere"))))

      ;; first 4 plots
      (dotimes (k 4)
	(test-poly k)))
    
    ;; second 4 plots
    (let* ((npts 1000)
	   (x (make-float-array npts))
	   (y (make-float-array npts))
	   (z (make-float-array npts)))
      (dotimes (i npts)
	(setf (aref z i) (- (* 2.0 (/ i npts)) 1)
	      (aref x i) (* (aref z i) (cos (* 2.0 pi 6.0 (/ i npts))))
	      (aref y i) (* (aref z i) (sin (* 2.0 pi 6.0 (/ i npts))))))

      (dotimes (k 4)
	(pladv 0)
	(plvpor 0.0 1.0 0.0 0.9)
	(plwind -1.0 1.0 -0.9 1.1)
	(plcol0 1)
	(plw3d 1.0 1.0 1.0 -1.0 1.0 -1.0 1.0 -1.0 1.0 (aref alt k) (aref az k))
	(plbox3 "bnstu" "x axis" 0.0 0
		"bnstu" "y axis" 0.0 0
		"bcdmnstuv" "z axis" 0.0 0)
	(plcol0 2)
	(if (/= (aref opt k) 0)
	    (plline3 x y z)
	    (plstring3 x y z "⋅"))
	(plcol0 3)
	(plmtex "t" 1.0 0.5 0.5 
		(format nil "#frPLplot Example 18 - Alt=~d, Az=~d" (round (aref alt k)) (round (aref az k)))))))

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
