;;;;
;;;; PLplot example 27
;;;;
;;;; hazen 02/14
;;;;

(in-package :plplot-examples)

(defun example27 (&optional (dev default-dev))
  (plsdev dev)
  (plsetopt "eofill" "")
  (labels ((spiro (params fill)
	     (let* ((npnt 2000)
		    (xcoord (make-float-array (1+ npnt)))
		    (ycoord (make-float-array (1+ npnt)))
		    (windings (floor (/ (abs (floor (aref params 1)))
					(gcd (floor (aref params 0)) (floor (aref params 1))))))
		    (steps (floor (/ npnt windings)))
		    (dphi (* 2.0 (/ pi (float steps)))))
	       (dotimes (i (1+ (* windings steps)))
		 (let* ((dp (- (aref params 0) (aref params 1)))
			(phi (* i dphi))
			(phiw (* (/ dp (aref params 1)) phi)))
		   (setf (aref xcoord i) (+ (* dp (cos phi))
					    (* (aref params 2) (cos phiw)))
			 (aref ycoord i) (- (* dp (sin phi))
					    (* (aref params 2) (sin phiw))))))
	       (multiple-value-bind (xmin xmax) (min-max xcoord)
		 (multiple-value-bind (ymin ymax) (min-max ycoord)
		   (let ((xrange-adjust (* 0.15 (- xmax xmin)))
			 (yrange-adjust (* 0.15 (- ymax ymin))))
		     (plwind (- xmin xrange-adjust) (+ xmax xrange-adjust) 
			     (- ymin yrange-adjust) (+ ymax yrange-adjust))
		     (plcol0 1)
		     (if fill
			 (plfill (adjust-array xcoord (1+ (* windings steps)))
				 (adjust-array ycoord (1+ (* windings steps))))
			 (plline (adjust-array xcoord (1+ (* windings steps)))
				 (adjust-array ycoord (1+ (* windings steps))))))))))
	   (arc ()
	     (let* ((nseg 8)
		    (theta 0.0)
		    (dtheta (/ 360.0 (float nseg))))
	       (plenv -10.0 10.0 -10.0 10.0 1 0)
	       (dotimes (i nseg)
		 (plcol0 (+ (mod i 2) 1))
		 (plarc 0.0 0.0 8.0 8.0 theta (+ theta dtheta) 0.0 nil)
		 (incf theta dtheta))
	       (let* ((a 3.0)
		      (b (* a (tan (/ (/ (* dtheta pi) 180.0) 2.0))))
		      (theta (/ dtheta 2.0)))
		 (dotimes (i nseg)
		   (plcol0 (- 2 (mod i 2)))
		   (plarc (* a (cos (/ (* theta pi) 180.0))) (* a (sin (/ (* theta pi) 180.0)))
			  a b 0.0 360.0 theta 1)
		   (incf theta dtheta))))))
			   
    (let ((params (vector (vector 21.0 7.0 7.0 3.0)
			  (vector 21.0 7.0 10.0 3.0)
			  (vector 21.0 -7.0 10.0 3.0)
			  (vector 20.0 3.0 7.0 20.0)
			  (vector 20.0 3.0 10.0 20.0)
			  (vector 20.0 -3.0 10.0 20.0)
			  (vector 20.0 13.0 7.0 20.0)
			  (vector 20.0 13.0 20.0 20.0)
			  (vector 20.0 -13.0 20.0 20.0))))
      (plinit)
      (plssub 3 3)
      (dotimes (i 9)
	(pladv 0)
	(plvpor 0.0 1.0 0.0 1.0)
	(spiro (aref params i) nil))

      (pladv 0)
      (plssub 1 1)
      (dotimes (i 9)
	(pladv 0)
	(plvpor 0.0 1.0 0.0 1.0)
	(spiro (aref params i) nil))

      (pladv 0)
      (plssub 1 1)
      (dotimes (i 9)
	(pladv 0)
	(plvpor 0.0 1.0 0.0 1.0)
	(spiro (aref params i) t))

      (arc)
      (plend1))))

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
