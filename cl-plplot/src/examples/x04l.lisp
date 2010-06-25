;;;;
;;;; PLplot example 4
;;;;
;;;; hazen 06/10
;;;;

(in-package :plplot-examples)

(defun example4 (&optional (dev default-dev))
  (plsdev dev)
  (plinit)
  (plfont 2)
  (labels ((plot (type)
	     (let ((freql (make-float-array 101))
		   (ampl (make-float-array 101))
		   (phase (make-float-array 101)))
	       (dotimes (i 101)
		 (setf (aref freql i) (+ -2.0 (/ i 20.0)))
		 (let ((freq (expt 10.0 (aref freql i))))
		   (setf (aref ampl i)
			 (* 20.0 (log (/ 1.0 (sqrt (+ 1.0 (expt freq 2.0)))) 10.0))
			 (aref phase i)
			 (- (* (/ 180.0 3.14159) (atan freq))))))
	       (pladv 0)
	       (plvpor 0.15 0.85 0.1 0.9)
	       (plwind -2.0 3.0 -80.0 0.0)
	       (plcol0 1)
	       (if type
		   (plbox "bcfghlnst" 0.0 0 "bcghnstv" 0.0 0)
		   (plbox "bclnst" 0.0 0 "bnstv" 0.0 0))
	       (plcol0 2)
	       (plline freql ampl)
	       (plcol0 1)
	       (plptex 1.6 -30.0 1.0 -20.0 0.5 "-20 dB/decade")
	       (plcol0 1)
	       (plmtex "b" 3.2 0.5 0.5 "Frequency")
	       (plmtex "t" 2.0 0.5 0.5 "Single Pole Low-Pass Filter")
	       (plcol0 2)
	       (plmtex "l" 5.0 0.5 0.5 "Amplitude (dB)")
	       (unless type
		 (plcol0 1)
		 (plwind -2.0 3.0 -100.0 0.0)
		 (plbox "" 0.0 0 "cmstv" 30.0 3)
		 (plcol0 3)
		 (plline freql phase)
		 (plcol0 3)
		 (plmtex "r" 5.0 0.5 0.5 "Phase shift (degrees)")))))
    (plot nil)
    (plot t))
    
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
