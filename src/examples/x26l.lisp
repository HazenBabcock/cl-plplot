;;;;
;;;; PLplot example 26
;;;;
;;;; This example also require unicode support.
;;;;
;;;; hazen 07/10
;;;;

(in-package :plplot-examples)

(defun example26 (&optional (dev default-dev))
  (plsdev dev)
  (let ((x-label (vector "Frequency"
			 "Частота"))
	(y-label (vector "Amplitude (dB)"
			 "Амплитуда (dB)"))
	(alty-label (vector "Phase shift (degrees)"
			    "Фазовый сдвиг (градусы)"))
	(legend-text (vector (vector "Amplitude" "Phase shift")
			     (vector "Амплитуда" "Фазовый сдвиг")))
	(title-label (vector "Single Pole Low-Pass Filter"
			     "Однополюсный Низко-Частотный Фильтр"))
	(line-label (vector "-20 dB/decade"
			    "-20 dB/десяток")))
    (labels ((plot (type index)
	       (let ((freql (make-float-array 101))
		     (ampl (make-float-array 101))
		     (phase (make-float-array 101)))
		 (pladv 0)
		 (dotimes (i 101)
		   (setf (aref freql i) (+ -2.0 (/ i 20.0)))
		   (let ((freq (expt 10.0 (aref freql i))))
		     (setf (aref ampl i) (* 20.0
					    (log (/ 1.0
						    (sqrt (+ 1.0 (expt freq 2.0))))
						 10.0))
			   (aref phase i) (* -1.0 (/ 180.0 pi) (atan freq)))))
		 (plvpor 0.15d0 0.85d0 0.1d0 0.9d0)
		 (plwind -2.0 3.0 -80.0 0.0)
		 (plcol0 1)
		 (cond
		   ((= type 0)
		    (plbox "bclnst" 0.0 0 "bnstv" 0.0 0))
		   (t
		    (plbox "bcfghlnst" 0.0 0 "bcghnstv" 0.0 0)))
		 (plcol0 2)
		 (plline freql ampl)
		 (plcol0 2)
		 (plptex 1.6 -30.0 1.0 -20.0 0.5 (aref line-label index))
		 (plcol0 1)
		 (plmtex "b" 3.2 0.5 0.5 (aref x-label index))
		 (plmtex "t" 2.0 0.5 0.5 (aref title-label index))
		 (plcol0 2)
		 (plmtex "l" 5.0 0.5 0.5 (aref y-label index))
		 (when (= type 0)
		   (plcol0 1)
		   (plwind -2.0 3.0 -100.0 0.0)
		   (plbox "" 0.0 0 "cmstv" 30.0 3.0)
		   (plcol0 3)
		   (plline freql phase)
		   (plstring freql phase "#(728)")
		   (plcol0 3)
		   (plmtex "r" 5.0 0.5 0.5 (aref alty-label index)))
		 (plscol0a 15 32 32 32 0.7)
		 (pllegend (+ pl-legend-background pl-legend-bounding-box) 0
			   0.0 0.0 0.1 15
			   1 1 0 0
			   (vector pl-legend-line (+ pl-legend-line pl-legend-symbol))
			   1.0 1.0 2.0
			   1.0 (vector 2 3) (elt legend-text index)
			   nil nil nil nil
			   (vector 2 3) (vector 1 1) (vector 1.0 1.0)
			   (vector 0 3) (vector 0 1.0) (vector 0 4) (vector "" "#(728)")))))
      (plinit)
      (plfont 2)
      (dotimes (i (length x-label))
	(plot 0 i))
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
