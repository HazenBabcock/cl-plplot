;;;;
;;;; PLplot example 2
;;;;
;;;; hazen 01/14
;;;;

(in-package :plplot-examples)

(defun example2 (&optional (dev default-dev))
  (plsdev dev)
  (plinit)
  
  (labels ((draw-windows (nw cmap0-offset)
	     (plschr 0.0 3.5)
	     (plfont 4)
	     (dotimes (i nw)
	       (plcol0 (+ i cmap0-offset))
	       (pladv 0)
	       (let ((vmin 0.1)
		     (vmax 0.9))
		 (dotimes (j 3)
		   (plwidth (+ j 1))
		   (plvpor vmin vmax vmin vmax)
		   (plwind 0.0 1.0 0.0 1.0)
		   (plbox "bc" 0.0 0 "bc" 0.0 0)
		   (incf vmin 0.1)
		   (decf vmax 0.1)))
	       (plwidth 1)
	       (plptex 0.5 0.5 1.0 0.0 0.5 (write-to-string i)))))

    ;; page 1
    (plbop)
    (plssub 4 4)
    (draw-windows 16 0)
    (pleop)

    ;; page 2
    (plbop)
    (plssub 10 10)
    (let ((r (make-int-array 116))
	  (g (make-int-array 116))
	  (b (make-int-array 116)))
      (dotimes (i 100)
	(let ((h (* (/ 360.0 10.0) (mod i 10)))
	      (l (+ 0.15 (/ (* (- 0.85 0.15) (floor (/ i 10))) 9.0))))
	  (multiple-value-bind (r1 g1 b1)
	      (plhlsrgb h l 1.0)
	    (setf (aref r (+ i 16)) (floor (* r1 255.001))
		  (aref g (+ i 16)) (floor (* g1 255.001))
		  (aref b (+ i 16)) (floor (* b1 255.001))))))
      (dotimes (i 16)
	(multiple-value-bind (r1 g1 b1)
	    (plgcol0 i)
	  (setf (aref r i) r1
		(aref g i) g1
		(aref b i) b1)))
      (plscmap0 r g b))
    (draw-windows 100 16)
    (pleop))
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
