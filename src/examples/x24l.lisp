;;;;
;;;; PLplot example 24
;;;;
;;;; This requires unicode support.
;;;;
;;;; hazen 07/10
;;;;

(in-package :plplot-examples)

(defun example24 (&optional (dev default-dev))
  (plsdev dev)
  (let ((red (vector 240 204 204 204 0 39 125))
	(green (vector 240 0 125 204 204 80 0))
	(blue (vector 240 0 0 0 0 204 125))
	(px (vector 0.0 0.0 1.0 1.0))
	(py (vector 0.0 0.25 0.25 0.0))
	(sx (vector 0.16374
		    0.15844
		    0.15255
		    0.17332
		    0.50436
		    0.51721
		    0.49520
		    0.48713
		    0.83976
		    0.81688
		    0.82231
		    0.82647))
	(sy (vector 0.125
		    0.375
		    0.625
		    0.875
		    0.125
		    0.375
		    0.625
		    0.875
		    0.125
		    0.375
		    0.625
		    0.875))
	(peace (vector "#<0x00>和平"      ;Mandarin
		       "#<0x20>शांति"       ;Hindi
		       "#<0x10>Peace"     ; English
		       "#<0x10>שלום"      ; Hebrew
		       "#<0x10>Мир"       ; Russian
		       "#<0x10>Friede"    ; German
		       "#<0x30>평화"       ; Korean
		       "#<0x10>Paix"      ; French
		       "#<0x10>Paz"       ; Spanish
		       "#<0x10>ﺳﻼم"       ; Arabic
		       "#<0x10>Barış"     ; Turkish
		       "#<0x10>Hasîtî"))) ; Kurdish

    (plinit)
    (pladv 0)
    (plvpor 0.0 1.0 0.0 1.0)
    (plwind 0.0 1.0 0.0 1.0)
    (plcol0 0)
    (plbox "" 1.0 0 "" 1.0 0)
    (plscmap0n 7)
    (plscmap0 red green blue)
    (plschr 0 4.0)
    (plfont 1)
    
    (dotimes (i 4)
      (plcol0 (+ i 1))
      (plfill px py)
      (dotimes (j 4)
	(setf (aref py j) (+ (aref py j) (/ 1.0 4.0)))))
    
    (plcol0 0)
    (dotimes (i 12)
      (plptex (aref sx i) (aref sy i) 1.0 0.0 0.5 (aref peace i)))
    
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
