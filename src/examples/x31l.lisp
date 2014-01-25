;;;;
;;;; PLplot example 31
;;;;
;;;; hazen 08/10
;;;;

(in-package :plplot-examples)

(defun example31 (&optional (dev default-dev))
  (let ((status 0))

    ; test family parameters
    (multiple-value-bind (fam0 num0 bmax0) (plgfam)
      (let ((fam1 0)
	    (num1 10)
	    (bmax1 1000))
	(plsfam fam1 num1 bmax1)
	(multiple-value-bind (fam2 num2 bmax2) (plgfam)
	  (when (or (/= fam1 fam2)
		    (/= num1 num2)
		    (/= bmax1 bmax2))
	    (format t "plgfam test failed~%")
	    (setf status 1))))
      (plsfam fam0 num0 bmax0))

    ; test setting / getting page parameters
    (multiple-value-bind (xp0 yp0 xleng0 yleng0 xoff0 yoff0) (plgpage)
      (let ((xp1 200)
	    (yp1 200)
	    (xleng1 400)
	    (yleng1 200)
	    (xoff1 10)
	    (yoff1 20))
	(plspage xp1 yp1 xleng1 yleng1 xoff1 yoff1)
	(multiple-value-bind (xp2 yp2 xleng2 yleng2 xoff2 yoff2) (plgpage)
	  (when (or (/= xp1 xp2)
		    (/= yp1 yp2)
		    (/= xleng1 xleng2)
		    (/= yleng1 yleng2)
		    (/= xoff1 xoff2)
		    (/= yoff1 yoff2))
	    (format t "plgfam test failed~%")
	    (setf status 1))))
      (plspage xp0 yp0 xleng0 yleng0 xoff0 yoff0))

    ; test setting / getting compression
    (let ((compression1 95))
      (plscompression compression1)
      (plsdev dev)
      (plinit)
      (let ((compression2 (plgcompression)))
	(format t "Output various PLplot parameters~%")
	(format t "compression parameter = ~A~%" compression2)
	(when (/= compression1 compression2)
	  (format t "plgcompression test failed~%")
	  (setf status 1))))

    ;exercise plscolor, plscol0, plscmap1 and plscmap1a
    (plscolor 1)
    (plscol0 1 255 0 0)
    (plscmap1 (vector 0 255) (vector 255 0) (vector 0 0))
    (plscmap1a (vector 0 255) (vector 255 0) (vector 0 0) (vector 1.0 1.0))

    (let ((level2 (plglevel)))
      (format t "level parameter = ~A~%" level2)
      (when (/= level2 1)
	(format t "plglevel test failed.~%")
	(setf status 1)))

    (pladv 0)
    (plvpor 0.01 0.99 0.02 0.49)
    (multiple-value-bind (xmin xmax ymin ymax) (plgvpd)
      (when (or (/= xmin 0.01)
		(/= xmax 0.99)
		(/= ymin 0.02)
		(/= ymax 0.49))
	(format t "plgvpd test failed~%")
	(setf status 1))
      (let ((xmid (* 0.5 (+ xmin xmax)))
	    (ymid (* 0.5 (+ ymin ymax))))
	(plwind 0.2 0.3 0.4 0.5)
	(multiple-value-bind (xmin xmax ymin ymax) (plgvpw)
	  (format t "plwind: xmin, xmax, ymin, ymax = ~A ~A ~A ~A~%" xmin xmax ymin ymax)
	  (when (or (/= xmin 0.2)
		    (/= xmax 0.3)
		    (/= ymin 0.4)
		    (/= ymax 0.5))
	    (format t "plgvpw test failed~%")
	    (setf status 1)))
	(multiple-value-bind (wx wy win) (plcalc-world xmid ymid)
	  (declare (ignore win))
	  (when (or (< (abs (- wx (* 0.5 (+ xmin xmax)))) 1.0e-5)
		    (< (abs (- wy (* 0.5 (+ ymin ymax)))) 1.0e-5))
	    (format t "plcalc-world test failed~%")
	    (setf status 1)))))

    (let ((fnam (plgfnam)))
      (if (= (length fnam) 0)
	  (format t "No output file name is set~%")
	  (format t "Output file name read~%"))
      (format t "Output file name is ~A~%" fnam))

    (plsxax 3 0)
    (multiple-value-bind (digmax digits) (plgxax)
      (format t "x axis parameters: digmax, digits = ~A ~A~%" digmax digits)
      (when (/= digmax 3)
	(format t "plgxax test failed~%")
	(setf status 1)))

    (plsyax 4 0)
    (multiple-value-bind (digmax digits) (plgyax)
      (format t "y axis parameters: digmax, digits = ~A ~A~%" digmax digits)
      (when (/= digmax 4)
	(format t "plgyax test failed~%")
	(setf status 1)))

    (plszax 5 0)
    (multiple-value-bind (digmax digits) (plgzax)
      (format t "z axis parameters: digmax, digits = ~A ~A~%" digmax digits)
      (when (/= digmax 5)
	(format t "plgzax test failed~%")
	(setf status 1)))

    (plsdidev 0.05 -42 0.1 0.2)
    (multiple-value-bind (mar aspect jx jy) (plgdidev)
      (format t "device-space window parameters: mar, aspect, jx, jy = ~A ~A ~A ~A~%" mar aspect jx jy)
      (when (or (/= mar 0.05)
		(/= jx 0.1)
		(/= jy 0.2))
	(format t "plgdidev test failed")
	(setf status 1)))

    (plsdiori 1.0)
    (let ((ori (plgdiori)))
      (format t "ori parameter = ~A~%" ori)
      (when (/= ori 1.0)
	(format t "plgdiori test failed~%")
	(setf status 1)))

    (plsdiplt 0.1 0.2 0.9 0.8)
    (multiple-value-bind (xmin ymin xmax ymax) (plgdiplt)
      (format t "plot-space window parameters: xmin, ymin, ymax, ymax = ~A ~A ~A ~A~%" xmin ymin xmax ymax)
      (when (or (/= xmin 0.1)
		(/= xmax 0.9)
		(/= ymin 0.2)
		(/= ymax 0.8))
	(format t "plgdiplt test failed~%")
	(setf status 1))

      (plsdiplz 0.1 0.1 0.9 0.9)
      (multiple-value-bind (zxmin zymin zxmax zymax) (plgdiplt)
	(format t "zoomed plot-space window parameters: xmin, ymin, xmax, ymax = ~A ~A ~A ~A~%" zxmin zymin zxmax zymax)
	(when (or (> (abs (- zxmin (+ xmin (* (- xmax xmin) 0.1)))) 1.0e-5)
		  (> (abs (- zxmax (+ xmin (* (- xmax xmin) 0.9)))) 1.0e-5)
		  (> (abs (- zymin (+ ymin (* (- ymax ymin) 0.1)))) 1.0e-5)
		  (> (abs (- zymax (+ ymin (* (- ymax ymin) 0.9)))) 1.0e-5))
	  (format t "plsdiplz test failed~%")
	  (setf status 1))))

    (plscolbg 10 20 30)
    (multiple-value-bind (r g b) (plgcolbg)
      (format t "background color parameters: r, g, b = ~A ~A ~A~%" r g b)
      (when (or (/= r 10)
		(/= g 20)
		(/= b 30))
	(format t "plcolbg test failed~%")
	(setf status 1)))

    (plscolbga 20 30 40 0.5)
    (multiple-value-bind (r g b a) (plgcolbga)
      (format t "background/transparency colour parameters: r, g, b, a = ~A ~A ~A ~A~%" r g b a)
      (when (or (/= r 20)
		(/= g 30)
		(/= b 40)
		(/= a 0.5))
	(format t "plgcolbga test failed")
	(setf status 1))))
  
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
