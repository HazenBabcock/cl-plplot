;;;;
;;;; PLplot example 16
;;;;
;;;; hazen 02/14
;;;;

(in-package :plplot-examples)

(defun example16 (&optional (dev default-dev))
  (plsdev dev)

  (let* ((ns 20)
	 (nx 35)
	 (ny 46)
	 (cont-width 0.0)
	 (cont-color 0)
	 (axis-opts (vector "bcvtm"))
	 (axis-ticks (vector 0.0))
	 (axis-subticks (vector 0.0))
	 (label-opts (vector pl-colorbar-label-bottom))
	 (labels-text (vector "Magnitude"))
	 (n-values (vector (1+ ns)))
	 (values-arr (make-float-array (list 1 (1+ ns))))
	 (tr (make-float-array 6))
	 (cgrid0-x (make-float-array nx))
	 (cgrid0-y (make-float-array ny))
	 (cgrid1-x (make-float-array nx))
	 (cgrid1-y (make-float-array ny))
	 (cgrid2-x (make-float-array (list nx ny)))
	 (cgrid2-y (make-float-array (list nx ny)))
	 (z (make-float-array (list nx ny)))
	 (w (make-float-array (list nx ny)))
	 (clevel (make-float-array ns))
	 (shedge (make-float-array (1+ ns))))
    (labels ((my-pltr (x y)
	       (values (+ (* (aref tr 0) x) (* (aref tr 1) y) (aref tr 2))
		       (+ (* (aref tr 3) x) (* (aref tr 4) y) (aref tr 5))))
;		 (setf (cffi:mem-aref tx :double) (coerce tmpx 'double-float)
;		       (cffi:mem-aref ty :double) (coerce tmpy 'double-float)))))
	     (colorbar ()
	       (plschr 0.0 0.75)
	       (plsmaj 0.0 0.5)
	       (plsmaj 0.0 0.5)
	       (plcolorbar (+ pl-colorbar-shade pl-colorbar-shade-label) 0
			   0.005 0.0 0.0375 0.875 0 1 1 0.0 0.0
			   cont-color cont-width
			   label-opts labels-text
			   axis-opts axis-ticks axis-subticks
			   n-values values-arr)
	       (plschr 0.0 1.0)
	       (plschr 0.0 1.0)
	       (plschr 0.0 1.0)))
      (setf (aref tr 0) (/ 2.0 (- nx 1.0))
	    (aref tr 1) 0.0
	    (aref tr 2) -1.0
	    (aref tr 3) 0.0
	    (aref tr 4) (/ 2.0 (- ny 1.0))
	    (aref tr 5) -1.0)
      (dotimes (i nx)
	(let ((x (/ (- i (floor (/ nx 2))) (floor (/ nx 2)))))
	  (dotimes (j ny)
	    (let ((y (- (/ (- j (/ ny 2.0)) (/ ny 2.0)) 1.0)))
	      (setf (aref z i j) (- (* x x) (* y y) (* (sin (* 7.0 x)) (cos (* 7.0 y))))
		    (aref w i j) (- (* 2 x y) (* (cos (* 7.0 x)) (sin (* 7.0 y)))))))))
      (multiple-value-bind (zmin zmax) (min-max z)
	(dotimes (i ns)
	  (setf (aref clevel i) (+ zmin (/ (* (- zmax zmin) (+ i 0.5)) ns))))
	(dotimes (i (1+ ns))
	  (setf (aref shedge i) (+ zmin (/ (* (- zmax zmin) i) ns)))
	  (setf (aref values-arr 0 i) (aref shedge i))))
      (dotimes (i nx)
	(dotimes (j ny)
	  (multiple-value-bind (x y) (my-pltr i j)
	    (let ((argx (/ (* x pi) 2.0))
		  (argy (/ (* y pi) 2.0))
		  (distort 0.4))
	      (setf (aref cgrid0-x i) x
		    (aref cgrid0-y j) y
		    (aref cgrid1-x i) (+ x (* distort (cos argx)))
		    (aref cgrid1-y j) (- y (* distort (cos argy)))
		    (aref cgrid2-x i j) (+ x (* distort (cos argx) (cos argy)))
		    (aref cgrid2-y i j) (- y (* distort (cos argx) (cos argy))))))))

      ; plot1
      (plspal0 "cmap0_black_on_white.pal")
      (plspal1 "cmap1_gray.pal" t)
      (plscmap0n 3)
      (plinit)
      (pladv 0)
      (plvpor 0.1 0.9 0.1 0.9)
      (plwind -1.0 1.0 -1.0 1.0)
      (plpsty 0)
      (plshades z
		nil
		-1.0 1.0 -1.0 1.0
		shedge 2 0 0
		'plfill-callback
		t 
		nil nil)
      (colorbar)
      (plcol0 1)
      (plbox "bcnst" 0.0 0 "bcnstv" 0.0 0)
      (plcol0 2)
      (pllab "distance" "altitude" "Bogon density")
     
      ;plot2
      (plspal0 "cmap0_black_on_white.pal")
      (plspal1 "cmap1_blue_yellow.pal" t)
      (plscmap0n 3)
      (pladv 0)
      (plvpor 0.1 0.9 0.1 0.9)
      (plwind -1.0 1.0 -1.0 1.0)
      (plpsty 0)
      (with-pltr-data (pltr-data cgrid1-x cgrid1-y :z-vals z)
	(plshades z 
		  nil
		  -1.0 1.0 -1.0 1.0 
		  shedge 2 0 0
		  'plfill-callback
		  t
		  'pltr1-callback 
		  pltr-data))
      (colorbar)
      ;cgrid1-x cgrid1-y)
      (plcol0 1)
      (plbox "bcnst" 0.0 0 "bcnstv" 0.0 0)
      (plcol0 2)
      (pllab "distance" "altidude" "Bogon density")

      ; plot3
      (plspal0 "cmap0_black_on_white.pal")
      (plspal1 "cmap1_blue_red.pal" t)
      (plscmap0n 3)
      (pladv 0)
      (plvpor 0.1 0.9 0.1 0.9)
      (plwind -1.0 1.0 -1.0 1.0)
      (plpsty 0)
      (with-pltr-data (pltr-data cgrid2-x cgrid2-y :z-vals z)
	(plshades z 
		  nil
		  -1.0 1.0 -1.0 1.0 
		  shedge 2 0 0
		  'plfill-callback
		  t
		  'pltr2-callback 
		  pltr-data)
	(colorbar)
	(plcol0 1)
	(plbox "bcnst" 0.0 0 "bcnstv" 0.0 0)
	(plcol0 2)
	(plcont w 1 nx 1 ny clevel 'pltr2-callback pltr-data)
	(pllab "distance" "altitude" "Bogon density, with streamlines"))

      ; plot4
      (plspal0 "")
      (plspal1 "" t)
      (plscmap0n 3)
      (pladv 0)
      (plvpor 0.1 0.9 0.1 0.9)
      (plwind -1.0 1.0 -1.0 1.0)
      (plpsty 0)
      (with-pltr-data (pltr-data cgrid2-x cgrid2-y :z-vals z)
	(plshades z 
		  nil
		  -1.0 1.0 -1.0 1.0 
		  shedge 2 2 3 
		  'plfill-callback
		  t
		  'pltr2-callback
		  pltr-data))
      (colorbar)
      (plcol0 1)
      (plbox "bcnst" 0.0 0 "bcnstv" 0.0 0)
      (plcol0 2)
      (pllab "distance" "altitude" "Bogon density")
      
      ; plot5
      (plspal0 "cmap0_black_on_white.pal")
      (plspal1 "cmap1_gray.pal" t)
      (plscmap0n 3)
      (pladv 0)
      (plvpor 0.1 0.9 0.1 0.9)
      (plwind -1.0 1.0 -1.0 1.0)
      (plpsty 0)
      (dotimes (i nx)
	(let ((r (/ i (- nx 1.0))))
	  (dotimes (j ny)
	    (let ((th (* (/ (* 2.0 3.14159) (- ny 1.0)) j)))
	      (setf (aref cgrid2-x i j) (* r (cos th))
		    (aref cgrid2-y i j) (* r (sin th))
		    (aref z i j) (* (exp (* -1.0 r r))
				    (cos (* 5.0 3.14159 r))
				    (cos (* 5.0 th))))))))
      (multiple-value-bind (zmin zmax) (min-max z)
	(dotimes (i (1+ ns))
	  (setf (aref shedge i) (+ zmin (/ (* (- zmax zmin) i) ns)))
	  (setf (aref values-arr 0 i) (aref shedge i))))
      (with-pltr-data (pltr-data cgrid2-x cgrid2-y :z-vals z)
	(plshades z
		  nil
		  -1.0 1.0 -1.0 1.0 
		  shedge 2 0 0 
		  'plfill-callback
		  t
		  'pltr2-callback
		  pltr-data))
      (colorbar)
      (let* ((ppts 100)
	     (px (make-float-array ppts))
	     (py (make-float-array ppts)))
	(dotimes (i ppts)
	  (let ((th (* (/ (* 2.0 3.14159) (- ppts 2.0)) i)))
	    (setf (aref px i) (cos th)
		  (aref py i) (sin th))))
	(plcol0 1)
	(plline px py))
      (plcol0 2)
      (pllab "" "" "Tokamak Bogon Instability")))

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
