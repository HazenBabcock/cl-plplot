;;;;
;;;; PLplot example 28
;;;;
;;;; hazen 07/10
;;;;

(in-package :plplot-examples)

(defun example28 (&optional (dev default-dev))
  (plsdev dev)
  (let* ((xpts 2)
	 (ypts 2)
	 (nrevolution 16)
	 (nrotation 8)
	 (nshear 8)
	 (x (make-float-array xpts))
	 (y (make-float-array ypts))
	 (z (make-float-array (list xpts ypts)))
	 (xmin 0.0)
	 (xmax 1.0)
	 (xmid (* 0.5 (+ xmax xmin)))
	 (xrange (- xmax xmin))
	 (ymin 0.0)
	 (ymax 1.0)
	 (ymid (* 0.5 (+ ymax ymin)))
	 (yrange (- ymax ymin))
	 (zmin 0.0)
	 (zmax 1.0)
	 (zmid (* 0.5 (+ zmax zmin)))
	 (zrange (- zmax zmin))
	 (ysmin (+ ymin (* 0.1 yrange)))
	 (ysmax (- ymax (* 0.1 yrange)))
	 (ysrange (- ysmax ysmin))
	 (dysrot (/ ysrange (1- nrotation)))
	 (dysshear (/ ysrange (1- nshear)))
	 (zsmin (+ zmin (* 0.1 zrange)))
	 (zsmax (- zmax (* 0.1 zrange)))
	 (zsrange (- zsmax zsmin))
	 (dzsrot (/ zsrange (1- nrotation)))
	 (dzsshear (/ zsrange (1- nshear))))
    (dotimes (i xpts)
      (setf (aref x i) (+ xmin (/ (* i (- xmax xmin)) (1- xpts)))))
    (dotimes (j ypts)
      (setf (aref y j) (+ ymin (/ (* j (- ymax ymin)) (1- ypts)))))
    (dotimes (i xpts)
      (dotimes (j ypts)
	(setf (aref z i j) 0.0)))
    (plinit)

    ; page 1
    (pladv 0)
    (plvpor -0.15 1.15 -0.05 1.05)
    (plwind -1.2 1.2 -0.8 1.5)
    (plw3d 1.0 1.0 1.0 xmin xmax ymin ymax zmin zmax 20.0 45.0)
    (plcol0 2)
    (plbox3 "b" "" (- xmax xmin) 0
	    "b" "" (- ymax ymin) 0
	    "bcd" "" (- zmax zmin) 0)
    (plschr 0.0 1.0)
    (dotimes (i nrevolution)
      (let* ((omega (* 2.0 pi (/ i nrevolution)))
	     (sin-omega (sin omega))
	     (cos-omega (cos omega)))
	(plptex3 xmid ymid zmin
		 (* 0.5 xrange cos-omega) 
		 (* 0.5 yrange sin-omega)
		 0.0
		 (* -0.5 xrange sin-omega)
		 (* 0.5 yrange cos-omega)
		 0.0
		 0.0 "  revolution")
	(plptex3 xmax ymid zmid
		 0.0
		 (* -0.5 yrange cos-omega)
		 (* 0.5 zrange sin-omega)
		 0.0
		 (* 0.5 yrange sin-omega)
		 (* 0.5 zrange cos-omega)
		 0.0 "  revolution")
	(plptex3 xmid ymax zmid
		 (* 0.5 xrange cos-omega)
		 0.0
		 (* 0.5 zrange sin-omega)
		 (* -0.5 xrange sin-omega)
		 0.0
		 (* 0.5 zrange cos-omega)
		 0.0 "  revolution")))
    (plmesh x y z 3)

    ; page 2
    (pladv 0)
    (plvpor -0.15 1.15 -0.05 1.05)
    (plwind -1.2 1.2 -0.8 1.5)
    (plw3d 1.0 1.0 1.0 xmin xmax ymin ymax zmin zmax 20.0 45.0)
    (plcol0 2)
    (plbox3 "b" "" (- xmax xmin) 0
	    "b" "" (- ymax ymin) 0
	    "bcd" "" (- zmax zmin) 0)
    (plschr 0.0 1.0)
    (dotimes (i nrotation)
      (let* ((omega (* 2.0 pi (/ i nrotation)))
	     (sin-omega (sin omega))
	     (cos-omega (cos omega)))
	(plptex3 xmid ymax (- zsmax (* dzsrot i))
		 1.0
		 0.0
		 0.0
		 0.0
		 (* 0.5 yrange sin-omega)
		 (* 0.5 zrange cos-omega)
		 0.5 "rotation for y = y#dmax#u")
	(plptex3 xmax ymid (- zsmax (* dzsrot i))
		 0.0
		 -1.0
		 0.0
		 (* 0.5 xrange sin-omega)
		 0.0
		 (* 0.5 zrange cos-omega)
		 0.5 "rotation for x = x#dmax#u")
	(plptex3 xmid (- ysmax (* dysrot i)) zmin
		 1.0
		 0.0
		 0.0
		 0.0
		 (* 0.5 yrange cos-omega)
		 (* 0.5 zrange sin-omega)
		 0.5 "rotation for z = z#dmin#u")))
    (plmesh x y z 3)

    ; page 3
    (pladv 0)
    (plvpor -0.15 1.15 -0.05 1.05)
    (plwind -1.2 1.2 -0.8 1.5)
    (plw3d 1.0 1.0 1.0 xmin xmax ymin ymax zmin zmax 20.0 45.0)
    (plcol0 2)
    (plbox3 "b" "" (- xmax xmin) 0
	    "b" "" (- ymax ymin) 0
	    "bcd" "" (- zmax zmin) 0)
    (plschr 0.0 1.0)
    (dotimes (i nshear)
      (let* ((omega (+ 0.05 (* 2.0 pi (/ i nshear))))
	     (sin-omega (sin omega))
	     (cos-omega (cos omega)))
	(plptex3 xmid ymax (- zsmax (* dzsshear i))
		 1.0
		 0.0
		 0.0
		 (* 0.5 xrange sin-omega)
		 0.0
		 (* 0.5 zrange cos-omega)
		 0.5 "shear for y = y#dmax#u")
	(plptex3 xmax ymid (- zsmax (* dzsshear i))
		 0.0
		 -1.0
		 0.0
		 0.0
		 (* -0.5 yrange sin-omega)
		 (* 0.5 zrange cos-omega)
		 0.5 "shear for x = x#dmax#u")
	(plptex3 xmid (- ysmax (* dysshear i)) zmin
		 1.0
		 0.0
		 0.0
		 (* 0.5 xrange sin-omega)
		 (* 0.5 yrange cos-omega)
		 0.0
		 0.5 "shear for z = z#dmin#u")))
    (plmesh x y z 3)

    ; page 4
    (pladv 0)
    (plvpor -0.15 1.15 -0.05 1.05)
    (plwind -1.2 1.2 -0.8 1.5)
    (plw3d 1.0 1.0 1.0 xmin xmax ymin ymax zmin zmax 40.0 -30.0)
    (plcol0 2)
    (plbox3 "b" "" (- xmax xmin) 0
	    "b" "" (- ymax ymin) 0
	    "bcd" "" (- zmax zmin) 0)
    (plschr 0.0 1.2)
    (let* ((pstring "The future of our civilization depends on software freedom.")
	   (domega (/ (* 2.0 pi) (length pstring)))
	   (omega 0.0)
	   (radius 0.5)
	   (pitch (/ 1.0 (* 2.0 pi))))
      (dotimes (i (length pstring))
	(let ((sin-omega (sin omega))
	      (cos-omega (cos omega)))
	  (plptex3 (+ xmid (* radius sin-omega))
		   (- ymid (* radius cos-omega))
		   (+ zmin (* pitch omega))
		   (* radius cos-omega)
		   (* radius sin-omega)
		   pitch
		   0.0
		   0.0
		   1.0
		   0.5
		   (subseq pstring i (1+ i)))
	  (incf omega domega))))
    (plmesh x y z 3)

    ; page 5
    (pladv 0)
    (plvpor -0.15 1.15 -0.05 1.05)
    (plwind -1.2 1.2 -0.8 1.5)
    (plw3d 1.0 1.0 1.0 xmin xmax ymin ymax zmin zmax 20.0 45.0)
    (plcol0 2)
    (plbox3 "b" "" (- xmax xmin) 0
	    "b" "" (- ymax ymin) 0
	    "bcd" "" (- zmax zmin) 0)
    (plschr 0.0 1.0)
    (plmtex3 "xp" 3.0 0.5 0.5 "Arbitrarily displaced")
    (plmtex3 "xp" 4.5 0.5 0.5 "primary X-axis label")
    (plmtex3 "xs" -2.5 0.5 0.5 "Arbitrarily displaced")
    (plmtex3 "xs" -1.0 0.5 0.5 "secondary X-axis label")
    (plmtex3 "yp" 3.0 0.5 0.5 "Arbitrarily displaced")
    (plmtex3 "yp" 4.5 0.5 0.5 "primary Y-axis label")
    (plmtex3 "ys" -2.5 0.5 0.5 "Arbitrarily displaced")
    (plmtex3 "ys" -1.0 0.5 0.5 "secondary Y-axis label")
    (plmtex3 "zp" 4.5 0.5 0.5 "Arbitrarily displaced")
    (plmtex3 "zp" 3.0 0.5 0.5 "primary Z-axis label")
    (plmtex3 "zs" -2.5 0.5 0.5 "Arbitrarily displaced")
    (plmtex3 "zs" -1.0 0.5 0.5 "secondary Z-axis label")
    (plmesh x y z 3))

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
