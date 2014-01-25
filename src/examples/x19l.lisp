;;;;
;;;; PLplot example 19
;;;;
;;;; hazen 07/10
;;;;

(in-package :plplot-examples)

(defun example19 (&optional (dev default-dev))
  (plsdev dev)
  (labels ((map-transform (x y)
	     (let ((radius (- 90.0 y)))
	       (values (* radius (cos (/ (* x 3.14159) 180.0)))
		       (* radius (sin (/ (* x 3.14159) 180.0))))))
	   (map-fn (x y xt yt data)
	     (declare (ignore data))
	     (multiple-value-bind (v1 v2) (map-transform x y)
	       (setf (cffi:mem-aref xt :double) (coerce v1 'double-float)
		     (cffi:mem-aref yt :double) (coerce v2 'double-float))))
	   (mapform19 (n ax ay)
	     (dotimes (i n)
	       (multiple-value-bind (v1 v2) 
		   (map-transform (cffi:mem-aref ax :double i) 
				  (cffi:mem-aref ay :double i))
		 (setf (cffi:mem-aref ax :double i) (coerce v1 'double-float)
		       (cffi:mem-aref ay :double i) (coerce v2 'double-float)))))
	   (normalize-longitude (lon)
	     (if (and (>= lon -180.0) (<= lon 180.0))
		 lon
		 (let ((times (floor (/ (+ (abs lon) 180.0) 360.0))))
		   (if (< lon 0.0)
		       (+ lon (* 360.0 times))
		       (- lon (* 360.0 times))))))
	   (geolocation-labeler (axis value label length data)
	     (declare (ignore data))
	     (let* ((label-val value)
		    (direction-label (if (= axis 2)
					 (cond
					   ((> label-val 0.0) " N")
					   ((< label-val 0.0) " S")
					   (t "Eq"))
					 (progn
					   (setf label-val (normalize-longitude value))
					   (cond
					     ((> label-val 0.0) " E")
					     ((< label-val 0.0) " W")
					     (t ""))))))
	       (cffi:lisp-string-to-foreign (if (and (= axis 2) (= value 0.0))
						direction-label
						(format nil "~,0f~a" (abs label-val) direction-label))
					    label
					    length))))

    (plinit)
    (plcol0 1)
    (pl-set-label-fn #'geolocation-labeler)
    (plslabelfunc (pl-null-pointer))

    (let ((miny -70)
	  (maxy 80))
      ; Most of the world
      (let ((minx 190)
	    (maxx (+ 190 360)))
	(plenv minx maxx miny maxy 1 70)
	(plmap "usaglobe" minx maxx miny maxy))

      ; The Americas
      (let ((minx 190)
	    (maxx 340))
	(plcol0 1)
	(plenv minx maxx miny maxy 1 70)
	(plmap "usaglobe" minx maxx miny maxy))
	
      ; Polar, Northern hemisphere
      (let ((minx 0)
	    (maxx 360))
	(plenv -75.0 75.0 -75.0 75.0 1 -1)
	(pl-set-map-fn #'mapform19)
	(plmap "globe" minx maxx miny maxy)
	(pllsty 2)
	(plmeridians 10.0 10.0 0.0 360.0 -10.0 80.0))
	(pl-reset-map-fn)

      ; Polar, Northern hemisphere w/ PLplot-wide transform
      (let ((minx 0)
	    (maxx 360))
	(pl-set-transform-fn #'map-fn)
	(plstransform (pl-null-pointer))
	(pllsty 1)
	(plenv -75.0 75.0 -75.0 75.0 1 -1)
	(plmap "globe" minx maxx miny maxy)
	(pllsty 2)
	(plmeridians 10.0 10.0 0.0 360.0 -10.0 80.0)
	(plcol0 2)
	(plssym 0.0 2.0)
	(plpoin (vector -76.6125) (vector 39.2902778) 18)
	(plssym 0.0 1.0)
	(plptex -76.6125 43.0 0.0 0.0 0.0 "Baltimore, MD")
	(pl-reset-transform))))

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
