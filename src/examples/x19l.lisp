;;;;
;;;; PLplot example 19
;;;;
;;;; hazen 02/14
;;;;

(in-package :plplot-examples)

(defun x19-map-transform (x y)
  (let ((radius (- 90.0 y)))
    (values (* radius (cos (/ (* x 3.14159) 180.0)))
	    (* radius (sin (/ (* x 3.14159) 180.0))))))

(defun x19-normalize-longitude (lon)
  (if (and (>= lon -180.0) (<= lon 180.0))
      lon
      (let ((times (floor (/ (+ (abs lon) 180.0) 360.0))))
	(if (< lon 0.0)
	    (+ lon (* 360.0 times))
	    (- lon (* 360.0 times))))))

(cffi:defcallback x19-map-fn :void ((x plflt) (y plflt) (tx *plflt) (ty *plflt) (pltr-data :pointer))
  (declare (ignore pltr-data))
  (multiple-value-bind (v1 v2) (x19-map-transform x y)
    (setf (cffi:mem-aref tx 'plflt) (cffi:convert-to-foreign v1 'plflt)
	  (cffi:mem-aref ty 'plflt) (cffi:convert-to-foreign v2 'plflt))))

(cffi:defcallback x19-mapform19 :void ((n plint) (ax *plflt) (ay *plflt))
  (dotimes (i n)
    (multiple-value-bind (v1 v2) 
	(x19-map-transform (cffi:mem-aref ax 'plflt i) 
			   (cffi:mem-aref ay 'plflt i))
      (setf (cffi:mem-aref ax 'plflt i) (cffi:convert-to-foreign v1 'plflt)
	    (cffi:mem-aref ay 'plflt i) (cffi:convert-to-foreign v2 'plflt)))))

(cffi:defcallback x19-geolocation-labeler :void ((axis plint) (value plflt) (label-text :pointer) (length plint) (label-data pldata))
  (declare (ignore label-data))
  (let* ((label-val value)
	 (direction-label (if (= axis 2)
			      (cond
				((> label-val 0.0) " N")
				((< label-val 0.0) " S")
				(t "Eq"))
			      (progn
				(setf label-val (x19-normalize-longitude value))
				(cond
				  ((> label-val 0.0) " E")
				  ((< label-val 0.0) " W")
				  (t ""))))))
    (cffi:lisp-string-to-foreign (if (and (= axis 2) (= value 0.0))
				     direction-label
				     (format nil "~,0f~a" (abs label-val) direction-label))
				 label-text
				 length)))


(defun example19 (&optional (dev default-dev))
  (plsdev dev)
  (plinit)
  (plcol0 1)
  (plslabelfunc 'x19-geolocation-labeler nil)
  
  (let ((miny -70)
	(maxy 80))
    ; Most of the world
    (let ((minx 190)
	  (maxx (+ 190 360)))
      (plenv minx maxx miny maxy 1 70)
      (plmap nil "usaglobe" minx maxx miny maxy))

    ; The Americas
    (let ((minx 190)
	  (maxx 340))
      (plcol0 1)
      (plenv minx maxx miny maxy 1 70)
      (plmap nil "usaglobe" minx maxx miny maxy))
    
    ; Polar, Northern hemisphere
    (let ((minx 0)
	  (maxx 360))
      (plenv -75.0 75.0 -75.0 75.0 1 -1)
      (plmap 'x19-mapform19 "globe" minx maxx miny maxy)
      (pllsty 2)
      (plmeridians 'x19-mapform19 10.0 10.0 0.0 360.0 -10.0 80.0))

    ; Polar, Northern hemisphere w/ PLplot-wide transform
    (let ((minx 0)
	  (maxx 360))
      (plstransform 'x19-map-fn nil)
      (pllsty 1)
      (plenv -75.0 75.0 -75.0 75.0 1 -1)
      (plmap nil "globe" minx maxx miny maxy)
      (pllsty 2)
      (plmeridians nil 10.0 10.0 0.0 360.0 -10.0 80.0)
      (plcol0 2)
      (plssym 0.0 2.0)
      (plpoin (vector -76.6125) (vector 39.2902778) 18)
      (plssym 0.0 1.0)
      (plptex -76.6125 43.0 0.0 0.0 0.0 "Baltimore, MD")))
  
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
