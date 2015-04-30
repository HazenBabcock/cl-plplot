;;;;
;;;; PLplot example 19
;;;;
;;;; hazen 02/14
;;;;

(in-package :plplot-examples)

(defun x19-map-transform (x y)
  (let ((radius (- 90.0 y)))
    (values (* radius (cos (/ (* x pi) 180.0)))
	    (* radius (sin (/ (* x pi) 180.0))))))

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
    (let ((minx -170)
	  (maxx (+ -170 360)))
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

  ; An example using shapefiles.
  (plstransform nil nil)
  (pllsty 1)
  (let ((beachareas (vector 23 24))
	(nwoodlandareas 94)
	(woodlandareas (make-int-array 94))
	(shingleareas (vector 0 1 24 25 26 27 28 29 30 31 32 33 34 35 217 2424 2425 2426 2427 2428 2491 2577))
	(ncragareas 2024)
	(cragareas (make-int-array 2024))
	(majorroads (vector 33 48 71 83 89 90 101 102 111))
	(minx 265000)
	(maxx 270000)
	(miny 145000)
	(maxy 150000))
    (plscol0 0 255 255 255)     ; white
    (plscol0 1 0 0 0)           ; black
    (plscol0 2 255 200 0)       ; yelow for sand
    (plscol0 3 60 230 60)       ; green for woodland
    (plscol0 4 210 120 60 )     ; brown for contours
    (plscol0 5 150 0 0 )        ; red for major roads
    (plscol0 6 180 180 255 )    ; pale blue for water
    (plscol0 7 100 100 100 )    ; pale grey for shingle or boulders
    (plscol0 8 100 100 100 )    ; dark grey for custom polygons - generally crags


    (plcol0 1)
    (plenv minx maxx miny maxy 1 -1)
    (pllab "" "" "Martinhoe CP, Exmoor National Park, UK (shapelib only)")

    ; Beach
    (plcol0 2)
    (plmapfill nil "ss/ss64ne_Landform_Area" minx maxx miny maxy beachareas)

    ; Woodland
    (plcol0 3)
    (dotimes (i nwoodlandareas)
      (setf (aref woodlandareas i) (+ i 128)))
    (plmapfill nil "ss/ss64ne_Landform_Area" minx maxx miny maxy woodlandareas)

    ; Shingle or boulders
    (plcol0 7)
    (plmapfill nil "ss/ss64ne_Landform_Area" minx maxx miny maxy shingleareas)

    ; Crags
    (plcol0 8)
    (dotimes (i ncragareas)
      (setf (aref cragareas i) (+ i 325)))
    (plmapfill nil "ss/ss64ne_Landform_Area" minx maxx miny maxy cragareas)

    ; Draw contours, we need to separate contours from high/low coastline
    ; Draw_contours(pls, "ss/SS64_line", 433, 20, 4, 3, minx, maxx, miny, maxy );
    (plcol0 4)
    (plmapline nil "ss/ss64ne_Height_Contours" minx maxx miny maxy nil)

    ; Draw the sea and surface water
    (plwidth 0.0)
    (plcol0 6)
    (plmapfill nil "ss/ss64ne_Water_Area" minx maxx miny maxy nil)
    (plwidth 2.0)
    (plmapfill nil "ss/ss64ne_Water_Line" minx maxx miny maxy nil)

    ; Draw the roads, first with black and then thinner with colour to give an
    ; an outlined appearance
    (plwidth 5.0)
    (plcol0 1)
    (plmapline nil "ss/ss64ne_Road_Centreline" minx maxx miny maxy nil)
    (plwidth 3.0)
    (plcol0 0)
    (plmapline nil "ss/ss64ne_Road_Centreline" minx maxx miny maxy nil)
    (plcol0 5)
    (plmapline nil "ss/ss64ne_Road_Centreline" minx maxx miny maxy majorroads)

    ; Draw buildings
    (plwidth 1.0)
    (plcol0 1)
    (plmapfill nil "ss/ss64ne_Building_Area" minx maxx miny maxy nil)

    ; Labels
    (plsfci #x80000100)
    (plschr 0 0.8)
    (plmaptex nil "ss/ss64ne_General_Text" 1.0 0.0 0.5 "MARTINHOE CP" minx maxx miny maxy 202)
    (plschr 0 0.7)

    (plmaptex nil "ss/ss64ne_General_Text" 1.0 0.0 0.5
	      (concatenate 'string "Heale" (string #\newline) "Down")
	      minx maxx miny maxy 13)
    (plmaptex nil "ss/ss64ne_General_Text" 1.0 0.0 0.5
	      (concatenate 'string "South" (string #\newline) "Down")
	      minx maxx miny maxy 34)
    (plmaptex nil "ss/ss64ne_General_Text" 1.0 0.0 0.5
	      (concatenate 'string "Martinhoe" (string #\newline) "Common")
	      minx maxx miny maxy 42)
    (plmaptex nil "ss/ss64ne_General_Text" 1.0 0.0 0.5 "Woody Bay" minx maxx miny maxy 211)
    (plschr 0 0.6)
    (plmaptex nil "ss/ss64ne_General_Text" 1.0 0.0 0.5 "Mill Wood" minx maxx miny maxy 16)
    (plmaptex nil "ss/ss64ne_General_Text" 1.0 0.0 0.5 "Heale Wood" minx maxx miny maxy 17)
    (plmaptex nil "ss/ss64ne_General_Text" 1.0 0.0 1.0 "Bodley" minx maxx miny maxy 31)
    (plmaptex nil "ss/ss64ne_General_Text" 1.0 0.0 0.0 "Martinhoe" minx maxx miny maxy 37)
    (plmaptex nil "ss/ss64ne_General_Text" 1.0 0.0 0.5
	      (concatenate 'string "Woolhanger" (string #\newline) "Common")
	      minx maxx miny maxy 60)
    (plmaptex nil "ss/ss64ne_General_Text" 1.0 0.0 0.5
	      (concatenate 'string "West Ilkerton" (string #\newline) "Common")
	      minx maxx miny maxy 61)
    (plmaptex nil "ss/ss64ne_General_Text" 1.0 0.0 0.5
	      (concatenate 'string "Caffyns" (string #\newline) "Heanton" (string #\newline) "Down")
	      minx maxx miny maxy 62))

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
