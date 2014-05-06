;;;;
;;;; PLplot example 29.
;;;;
;;;; Fixme: This example may have a problem with floating point 
;;;; exceptions. I was unsuccesful in my attempts to figure
;;;; out why as the numbers that it passes to the PLplot
;;;; library are identical with those passed by the C
;;;; version of this example.
;;;;
;;;; hazen 07/10
;;;;

(in-package :plplot-examples)

(defun example29 (&optional (dev default-dev))
  (plsdev dev)
  (labels ((plot1 ()
	     (let* ((npts 73)
		    (xmin 0.0)
		    (xmax (* 60.0 60.0 24.0))
		    (ymin 10.0)
		    (ymax 20.0)
		    (x (make-float-array npts))
		    (y (make-float-array npts))
		    (xerr1 (make-float-array npts))
		    (xerr2 (make-float-array npts))
		    (yerr1 (make-float-array npts))
		    (yerr2 (make-float-array npts)))
	       (dotimes (i npts)
		 (setf (aref x i) (* xmax (/ i npts))
		       (aref y i) (- 15.0 (* 5.0 (cos (* 2 3.14159 (/ i npts)))))
		       (aref xerr1 i) (- (aref x i) (* 60 5))
		       (aref xerr2 i) (+ (aref x i) (* 60 5))
		       (aref yerr1 i) (- (aref y i) 0.1)
		       (aref yerr2 i) (+ (aref y i) 0.1)))
	       (pladv 0)
	       (plsmaj 0.0 0.5)
	       (plsmin 0.0 0.5)
	       (plvsta)
	       (plwind xmin xmax ymin ymax)
	       (plcol0 1)
	       (pltimefmt "%H:%M")
	       (plbox "bcnstd" (* 3.0 60 60) 3
		      "bcnstv" 1 5)
	       (plcol0 3)
	       (pllab "Time (hours:mins)"
		      "Temperature (degC)"
		      "@frPLplot Example 29 - Daily temperature")
	       (plcol0 4)
	       (plline x y)
	       (plcol0 2)
	       (plerrx xerr1 xerr2 y)
	       (plcol0 3)
	       (plerry x yerr1 yerr2)
	       (plsmin 0.0 1.0)
	       (plsmaj 0.0 1.0)))
	   (plot2 ()
	     (let* ((lat 51.5)
		    (npts 365)
		    (xmin 0.0)
		    (xmax (* npts 60.0 60.0 24.0))
		    (ymin 0.0)
		    (ymax 24.0)
		    (x (make-float-array npts))
		    (y (make-float-array npts)))
	       (dotimes (j npts)
		 (let* ((p (asin (* 0.39795 (cos (+ 0.2163108 (* 2 (atan (* 0.9671396 (tan (* 0.00860 (- j 186)))))))))))
			(d (- 24.0 (* (/ 24.0 3.14159)
				      (acos (/ (+ (sin (/ (* 0.8333 3.14159) 180.0)) (* (sin (/ (* lat 3.14159) 180.0)) (sin p)))
					       (* (cos (/ (* lat 3.14159) 180.0)) (cos p))))))))
		   (setf (aref x j) (* j 60.0 60.0 24.0)
			 (aref y j) d)))
	       (plcol0 1)
	       (pltimefmt "%b %d")
	       (plprec 1 1)
	       (plenv xmin xmax ymin ymax 0 40)
	       (plcol0 3)
	       (pllab "Date" "Hours of daylight" "@frPLplot Example 29 - Hours of daylight at 51.5N")
	       (plcol0 4)
	       (plline x y)
	       (plprec 0 0)))
	   (plot3 ()
	     (let* ((tstart (plctime 2005 11 01 0 0 0))
		    (npts 62)
		    (xmin tstart)
		    (xmax (+ xmin (* npts 60.0 60.0 24.0)))
		    (ymin 0.0)
		    (ymax 5.0)
		    (x (make-float-array npts))
		    (y (make-float-array npts)))
	       (dotimes (i npts)
		 (setf (aref x i) (+ xmin (* i 60.0 60.0 24.0))
		       (aref y i) (+ 1.0 (sin (* 2.0 3.14159 (/ i 7.0)))
				     (exp (/ (if (< i (- npts i)) i (- npts i)) 31.0)))))
	       (pladv 0)
	       (plvsta)
	       (plwind xmin xmax ymin ymax)
	       (plcol0 1)
	       (pltimefmt "%F")
	       (plbox "bcnstd" (* 14.0 24.0 60.0 60.0) 14
		      "bcnstv" 1 4)
	       (plcol0 3)
	       (pllab "Date" "Hours of television watched" "@frPLplot Example 29 - Hours of television watched in Dec 2005 / Jan 2006")
	       (plcol0 4)
	       (plssym 0.0 0.5)
	       (plpoin x y 2)
	       (plline x y)))
	   (plot4 ()
	     (let* ((scale 365.242198781)
		    (offset1 -678940)
		    (offset2 -0.3641639)
		    (if-TAI-time-format)
		    (npts)
		    (title-suffix)
		    (time-format)
		    (xlabel-step)
		    (xtitle)
		    (xmin)
		    (xmax)
		    (ymin)
		    (ymax))
	       (plconfigtime scale offset1 offset2 #x0 nil 0 0 0 0 0 0)
	       (dotimes (kind 7)
		 (cond
		   ((= kind 0)
		    (setf xmin (plctime 1950 0 2 0 0 0)
			  xmax (plctime 2020 0 2 0 0 0)
			  npts (+ (* 70 12) 1)
			  ymin 0.0
			  ymax 36.0
			  time-format "%Y%"
			  if-TAI-time-format t
			  title-suffix "from 1950 to 2020"
			  xtitle "Year"
			  xlabel-step 10))
		   ((or (= kind 1) (= kind 2))
		    (setf xmin (plctime 1961 7 1 0 0 (- 1.64757 0.2))
			  xmax (plctime 1961 7 1 0 0 (+ 1.64757 0.2))
			  npts 1001
			  ymin 1.625
			  ymax 1.725
			  time-format "%S%2%"
			  title-suffix "near 1961-08-01 (TAI)"
			  xlabel-step (/ 0.05 (* scale 86400.0))
			  if-TAI-time-format (if (= kind 1) t nil)
			  xtitle (if (= kind 1) "Seconds (TAI)" "Seconds (TAI) labelled with corresponding UTC")))
		   ((or (= kind 3) (= kind 4))
		    (setf xmin (plctime 1963 10 1 0 0 (- 2.6972788 0.2))
			  xmax (plctime 1963 10 1 0 0 (+ 2.6972788 0.2))
			  npts 1001
			  ymin 2.55
			  ymax 2.75
			  time-format "%S%2%"
			  title-suffix "near 1963-11-01 (TAI)"
			  xlabel-step (/ 0.05 (* scale 86400.0))
			  if-TAI-time-format (if (= kind 3) t nil)
			  xtitle (if (= kind 3) "Seconds (TAI)" "Seconds (TAI) labelled with corresponding UTC")))
		   ((or (= kind 5) (= kind 6))
		    (setf xmin (plctime 2009 0 1 0 0 (- 34.0 5.0))
			  xmax (plctime 2009 0 1 0 0 (+ 34.0 5.0))
			  npts 1001
			  ymin 32.5
			  ymax 34.5
			  time-format "%S%2%"
			  title-suffix "near 2009-01-01 (TAI)"
			  xlabel-step (/ 1.0 (* scale 86400.0))
			  if-TAI-time-format (if (= kind 5) t nil)
			  xtitle (if (= kind 5) "Seconds (TAI)" "Seconds (TAI) labelled with corresponding UTC")))
		   (t nil))
		 (let ((x (make-float-array npts))
		       (y (make-float-array npts)))
		   (dotimes (i npts)
		     (setf (aref x i) (+ xmin (* i (/ (- xmax xmin) (1- npts)))))
		     (plconfigtime scale offset1 offset2 #x0 nil 0 0 0 0 0 0)
		     (multiple-value-bind (tai-year tai-month tai-day tai-hour tai-min tai-sec)
			 (plbtime (aref x i)))
		     (plconfigtime scale offset1 offset2 #x2 nil 0 0 0 0 0 0)
		     (multiple-value-bind (utc-year utc-month utc-day utc-hour utc-min utc-sec)
			 (plbtime (aref x i))
		       (plconfigtime scale offset1 offset2 #x0 nil 0 0 0 0 0 0)
		       (let ((utc (plctime utc-year utc-month utc-day utc-hour utc-min utc-sec)))
			 (setf (aref y i) (* (- (aref x i) utc) scale 86400)))))
		   (pladv 0)
		   (plvsta)
		   (plwind xmin xmax ymin ymax)
		   (plcol0 1)
		   (plconfigtime scale offset1 offset2 (if if-TAI-time-format #x0 #x2) nil 0 0 0 0 0 0)
		   (pltimefmt time-format)

		   ; Older versions of PLplot will throw a floating point exception here.
		   (handler-case
		       (plbox "bcnstd" xlabel-step 0
			      "bcnstv" 0.0 0)
		     (error () (format t "plbox problem~%")))

		   (plcol0 3)
		   (pllab xtitle
			  "TAI-UTC (sec)"
			  (format nil "@frPLplot Example 29 - TAI-UTC ~A" title-suffix))
		   (plcol0 4)
		   (plline x y))))))
    (plinit)
    (plsesc #\@)
    (plot1)
    (plot2)
    (plot3)
    (plot4)
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
