;;;;
;;;; An example of how to integrate cl-plplot with commonqt.
;;;;
;;;; This uses the PLplot "extqt" driver.
;;;;
;;;; hazen 07/14
;;;;

; For simplicity, just use the commonqt-plot package.
(in-package :commonqt-plot)


;; functions

(defun make-float-array (length &optional (initial-value 0.0))
  (make-array length :element-type 'float :initial-element initial-value))


;; classes

(defclass qplot ()
  ((plot :initform nil
	 :accessor plot))
  (:metaclass qt-class)
  (:qt-superclass "QMainWindow")
  (:override ("closeEvent" close-event))
  (:slots ("interactive(bool)" interactive)
	  ("plotCurves(bool)" plot-curves)
	  ("plotHistogram(bool)" plot-histogram)
	  ("quit(bool)" quit)))


;; methods

(defmethod close-event ((instance qplot) event)
  (declare (ignore event))
  (plend1))

(defmethod initialize-instance :after ((instance qplot) &key parent)
  (if parent
      (new instance parent)
      (new instance))

  ; UI setup
  (let ((curves-action (#_new QAction "Curves" instance))
	(histogram-action (#_new QAction "Histogram" instance))
	(interactive-action (#_new QAction "Interactive Selection" instance))
	(quit-action (#_new QAction "Quit" instance))
	(plot-menu (#_addMenu (#_menuBar instance) "Plot")))
    (#_addAction plot-menu curves-action)
    (#_addAction plot-menu histogram-action)
    (#_addAction plot-menu interactive-action)
    (#_addSeparator plot-menu)
    (#_addAction plot-menu quit-action)
    (connect curves-action "triggered(bool)" instance "plotCurves(bool)")
    (connect histogram-action "triggered(bool)" instance "plotHistogram(bool)")
    (connect interactive-action "triggered(bool)" instance "interactive(bool)")
    (connect quit-action "triggered(bool)" instance "quit(bool)"))

  ; PLplot setup
  (setf (plot instance) (#_new QtExtWidget 600 600 instance))
  (#_setWindowTitle instance "Qt Example")
  (#_setCentralWidget instance (plot instance))
  (#_QGlobalSpace::plsetqtdev (plot instance))
  (plsdev "extqt")
  (plinit)
  (#_resize instance 600 600))

(defmethod interactive ((instance qplot) bool)
  (declare (ignore bool))
  (multiple-value-bind (x y)
      (let ((c-x (cffi:foreign-alloc :double))
	    (c-y (cffi:foreign-alloc :double)))
	(unwind-protect
	     (progn
	       (#_captureMousePlotCoords (plot instance) c-x c-y)
	       (values (coerce (cffi:convert-from-foreign (cffi:mem-aref c-x :double) :double) 'double-float)
		       (coerce (cffi:convert-from-foreign (cffi:mem-aref c-y :double) :double) 'double-float)))
	  (progn
	    (cffi:foreign-free c-x)
	    (cffi:foreign-free c-y))))
    (let ((msg-box (#_new QMessageBox)))
      (#_setWindowTitle msg-box "Message")
      (#_setText msg-box (format nil "Selection: (~,2f ~,2f)" x y))
      (#_exec msg-box))))

(defmethod plot-curves ((instance qplot) bool)
  (declare (ignore bool))
  (#_clearWidget (plot instance))

  ; 1st plot
  (let ((x (make-float-array 360))
	(s (make-float-array 360))
	(c (make-float-array 360)))
    (dotimes (i 360)
      (setf (aref x i) i)
      (setf (aref s i) (sin (/ (* 3.14159 i) 180.0)))
      (setf (aref c i) (cos (/ (* 3.14159 i) 180.0))))
    (pladv 0)
    (plvpor 0.05 0.95 0.05 0.45)
    (plwind 0.0 360.0 -1.2 1.2)

    (plcol0 2)
    (plbox "bcnst" 0 0 "bcnst" 0 0)

    (plcol0 1)
    (plwidth 2)
    (plline x s)

    (plcol0 3)
    (plwidth 1)
    (pllsty 2)
    (plline x c)
    (pllsty 1)

    (plcol0 2)
    (plmtex "t" 1.0 0.5 0.5 "Sines"))

  ; 2nd plot
  (let ((x (make-float-array 201))
	(s (make-float-array 201))
	(c (make-float-array 201)))
    (dotimes (i 201)
      (setf (aref x i) (+ -1.0 (/ i 100.0)))
      (setf (aref s i) (* (aref x i) (aref x i)))
      (setf (aref c i) (* (aref s i) (aref x i))))
    (plvpor 0.05 0.95 0.55 0.95)
    (plwind -1.0 1.0 -1.0 1.0)
    
    (plcol0 2)
    (plbox "bcnst" 0 0 "bcnst" 0 0)

    (plcol0 1)
    (plwidth 2)
    (plline x s)

    (plcol0 3)
    (plwidth 1)
    (pllsty 2)
    (plline x c)
    (pllsty 1)

    (plcol0 2)
    (plmtex "t" 1.0 0.5 0.5 "Square & Cubic"))

  (#_update instance))

(defmethod plot-histogram ((instance qplot) bool)
  (declare (ignore bool))
  (#_clearWidget (plot instance))
  (labels ((plfbox (x0 y0)
	     (let ((x (vector x0 x0 (1+ x0) (1+ x0)))
		   (y (vector 0.0 y0 y0 0.0)))
	       (plfill x y)
	       (plcol0 1)
	       (pllsty 1)
	       (plline x y))))
    (let ((y0 (vector 5 15 12 24 28 30 20 8 12 3))
	  (pos (vector 0.0 0.25 0.5 0.75 1.0))
	  (red (vector 0.0 0.25 0.5 1.0 1.0))
	  (green (vector 1.0 0.5 0.5 0.5 1.0))
	  (blue (vector 1.0 1.0 0.5 0.25 0.0)))

      (pladv 0)
      (plvsta)
      
      (plcol0 2)

      (plwind 1980.0 1990.0 0.0 35.0)
      (plbox "bc" 1.0 0 "bcnv" 10.0 0)
      (plcol0 2)
      (pllab "Year" "Widget Sales (millions)" "#frPLplot Example 12")

      (plscmap1l 1 pos red green blue nil)
      
      (dotimes (i 10)
	(plcol1 (/ i 9.0))
	(plpsty 0)
	(plfbox (+ 1980 i) (aref y0 i))
	(plptex (+ 1980 i 0.5) (1+ (aref y0 i)) 1.0 0.0 0.5 (format nil "~,0f" (aref y0 i)))
	(plmtex "b" 1.0 (- (* (1+ i) 0.1) 0.05) 0.5 (format nil "~d" (+ 1980 i))))))
  (#_update instance))

(defmethod quit ((instance qplot) bool)
  (declare (ignore bool))
  (#_close instance))


;; run

(defun main ()
  (with-main-window (window (make-instance 'qplot))
    (plot-curves window nil)))


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
