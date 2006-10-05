;;;;
;;;; Copyright (c) 2006 Hazen P. Babcock
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
;;;;
;;;; Functions that are most closely related to the contour-plot class.
;;;;
;;;; hazen 10/06
;;;;

(in-package #:cl-plplot)

(defun check-contour-levels (data contour-levels)
  "Creates default contour levels based on data in the event that the contour
   levels have not already been specified."
  (if contour-levels
      contour-levels
      (let ((min (aref data 0 0))
	    (max (aref data 0 0))
	    (default-levels (make-float-vector 10)))
	(dotimes (i (array-dimension data 0))
	  (dotimes (j (array-dimension data 1))
	    (when (> (aref data i j) max)
	      (setf max (aref data i j)))
	    (when (< (aref data i j) min)
	      (setf min (aref data i j)))))
	(let* ((range (- max min))
	       (increment (/ range 8)))
	  (dotimes (i 10)
	    (setf (aref default-levels i) min)
	    (incf min increment)))
	default-levels)))

(defun check-x-min (data data-x-min)
  "Chooses a default x minimum in the event that this is not specfied."
  (declare (ignore data))
  (if data-x-min
      data-x-min
      1))

(defun check-y-min (data data-y-min)
  "Chooses a default y minimum in the event that this is not specified."
  (declare (ignore data))
  (if data-y-min
      data-y-min
      1))

(defun check-x-max (data data-x-max)
  "Chooses a default x maximum in the event that this is not specified."
  (if data-x-max
      data-x-max
      (array-dimension data 0)))

(defun check-y-max (data data-y-max)
  "Chooses a default y maximum in the event that this is not specified."
  (if data-y-max
      data-y-max
      (array-dimension data 1)))

(defun new-contour-plot (data &key contour-levels (line-width 1) (fill-type :none) contour-labels data-x-min data-x-max data-y-min data-y-max (copy t))
   "Creates a new contour plot.
    data is a 2D array of z values.
    contour-levels is a 1D vector of floats specifying the levels at which the contours
       should appear. If this is nil, then default contours are created based on the
       minimum and maximum values in data.
    line-width is an integer specifying what size line to use for the contours (or
       zero for no line).
    fill-type is one of :none (contours only), :block (a single color between each contour)
       or :smooth (color varies continously between contours).
    contour-labels...
    data-x-min & data-y-min specify the 0,0 point of data in real coordinates.
    data-x-max & data-y-max specify the (max,max) point of data in real coordinates."
   (when (= (array-rank data) 2)
     (make-instance 'contour-plot
		    :data (copy-matrix data copy)
		    :line-width line-width
		    :fill-type fill-type
		    :contour-levels (check-contour-levels data (copy-vector contour-levels copy))
		    :contour-labels contour-labels
		    :data-x-min (check-x-min data data-x-min)
		    :data-x-max (check-x-max data data-x-max)
		    :data-y-min (check-y-min data data-y-min)
		    :data-y-max (check-y-max data data-y-max))))

(def-edit-method contour-plot (line-width fill-type contour-labels data-x-min data-x-max data-y-min data-y-max)
  "edit-contour-plot, Edits the visual properties of a plot
    Set the line width with :line-width (integer, 0 means no line).
    Set the fill-type with :fill-type (:none :block :smooth).
    Set the contour-labels...
    Set data-x-min with :data-x-min.
    Set data-x-max with :data-x-max.
    Set data-y-min with :data-y-min.
    Set data-y-max with :data-y-max.")

(defmethod plot-min-max ((a-plot contour-plot))
  "Returns the minimum and maximum values in a contour plot as a 4 element vector."
  (vector (data-x-min a-plot) (data-x-max a-plot)
	  (data-y-min a-plot) (data-y-max a-plot)))


;; draw a contour plot

(defmethod render-plot ((a-plot contour-plot) &optional ignored)
  "Renders a x-y plot in the current window."
  (declare (ignore ignored)))


