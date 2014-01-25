;;;;
;;;; Functions that are most closely related to the extended color class.
;;;;
;;;; This color class is used to setup PLplot's color map 1. This
;;;; color map will contain a large number of colors (typically
;;;; 128 or 256) that are (usually) continuous. This color map
;;;; is used for contour-plot to provide the :smooth fill.
;;;;
;;;; To be compatible with most of the plplot device drivers you
;;;; should probably limit yourself to 128 colors.
;;;;
;;;; hazen 10/06
;;;;

(in-package #:cl-plplot)


;; external

(defun new-extended-color-table (&key control-points (color-table-size 128))
  "Creates a new extended color table instance from a vector of control
   points. For example: #((0.0 0.0 0.0 0.0) (1.0 255 255 255)) will
   create a gray scale color table. See plscmap1l in the PLplot
   documentation for a more thorough explanation."
  (make-instance 'extended-color-table
		 :control-points (when (vectorp control-points)
				   control-points)
		 :color-table-size color-table-size))

(defun default-extended-color-table ()
  "Returns the default color table."
  (new-extended-color-table :control-points (vector #(0.0 0 0 255)
						    #(1.0 255 0 0))))

(defmethod set-color-table ((a-window window) (a-extended-color-table extended-color-table))
  "Sets the color table associated with a-window to a-color-table.
   Returns the old color table."
  (let ((old-color-table (extended-color-table a-window)))
    (setf (extended-color-table a-window) a-extended-color-table)
    old-color-table))

(defgeneric initialize-extended-color-table (a-extended-color-table))

(defmethod initialize-extended-color-table ((a-extended-color-table extended-color-table))
  "Initializes the extended color table in PLplot and sets it to match
   our local version of the extended color table."
  (let* ((control-points (control-points a-extended-color-table))
	 (number-control-points (length control-points))
	 (locations (make-float-vector number-control-points))
	 (red-values (make-float-vector number-control-points))
	 (green-values (make-float-vector number-control-points))
	 (blue-values (make-float-vector number-control-points))
	 (rev-values (make-array number-control-points :initial-element nil)))
    (dotimes (i number-control-points)
      (setf (aref locations i) (aref (aref control-points i) 0))
      (setf (aref red-values i) (/ (aref (aref control-points i) 1) 255.0))
      (setf (aref green-values i) (/ (aref (aref control-points i) 2) 255.0))
      (setf (aref blue-values i) (/ (aref (aref control-points i) 3) 255.0)))
    (plscmap1n (color-table-size a-extended-color-table))
    (plscmap1l t locations red-values green-values blue-values rev-values)))


; This variable is set in render-window to the current extended color table.

(defvar *current-extended-color-table*)


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
