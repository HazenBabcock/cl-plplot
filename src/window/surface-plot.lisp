;;;;
;;;; Functions that are most closely related to the surface-plot class.
;;;;
;;;; hazen 2/07
;;;;

(in-package #:cl-plplot)

(defun new-surface-plot (data-x data-y data-z &key contour-levels (copy t) (line-width 1) (line-style 1) 
			 (line-color *foreground-color*) light-source surface-options)
   "Creates a new 3D (solid surface) plot.
    data-x specifies the x values of the points in data-z. If data-x is nil then data-z will 
       be plotted against its row index in x.
    data-y specifies the y avlues of the points in data-z. If data-y is nil then data-z will 
       be plotted against its column index in y.
    data-z is a 2D array of z values for the plot.
    contour-levels specifies the levels at which to draw contours, if desired. If this is
       not specified, default values are chosen.
    If copy is true then copies of data-x, data-y and data-z will be made, otherwise reference
       will be kept to the original vectors.
    line-width should be an integer line width, or zero if no line is desired
    line-style specifies what style line to draw (if a line is drawn), this should be a number
       between 1 and 8.
    line-color is the color to use for the lines in the plot.
    light-source is a 3 element vector #(x y z) specifying the location of the light source
       that will illuminate the plot surface.
    surface-options is list containing zero or more of the following symbols:
       :faceted - a network of lines is drawing connecting the points that make up the surface.
       :base-contours - a contour plot is also drawn in the base xy plane.
       :surface-contours - contour levels are drawn on the surface of the plot.
       :curtain - a curtain between the borders of the surface and the base xy plane.
       :magnitude-coloring - the surface is colored according to the z value of the plot. If
          this is not set then surface is colored according the intensity of the reflected
          light from light source."
   (when (3D-plot-check-lengths data-x data-y data-z)
     (make-instance 'surface-plot
		    :data-x (copy-vector (if data-x data-x (make-index-vector (array-dimension data-z 0))) copy)
		    :data-y (copy-vector (if data-y data-y (make-index-vector (array-dimension data-z 1))) copy)
		    :data-z (copy-matrix data-z copy)
		    :contour-levels (check-contour-levels data-z (copy-vector contour-levels copy))
		    :line-width line-width
		    :line-style line-style
		    :line-color line-color
		    :light-source (when light-source (copy-vector light-source copy))
		    :surface-options surface-options)))

(def-edit-method surface-plot (line-width line-style line-color light-source surface-options)
  "edit-surface-plot, Edits the visual properties of a solid surface plot
    Set the line width with :line-width (integer, 0 means no line).
    Set the line style with :line-style (integer between 1 and 8).
    Set the line color with :line-color symbol.
    Move the light-source to a new position with :light-source #(x y z).
    Change the surface-options with :surface-options to a list including zero or more of
      :faceted, :base-contours, :surface-contours, :curtain and :magnitude-coloring")


;; draw a plot

(defmethod render-plot ((a-plot surface-plot) &optional (default-symbol 0))
  "Renders a surface plot in the current window."
  (declare (ignore default-symbol))
  (set-foreground-color (line-color a-plot))
  (when (> (line-width a-plot) 0)
    (when (range-check (line-style a-plot) 1 8)
      (pllsty (line-style a-plot)))
    (plwidth (line-width a-plot)))
  (let ((parsed-options
	 (if (listp (surface-options a-plot))
	     (apply #'+
		    (mapcar #'(lambda (x)
				(cond
				  ((equal x :faceted) 128)
				  ((equal x :base-contours) 8)
				  ((equal x :surface-contours) 32)
				  ((equal x :curtain) 64)
				  ((equal x :magnitude-coloring) 4)
				  (t
				   (format t "Unknown surface options : ~A~%" x)
				   0)))
			    (surface-options a-plot)))
	     0)))
    (when (vectorp (light-source a-plot))
      (let ((source (light-source a-plot)))
	(pllightsource (aref source 0) (aref source 1) (aref source 2))))
    (plsurf3d (data-x a-plot) (data-y a-plot) (data-z a-plot) parsed-options (contour-levels a-plot))))

;;;;
;;;; Copyright (c) 2007 Hazen P. Babcock
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
