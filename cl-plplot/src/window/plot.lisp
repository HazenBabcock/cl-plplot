;;;;
;;;; All plot object must support two functions.
;;;;
;;;; (1) plot-min-max returns the window size that would be ideal for the 
;;;;     plot object as the vector #(xmin xmax ymin ymax).
;;;; 
;;;; (2) render-plot draws the plot object in the current window. It
;;;;     should make no assumptions about the current foreground color,
;;;;     pen size, etc...
;;;;
;;;; Here we provide a mechanism whereby the user can create their own
;;;; plot objects that will be drawn in the window just as if they were
;;;; one of the built in plot objects (x-y-plot, bar-graph, contour-plot).
;;;;
;;;; hazen 10/06
;;;;

(in-package #:cl-plplot)

(defgeneric plot-min-max (plot))

(defmethod plot-min-max ((a-plot plot))
  (funcall (min-max-function a-plot)))

(defgeneric render-plot (plot &optional parameters))

(defmethod render-plot ((a-plot plot) &optional parameters)
  (funcall (render-function a-plot) parameters))

(defun new-custom-plot-object (min-max-function render-function)
  "Allows the user to create their own custom plot object.
    min-max-function must be either nil or a function of no arguments
      that returns the vector #(xmin xmax ymin ymax) that specifies the
      ideal window size for this object.
    render-function must be a function of one argument that specifies how
      to render the plot object. Generally the rendering will be done with 
      a bunch of calls to functions in the cl-plplot-system module. The
      current plot number will be passed to this function."
  (make-instance 'plot
		 :min-max-function (if min-max-function
				       min-max-function
				       #'(lambda () (vector 0.0 1.0 0.0 1.0)))
		 :render-function render-function))

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
