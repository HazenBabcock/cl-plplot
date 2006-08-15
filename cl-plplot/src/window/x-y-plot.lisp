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
;;;; Functions that are most closely related to the x-y-plot class.
;;;;
;;;; hazen 6/06
;;;;

(in-package #:cl-plplot)

(defun new-x-y-plot (x y &key (copy t) (line-width 1) (line-style 1) (symbol-size 0.0) symbol-type (color *foreground-color*) x-error y-error)
   "Creates a new x-y plot.
    If x is nil then y will be plotted against its index.
    If copy is true then copies of x,y,x-error and y-error will be made, otherwise references
       will be kept to the original vectors.
    line-width should be an integer line width, or zero if no line is desired
    line-style specifies what style line to draw (if a line is drawn), this should be a number
       between 1 and 8.
    symbol-size specified how big to draw the symbols (1.0 is standard size).
       If it is zero the symbols are not drawn.
    symbol-type should be set to a number (that specifies a symbol) if you want specific
       types of symbols, otherwise default symbol types are used.
    color is the color to use when plotting the lines and symbols, it should be a symbol
       that specifies a color in the current color table. If it is not specified then the 
       current foreground color will be used.
    x-error should be a vector of the same length as x that contains the size of the error
       bars in x.
    y-error is for error bars in y."
   (when (check-lengths x y x-error y-error)
     (labels ((copy-if-copy (vector)
		(if copy
		    (copy-seq vector)
		    vector)))
       (make-instance 'x-y-plot
		      :data-x (copy-if-copy (if x x (make-index-vector y)))
		      :data-y (copy-if-copy y)
		      :line-width line-width
		      :line-style line-style
		      :symbol-size symbol-size
		      :symbol-type symbol-type
		      :color color
		      :x-error (copy-if-copy x-error)
		      :y-error (copy-if-copy y-error)))))

(def-edit-method x-y-plot (line-width line-style symbol-size symbol-type color)
  "edit-x-y-plot, Edits the visual properties of a plot
    Set the line width with :line-width (integer, 0 means no line).
    Set the line style with :line-style (integer between 1 and 8).
    Set the symbol size with :symbol-size (1.0 is the defaul size, 0.0 means no symbols).
    Set the symbol type with :symbol-type (integer or nil to use the default types).
    Set the color with :color symbol.")

(defgeneric plot-min-max (plot))

(defmethod plot-min-max ((a-plot x-y-plot))
  "Returns the minimum and maximum values in a plot as a 4 element vector."
  (let* ((x-data (data-x a-plot))
	 (y-data (data-y a-plot))
	 (min-x (aref x-data 0))
	 (max-x (aref x-data 0))
	 (min-y (aref y-data 0))
	 (max-y (aref y-data 0)))
    (dotimes (i (length x-data))
      (when (< (aref x-data i) min-x) (setf min-x (aref x-data i)))
      (when (> (aref x-data i) max-x) (setf max-x (aref x-data i)))
      (when (< (aref y-data i) min-y) (setf min-y (aref y-data i)))
      (when (> (aref y-data i) max-y) (setf max-y (aref y-data i))))
    (let ((x-delta (* 0.05 (- max-x min-x)))
	  (y-delta (* 0.05 (- max-y min-y))))
      (vector (- min-x x-delta) (+ max-x x-delta) 
	      (- min-y y-delta) (+ max-y y-delta)))))

;; draw a plot

(defgeneric render-plot (plot &optional parameters))

(defmethod render-plot ((a-plot x-y-plot) &optional (default-symbol 0))
  "Renders a x-y plot in the current window."
  (set-foreground-color (color a-plot))
  (when (> (line-width a-plot) 0)
    (when (range-check (line-style a-plot) 1 8)
      (pllsty (line-style a-plot)))
    (plwid (line-width a-plot))
    (plline (data-x a-plot) (data-y a-plot)))
  (when (> (symbol-size a-plot) 0)
    (plssym 0 (symbol-size a-plot))
    (let ((temp-symbol (if (symbol-type a-plot) (symbol-type a-plot) default-symbol)))
      (unless (range-check temp-symbol 0 3000) (setf temp-symbol 0))
      (plpoin (data-x a-plot) (data-y a-plot) temp-symbol)))
  (when (x-error a-plot)
    (multiple-value-bind (min-error max-error) (error-bar (data-x a-plot) (x-error a-plot))
      (plerrx min-error max-error (data-y a-plot))))
  (when (y-error a-plot)
    (multiple-value-bind (min-error max-error) (error-bar (data-y a-plot) (y-error a-plot))
      (plerry (data-x a-plot) min-error max-error))))

