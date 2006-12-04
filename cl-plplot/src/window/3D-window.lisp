;;;;
;;;; Functions that are most closely related to the 3D window class.
;;;;
;;;; hazen 12/06
;;;;

(in-package #:cl-plplot)

(new-object-defun 3D-window (&key x-axis y-axis z-axis title (window-line-width 1.0) (window-font-size *font-size*) 
			       (foreground-color *foreground-color*) (background-color *background-color*)
			       (viewport-x-min 0.1) (viewport-x-max 0.9) (viewport-y-min 0.1) (viewport-y-max 0.9) 
			       plots text-labels color-table (altitude 60) (azimuth 30))
  "new-3D-window, creates and returns a new 3D-window object
    x-axis is a object of type axis.
    y-axis is a object of type axis.
    z-axis is a object of type axis.
    title is a object of type axis-label.
    foreground-color is a color symbol in the current color table.
    background-color is a color symbol in the curretn color table.
    window-line-width is a floating point number specifying the pen width to use
      when drawing the border & the tick marks.
    window-font-size is the font size to use for the tick mark labels.
    viewport-x-min (0.0 - 1.0) is the location of the left side of the border in the device window.
    viewport-x-max (0.0 - 1.0) is the location of the right side of the border.
    viewport-y-min (0.0 - 1.0) is the location of the bottom side of the border.
    viewport-y-max (0.0 - 1.0) is the location of the top side of the border.
    plots is a list of plot objects.
    text-labels is a list of text-label objects.
    color-table specifies what color table to use.
    altitude specifies the angle by which to rotate the plot around the x axis.
    azimuth specified the angle by which to rotate the plot around the z axis.")

(def-edit-method 3D-window (x-axis y-axis z-axis title foreground-color background-color window-line-width 
				window-font-size viewport-x-min viewport-x-max viewport-y-min viewport-y-max 
				plots text-labels color-table altitude azimuth)
  "edit-3D-window, edits a 3D window object.
    Set x-axis to a new object of type axis with :x-axis.
    Set y-axis to a new object of type axis with :y-axis.
    Set z-axis to a new object of type axis with :z-axis.
    Set title to a new object of type axis-label with :title.
    Set the foreground color with :foreground-color.
    Set the background color with :background-color.
    Set the pen width for drawing the border with :window-line-width.
    Set the font size for the tick labels with :window-font-size.
    Set the location of the left border with :viewport-x-min.
    Set the location of the right border with :viewport-x-max.
    Set the location of the bottom border with :viewport-y-min.
    Set the location of the top border with :viewport-y-max.
    Set :plots to a list of plot objects to change the plots associated with a window.
    Set :text-labels to a list of text-label objects to change the text-labels associated with a window.
    Set :color-table to a new color table object to change the colors of a plot.
    Set the observer altitude in degrees with :altitude.
    Set the observer azimuth in degrees with :azimuth.")

(def-add-remove-methods 3D-window plots plot)
  ;Creates methods add-plot-to-window & remove-plot-from-window.

(def-add-remove-methods 3D-window text-labels text-label)
  ;Creates methods add-text-label-to-window & remove-text-label-from-window.

(defun basic-3D-window (&key (x-label "x-axis") (y-label "y-axis") (z-label "z-axis") (title "cl-plplot") 
		   (background-color *background-color*) (foreground-color *foreground-color*))
  "Creates a 3D window object with ready-to-go axises. Note that in 3D plots you don't have the same control
   that you have in 2D plots about the text location, font-size, etc... so we don't bother to set so many
   axis-label and text-item object properties. The label font size of the axis labels is instead determined
   by window-font-size."
  (let ((title (new-axis-label (new-text-item title :font-size 1.5 :text-color foreground-color) :top 1.5))
	(x-axis (new-axis :axis-labels (list (new-axis-label (new-text-item x-label)))))
	(y-axis (new-axis :axis-labels (list (new-axis-label (new-text-item y-label)))))
	(z-axis (new-axis :axis-labels (list (new-axis-label (new-text-item z-label))))))
    (new-window :x-axis x-axis
		:y-axis y-axis
		:z-axis z-axis
		:title title
		:foreground-color foreground-color
		:background-color background-color)))

(defun render-3D-window (a-window device filename size-x size-y)
  "This handles drawing a 3D window."
  ;; setup window
  (setup-window a-window device filename size-x size-y)

  ;; start plotting
  (plinit)
  (unwind-protect
       (progn
	 ; setup viewport
	 (pladv 0)
	 (plvpor (viewport-x-min a-window) (viewport-x-max a-window) (viewport-y-min a-window) (viewport-y-max a-window))
	 (multiple-value-bind (x-min x-max y-min y-max z-min z-max) (get-axis-ranges a-window)
	   (plwind x-min x-max y-min y-max)
	   (plw3d (* 2 (- x-max x-min)) (* 2 (- y-max y-min)) (* 2 (- z-max z-min))
		  x-min x-max y-min y-max z-min z-max
		  (azimuth a-window) (altitude a-window)))

	 ;; title, axis & axis labels
	 (set-foreground-color (foreground-color a-window))
	 (pllsty 1)
	 (plwid (window-line-width a-window))
	 (plschr 0 (window-font-size a-window))
	 (plbox3 (get-axis-properties (x-axis a-window)) ; x-axis
		 (render-text (axis-text-item (car (axis-labels (x-axis a-window)))) t)
		 (major-tick-interval (x-axis a-window))
		 (minor-tick-number (x-axis a-window))
		 
		 (get-axis-properties (y-axis a-window)) ; y-axis
		 (render-text (axis-text-item (car (axis-labels (y-axis a-window)))) t)
		 (major-tick-interval (y-axis a-window))
		 (minor-tick-number (y-axis a-window))
		 
		 (get-axis-properties (z-axis a-window)) ; z-axis
		 (render-text (axis-text-item (car (axis-labels (z-axis a-window)))) t)
		 (major-tick-interval (z-axis a-window))
		 (minor-tick-number (z-axis a-window)))
	 (render-axis-label (title a-window))
	 
	 ;; render plots
	 (when (plots a-window)
	   (let ((default-symbol 0))
	     (dolist (a-plot (plots a-window))
	       (render-plot a-plot default-symbol)
	       (incf default-symbol))))
	   
	 ;; text labels
	 (when (text-labels a-window)
	   (dolist (a-text-label (text-labels a-window))
	     (render-text-label a-text-label))))
    (plend)))

(defmethod render ((a-window 3D-window) device &key filename (size-x 600) (size-y 500))
  "Renders a 3D-window and it associated plots and labels using device.
    device: a string naming a plplot graphical device such as 'xwin'.
    filename: where to save the graph for file based devices.
    size-x: the size of the window in x (pixels).
    size-y: the size of the window in y (pixels).
   If you are using cl-plplot in a multi-threaded environment you should
   thread lock prior to calling render, as the PLplot library only handles
   rendering one plot at a time."
  (render-3D-window a-window device filename size-x size-y))

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
