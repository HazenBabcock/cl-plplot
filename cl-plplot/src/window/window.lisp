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
;;;; Functions that are most closely related to the window class.
;;;;
;;;; hazen 6/06
;;;;

(in-package #:cl-plplot)

(new-object-defun window (&key x-axis y-axis title (window-line-width 1.0) (window-font-size *font-size*) 
			       (foreground-color *foreground-color*) (background-color *background-color*)
			       (viewport-x-min 0.1) (viewport-x-max 0.9) (viewport-y-min 0.1) (viewport-y-max 0.9) 
			       plots text-labels)
  "new-window, creates and returns a new window object
    x-axis is a object of type axis.
    y-axis is a object of type axis.
    title is a object of type axis-label.
    window-line-width is a floating point number specifying the pen width to use
      when drawing the border & the tick marks.
    window-font-size is the font size to use for the tick mark labels.
    foreground-color is the color to use when drawing the border & etc.
    background-color is the background color of the window.
    viewport-x-min (0.0 - 1.0) is the location of the left side of the border in the device window.
    viewport-x-max (0.0 - 1.0) is the location of the right side of the border.
    viewport-y-min (0.0 - 1.0) is the location of the bottom side of the border.
    viewport-y-max (0.0 - 1.0) is the location of the top side of the border.
    plots is a list of plot objects.
    text-labels is a list of text-label objects.")

(def-edit-method window (x-axis y-axis title window-line-width window-font-size foreground-color background-color
				viewport-x-min viewport-x-max viewport-y-min viewport-y-max plots text-labels)
  "edit-window, edits a window object.
    Set x-axis to a new object of type axis with :x-axis.
    Set y-axis to a new object of type axis with :y-axis.
    Set title to a new object of type axis-label with :title.
    Set the pen width for drawing the border with :window-line-width.
    Set the font size for the tick labels with :window-font-size.
    Set the color of the border & etc. with :foreground-color.
    Set the color of the background with :background-color.
    Set the location of the left border with :viewport-x-min.
    Set the location of the right border with :viewport-x-max.
    Set the location of the bottom border with :viewport-y-min.
    Set the location of the top border with :viewport-y-max.
    Set :plots to a list of plot objects to change the plots associated with a window.
    Set :text-labels to a list of text-label objects to change the text-labels associated with a window.")

(def-add-remove-methods window plots plot)
  ;Creates methods add-plot-to-window & remove-plot-from-window.

(def-add-remove-methods window text-labels text-label)
  ;Creates methods add-text-label-to-window & remove-text-label-from-window.

(defun basic-window (&key (x-label "x-axis") (y-label "y-axis") (title "cl-plplot") 
		   (background-color *background-color*) (foreground-color *foreground-color*))
  "Creates a basic window object with ready-to-go axises."
  (let ((title (new-axis-label (new-text-item title :font-size 1.5 :text-color foreground-color) :top 1.5))
	(x-axis (new-axis :axis-labels (list
					(new-axis-label
					 (new-text-item x-label :font-size 1.3 :text-color foreground-color) :bottom 2.5))))
	(y-axis (new-axis :axis-labels (list
					(new-axis-label
					 (new-text-item y-label :font-size 1.3 :text-color foreground-color) :left 3.0)))))
    (new-window :x-axis x-axis
		:y-axis y-axis
		:title title
		:background-color background-color
		:foreground-color foreground-color)))

(defgeneric edit-window-axis (window which-axis &key axis-min axis-max major-tick-interval minor-tick-number properties))

(defmethod edit-window-axis ((a-window window) which-axis &key axis-min axis-max major-tick-interval minor-tick-number properties)
  "Allows the user to edit the axis of a window. 
   which-axis should be one of the symbols :x or :y.
   See edit-axis for a more detailed explanation of the meaning of the different key words."
  (let ((a-axis (if (equal which-axis :x) (x-axis a-window) (y-axis a-window))))
    (edit-axis a-axis
	       :axis-min axis-min
	       :axis-max axis-max
	       :major-tick-interval major-tick-interval
	       :minor-tick-number minor-tick-number
	       :properties properties)))

(defun get-axis-ranges (a-window)
  "Figures out the minimum and maximum values for both axises if this has
   not already been specified. Defaults to 1.0 x 1.0 if there are no plots
   associated with the window."
  (let ((x-min (axis-min (x-axis a-window)))
	(x-max (axis-max (x-axis a-window)))
	(y-min (axis-min (y-axis a-window)))
	(y-max (axis-max (y-axis a-window))))
    (unless (and x-min x-max y-min y-max)
      (let ((temp-x-min)
	    (temp-x-max)
	    (temp-y-min)
	    (temp-y-max))
	(if (plots a-window)
	    (dolist (a-plot (plots a-window))
	      (let ((range (plot-min-max a-plot)))
		(when (or (not temp-x-min)
			  (< (aref range 0) temp-x-min))
		  (setf temp-x-min (aref range 0)))
		(when (or (not temp-x-max)
			  (> (aref range 1) temp-x-max))
		  (setf temp-x-max (aref range 1)))
		(when (or (not temp-y-min)
			  (< (aref range 2) temp-y-min))
		  (setf temp-y-min (aref range 2)))
		(when (or (not temp-y-max)
			  (> (aref range 3) temp-y-max))
		  (setf temp-y-max (aref range 3)))))
	    (progn
	      (setf temp-x-min 0.0)
	      (setf temp-x-max 1.0)
	      (setf temp-y-min 0.0)
	      (setf temp-y-max 1.0)))
	(unless x-min (setf x-min temp-x-min))
	(unless x-max (setf x-max temp-x-max))
	(unless y-min (setf y-min temp-y-min))
	(unless y-max (setf y-max temp-y-max))))
    (values x-min x-max y-min y-max)))

(defun render-window (a-window device filename size-x size-y)
  "This handles drawing the window & optionally returns the coordinates of a mouse click."
  ;; setup window
  (plsdev device)
  (when filename
    (plsfile filename))
  (plspage 0 0 size-x size-y 0 0)
  (plscmap0n 16)
  (plinit)
  (set-background-color (background-color a-window))
  (set-foreground-color (foreground-color a-window))
  (pladv 0)
  (plvpor (viewport-x-min a-window) (viewport-x-max a-window) (viewport-y-min a-window) (viewport-y-max a-window))
  (multiple-value-bind (x-min x-max y-min y-max) (get-axis-ranges a-window)
    (plwind x-min x-max y-min y-max))
  (plwid (window-line-width a-window))
  (plschr 0 (window-font-size a-window))
  (plbox (get-axis-properties (x-axis a-window)) (major-tick-interval (x-axis a-window)) (minor-tick-number (x-axis a-window))
	 (get-axis-properties (y-axis a-window)) (major-tick-interval (y-axis a-window)) (minor-tick-number (y-axis a-window)))
  ;; title & axis labels
  (render-axis-label (title a-window))
  (render-axis-labels (x-axis a-window))
  (render-axis-labels (y-axis a-window))
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

(defgeneric get-cursor (window device &key size-x size-y))

(defmethod get-cursor ((a-window window) device &key (size-x 600) (size-y 500))
  "Get the location (in window coordinates) of the next mouse click. In
   order to do this the window must first be rendered so that the user has
   something to click on."
  (render-window a-window device nil size-x size-y)
  (let ((mouse (plgetcursor)))
    (plend)
    (values (elt mouse 8) (elt mouse 9))))

(defgeneric render (window device &key filename size-x size-y))

(defmethod render ((a-window window) device &key filename (size-x 600) (size-y 500))
  "Renders a window and it associated plots and labels using device.
    device: a string naming a plplot graphical device such as 'xwin'.
    filename: where to save the graph for file based devices.
    size-x: the size of the window in x (pixels).
    size-y: the size of the window in y (pixels)."
  (render-window a-window device filename size-x size-y)
  (plend))