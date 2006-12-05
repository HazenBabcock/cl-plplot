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
;;;; Functions that are most closely related to the axis class.
;;;;
;;;; hazen 6/06
;;;;

(in-package #:cl-plplot)

(new-object-defun axis (&key axis-min axis-max (major-tick-interval 0) (minor-tick-number 0) (properties *axis-properties*) axis-labels)
  "new-axis, Creates and returns an axis object.
    axis-min is the minimum value for the axis.
    axis-amx is the maximum value for the axis.
    major-tick-interval is the spacing between major ticks (0 means use plplot default).
    minor-tick-number is the number of minor ticks to put between the major ticks.
    properties is a list of symbols as explained in edit-axis.
    axis-labels is a list of axis-label objects.")

(def-edit-method axis (axis-min axis-max major-tick-interval minor-tick-number properties)
  "edit-axis, Edits an axis.
    Set the minimum value with :axis-min.
    Set the maximum value with :axis-max.
    Set the spacing between major ticks with major-tick-interval.
    Set the spacing between minor ticks with minor-tick-interval.
    Set the properties with :properties. This should containing zero of more of
         the following symbols:
      :draw - draw axis on both sides of the window.
      :draw-bottom/left - draw axis on the bottom/left side of the window.
      :draw-top/right - draw axis on the top/right side of the window.
      :grid-lines - (z axis of 3D plots only) draw grid lines parallel to the
         x-y plane behind the figure.
      :fixed-point - use fixed point labels.
      :major-tick-grid - draw a grid on the graph at the major ticks.
      :minor-tick-grid - draw a grid on the graph at the minor ticks.
      :invert-ticks - draw ticks inward rather than outwards.
      :log-axis - draw the axis on a log scale.
      :major-tick-labels-above/right - draw the tick labels above/right of the ticks.
      :major-tick-labels-below/left - draw the tick labels below/left of the ticks.
      :minor-ticks - draw minor ticks.
      :major-ticks - draw major ticks.
      :text-under-axis - (x & y axis of 3D plots only) draw the text label under
         the x/y axis.
      :text-by-left-axis - (z axis of 3D plots only) draw the text label beside the
         left hand axis.
      :text-by-right-axis) (z axis of 3D plots only) draw the text label beside the
         right hand axis.")

(def-add-remove-methods axis axis-labels axis-label)
  ;Creates the functions add-axis-label-to-axis & remove-axis-label-from-axis.

(defgeneric get-axis-properties (axis))

(defmethod get-axis-properties ((a-axis axis))
  "Creates an options string that we can pass to plplot to get the
   axis (and possibly a grid as well) drawn as desired by the user."
  (let ((opt-string ""))
    (labels ((my-cat (char)
	       (setf opt-string (concatenate 'string opt-string (string char)))))
      (dolist (property (remove-duplicates (properties a-axis) :test #'equal))
	(cond
	  ((equal property :draw) (my-cat "a"))
	  ((equal property :draw-bottom/left) (my-cat "b"))
	  ((equal property :draw-top/right) (my-cat "c"))
	  ((equal property :grid-lines) (my-cat "d"))
	  ((equal property :fixed-point) (my-cat "f"))
	  ((equal property :major-tick-grid) (my-cat "g"))
	  ((equal property :minor-tick-grid) (my-cat "h"))
	  ((equal property :invert-ticks) (my-cat "i"))
	  ((equal property :log-axis) (my-cat "l"))
	  ((equal property :major-tick-labels-above/right) (my-cat "m"))
	  ((equal property :major-tick-labels-below/left) (my-cat "n"))
	  ((equal property :minor-ticks) (my-cat "s"))
	  ((equal property :major-ticks) (my-cat "t"))
	  ((equal property :text-under-axis) (my-cat "u"))
	  ((equal property :text-by-left-axis) (my-cat "u"))
	  ((equal property :text-by-right-axis) (my-cat "v"))
	  (t (format t "Unrecognized property ~A~%" property)))))
    opt-string))

(defgeneric render-axis-labels (axis))

(defmethod render-axis-labels ((a-axis axis))
  "Draws all the labels associated with an axis onto the current plot."
  (when (axis-labels a-axis)
    (dolist (a-label (axis-labels a-axis))
      (render-axis-label a-label))))
