;;;;
;;;; Functions that are most closely related to the axis-label class.
;;;;
;;;; hazen 6/06
;;;;

(in-package #:cl-plplot)

;; axis-label
(new-object-defun axis-label (axis-text-item side displacement &key (location 0.5) (orientation :parallel))
  "new-axis-label, Create and returns a new axis label.
     axis-text-item is a object of type text-item.
     side is one of :top, :bottom, :left or :right.
     displacement specifies the distance from the edge of the graph in
       units of the default font-size.
     location is the position of the label along the side of the graph (0.0 - 1.0).
     orientation is one :parallel or :perpendicular.")

(def-edit-method axis-label (axis-text-item side displacement location orientation)
  "edit-axis-label, Edits a axis-label object.
    Set axis-text-item to a new object of class text-item with :axis-text-item.
    Set which side to draw the label on with :side.
    Set the displacement from the edge of the the graph with :displacement.
    Set the location with along the side of the graph with :location.
    Set the orientation with (:parallel or :perpendicular) with :orientation.")

(defgeneric render-axis-label (axis-label))

(defmethod render-axis-label ((a-axis-label axis-label))
  "Draws a axis label onto the current plot."
  (let ((side-spec))
    (case (side a-axis-label)
      (:top (setf side-spec "t"))
      (:bottom (setf side-spec "b"))
      (:left (setf side-spec "l"))
      (otherwise (setf side-spec "r")))
    (when (equal (orientation a-axis-label) :perpendicular)
      (setf side-spec (concatenate 'string side-spec "v")))
    (plmtex side-spec
	    (displacement a-axis-label)
	    (location a-axis-label)
	    (text-justification (axis-text-item a-axis-label))
	    (render-text (axis-text-item a-axis-label)))))

;; 3D-axis-label
(new-object-defun 3D-axis-label (axis-text-item side displacement &key (location 0.5) (orientation :parallel) (primary/secondary :primary))
  "new-3D-axis-label, Create and returns a new 3D axis label.
     axis-text-item is a object of type text-item.
     side is one of :x, :y, or :z.
     displacement specifies the distance from the edge of the graph in
       units of the default font-size.
     location is the position of the label along the side of the graph (0.0 - 1.0).
     orientation is one :parallel or :perpendicular.
     primary/secondary is one of :primary or :secondary. This specifies which of two possible
       choices for each axis should be labeled.")

(def-edit-method 3D-axis-label (axis-text-item side displacement location orientation primary/secondary)
  "edit-3D-axis-label, Edits a 3D-axis-label object.
    Set axis-text-item to a new object of class text-item with :axis-text-item.
    Set which side to draw the label on with :side.
    Set the displacement from the edge of the the graph with :displacement.
    Set the location with along the side of the graph with :location.
    Set the orientation with (:parallel or :perpendicular) with :orientation.
    Set which axis to label (:primary or :secondary) with :primary/secondary")

(defmethod render-axis-label ((a-axis-label 3D-axis-label))
  "Draws a 3D axis label onto the current plot."
  (let ((side-spec 
	 (concatenate 'string
		      (case (side a-axis-label)
			(:x "x")
			(:y "y")
			(:z "z"))
		      (if (equal (primary/secondary a-axis-label) :primary)
			  "p" "s")
		      (if (equal (orientation a-axis-label) :perpendicular)
			  "v" ""))))
    (plmtex3 side-spec
	     (displacement a-axis-label)
	     (location a-axis-label)
	     (text-justification (axis-text-item a-axis-label))
	     (render-text (axis-text-item a-axis-label)))))

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
