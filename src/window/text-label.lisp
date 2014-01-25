;;;;
;;;; Functions that are most closely related to the text-label class.
;;;;
;;;; hazen 6/06
;;;;

(in-package #:cl-plplot)

(new-object-defun text-label (label-text-item text-x text-y &key (text-dx 0.0) (text-dy 0.0))
  "new-text-label, creates and returns a new text-label object.
    text-item should be an object created by new-text-item.
    text-x specifies the x location of the text reference point (in window coordinates).
    text-y specifies the y location of the text reference point.
    text-dx and text-dy specify the location of a second reference point (in window
      coordinates) the text is drawn along a line connecting (text-x,text-y) and
      (text-x + text-dx, text-y + text-dy).")

(def-edit-method text-label (label-text-item text-x text-y text-dx text-dy)
  "edit-text-label, Edits a text-label object.
    Set text-item to a new object of class text-item with :label-text-item.
    Set the x location of the text with :text-x.
    Set the y location of the text with :text-y.
    Set dx for drawing text at an angle with :text-dx.
    Set dy for drawing text at an angle with :text-dy.")

(new-object-defun 3D-text-label (label-text-item text-x text-y text-z &key 
						 (text-dx 0.0) (text-dy 0.0) (text-dz 0.0)
						 (text-sx 0.0) (text-sy 0.0) (text-sz 0.0))
  "new-3D-text-label, creates and returns a new 3D-text-label object.
    text-item should be an object created by new-text-item.
    text-x specifies the x location of the text reference point (in window coordinates).
    text-y specifies the y location of the text reference point.
    text-z specifies the y location of the text reference point.
    text-dx, text-dy and text-dz specify the location of a second reference point 
      (in window coordinates) the text is drawn along a line connecting 
      (text-x,text-y and text-z) and (text-x + text-dx, text-y + text-dy, text-z + text-dz).
    test-sx, text-sy and text-sz specify the location of a third reference point
      (in window coordinates) the text is sheared to be parallel to a line connecting
      (text-x,text-y and text-z) and (text-x + text-sx, text-y + text-sy, text-z + text-sz).")

(def-edit-method 3D-text-label (label-text-item text-x text-y text-z text-dx text-dy text-dz
						text-sx text-sy text-sz)
  "edit-text-label, Edits a text-label object.
    Set text-item to a new object of class text-item with :label-text-item.
    Set the x location of the text with :text-x.
    Set the y location of the text with :text-y.
    Set the z location of the text with :text-z.
    Set dx for drawing text at an angle with :text-dx.
    Set dy for drawing text at an angle with :text-dy.
    Set dz for drawing text at an angle with :text-dz.
    Set sx for shearing text with :text-sx.
    Set sy for shearing text with :text-sy.
    Set sz for shearing text with :text-sz.")

(defgeneric render-text-label (text-label))

(defmethod render-text-label ((a-text-label text-label))
  "Draws a text label onto the current plot."
  (plptex (text-x a-text-label)
	  (text-y a-text-label)
	  (text-dx a-text-label)
	  (text-dy a-text-label)
 	  (text-justification (label-text-item a-text-label))
	  (render-text (label-text-item a-text-label))))

(defmethod render-text-label ((a-text-label 3D-text-label))
  "Draws a 3D text label onto the current plot."
  (plptex3 (text-x a-text-label)
	   (text-y a-text-label)
	   (text-z a-text-label)
	   (text-dx a-text-label)
	   (text-dy a-text-label)
	   (text-dz a-text-label)
	   (text-sx a-text-label)
	   (text-sy a-text-label)
	   (text-sz a-text-label)
	   (text-justification (label-text-item a-text-label))
	   (render-text (label-text-item a-text-label))))

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
