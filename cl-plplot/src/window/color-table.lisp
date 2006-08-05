;;;;
;;;; Functions that are most closely related to the color class.
;;;;
;;;; To be compatible with most of the plplot device drivers we limit
;;;; ourselves to a user defined 16 color color-map. Note that color
;;;; index 0 is always the background color and that the default
;;;; foreground color has index 1.
;;;;
;;;; hazen 6/06
;;;;

(in-package #:cl-plplot)

(defun new-color-table (rgb-colors)
  "Creates a new color table instance from a vector of rgb triples,
   which also possibly includes a symbol to refer to the color by.
   For example: #((0 0 0 :black) (128 128 128 :grey) (255 255 255 :white))."
  (make-instance 'color-table :color-map rgb-colors))

(defun default-color-table ()
  "Returns the default color table."
  (new-color-table (vector #(255 255 255 :white)
			   #(0 0 0 :black)
			   #(240 50 50 :red)
			   #(255 255 0 :yellow)
			   #(0 255 0 :green)
			   #(127 255 212 :aquamarine)
			   #(255 192 203 :pink)
			   #(245 222 179 :wheat)
			   #(190 190 190 :grey)
			   #(165 42 42 :brown)
			   #(0 0 255 :blue)
			   #(138 43 226 :blue-violet)
			   #(0 255 255 :cyan)
			   #(64 224 208 :turquoise)
			   #(255 0 255 :magenta)
			   #(250 128 114 :salmon))))

(defgeneric update-color (a-color-table color-specifier new-color))

(defmethod update-color ((a-color-table color-table) color-specifier new-color)
  "Changes the color specified by color-specified to new-color, which
   should be one of: (r g b) or (r g b symbol). If you specify the color
   to change using a symbol then the symbol associated with the color
   will not be changed."
  (if (numberp color-specifier)
      (setf (aref (color-map a-color-table) color-specifier) new-color)
      (let ((color-index (find-a-color a-color-table color-specifier)))
	(if color-index
	    (let ((color-to-change (aref (color-map a-color-table) color-index)))
	      (setf (aref color-to-change 0) (aref new-color 0))
	      (setf (aref color-to-change 1) (aref new-color 1))
	      (setf (aref color-to-change 2) (aref new-color 2)))
	    (format t "Color ~A was not found!" new-color)))))

(defgeneric set-color-table (a-window a-color-table))

(defmethod set-color-table ((a-window window) (a-color-table color-table))
  "Sets the color table associated with a-windoe to a-color-table.
   Returns the old color table."
  (let ((old-color-table (color-table a-window)))
    (setf (color-table a-window) a-color-table)
    old-color-table))

(defgeneric swap-colors (a-color-table color new-index))

(defmethod swap-colors ((a-color-table color-table) color new-index)
  "Swaps the positions of color and the color at new-index in the color table. 
   This is useful when the user wants to change the default foreground
   and background colors to some other color in the color table."
  (let* ((a-color-map (color-map a-color-table))
	 (old-index (find-a-color a-color-table color))
	 (color-vec1 (copy-seq (aref a-color-map old-index)))
	 (color-vec2 (copy-seq (aref a-color-map new-index))))
    (setf (aref a-color-map old-index) color-vec2)
    (setf (aref a-color-map new-index) color-vec1)))

(defun find-a-color (a-color-table color-specifier)
  "Returns the index of the specified color in the color table or
   NIL if the color cannot be found."
  (let ((a-color-map (color-map a-color-table)))
    (dotimes (i (length a-color-map))
      (let ((a-color (aref a-color-map i)))
	(when (and (> (length a-color) 3)
		   (equal color-specifier (aref a-color 3)))
	  (return-from find-a-color i))))
    nil))

(defgeneric initialize-color-table (a-color-table))

(defmethod initialize-color-table ((a-color-table color-table))
  "Initializes the color table in plot and sets it to match
   our local version of the color table."
  (plscmap0n 16)
  (let ((a-color-map (color-map a-color-table)))
    (dotimes (i (length a-color-map))
      (let ((a-color (aref a-color-map i)))
	(plscol0 i (aref a-color 0) (aref a-color 1) (aref a-color 2))))))

(defun set-background-color ()
  "Switches the pen to the background color."
  (plcol0 0))

; Since the plplot library can only handle drawing one plot at a time
; this variable is set in render-window to the current color table.
; This approach, while not perhaps ideal, does let us avoid having
; to pass a color table into every function that wants to set a color.

(defvar *current-color-table*)

(defun set-foreground-color (color)
  "Switches the pen to the desired foreground color."
  (if (numberp color)
      (plcol0 0)
      (let ((color-index (find-a-color *current-color-table* color)))
	(if color-index
	    (plcol0 color-index)
	    (plcol0 1)))))

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
