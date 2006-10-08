;;;;
;;;; Functions that are most closely related to the color class.
;;;;
;;;; This color class is used to setup PLplot's color map 0. This
;;;; color map will contain a few (typically 16) colors that can
;;;; be referred to by name.
;;;;
;;;; To be compatible with most of the plplot device drivers you
;;;; should probably limit yourself to 16 colors. Note that color
;;;; index 0 is always the background color and that the default
;;;; foreground color has index 1.
;;;;
;;;; Each entry in the color map is a vector of #(r g b :symbol),
;;;; where :symbol is your handle to the color for later use
;;;; during actual plotting. Referring to the default color table 
;;;; as an example, the first color, designated :white, will be
;;;; the background color and the second color, designated
;;;; :black, will be the color that the plot rectangle, ticks
;;;; marks & etc are drawn in (ambiguously referred to as
;;;; the foreground color.)
;;;;
;;;; hazen 8/06
;;;;

(in-package #:cl-plplot)


;; external

(defun new-color-table (&optional rgb-colors)
  "Creates a new color table instance from a vector of rgb triples,
   which also includes a symbol to refer to the color by.
   For example: #((0 0 0 :black) (128 128 128 :grey) (255 255 255 :white))."
  (make-instance 'color-table 
		 :color-map (when (vectorp rgb-colors)
			      (if (vectorp (aref rgb-colors 0))
				  rgb-colors
				  (vector rgb-colors)))))

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

(defgeneric update-color (a-color-table color-symbol new-color))

(defmethod update-color ((a-color-table color-table) color-symbol new-color)
  "Changes the color specified by color-symbol to new-color, which
   should be a rgb triple in the form  #(r g b)."
  (let ((color-index (find-a-color a-color-table color-symbol)))
    (when color-index
      (let ((color-to-change (aref (color-map a-color-table) color-index)))
	(setf (aref color-to-change 0) (aref new-color 0))
	(setf (aref color-to-change 1) (aref new-color 1))
	(setf (aref color-to-change 2) (aref new-color 2))))))

(defgeneric set-color-table (a-window a-color-table))

(defmethod set-color-table ((a-window window) (a-color-table color-table))
  "Sets the color table associated with a-window to a-color-table.
   Returns the old color table."
  (let ((old-color-table (color-table a-window)))
    (setf (color-table a-window) a-color-table)
    old-color-table))

(defgeneric add-color-to-color-table (a-color-table new-color))

(defmethod add-color-to-color-table ((a-color-table color-table) new-color)
  "Adds a new color #(r g b :smybol) to the end of a color table."
  (let* ((a-color-map (color-map a-color-table))
	 (color-map-length (length a-color-map))
	 (new-color-map (make-array (1+ color-map-length))))
    (dotimes (i color-map-length)
      (setf (aref new-color-map i) (aref a-color-map i)))
    (setf (aref new-color-map color-map-length) new-color)
    (setf (color-map a-color-table) new-color-map)))

(defgeneric remove-color-from-color-table (a-color-table &optional new-color))

(defmethod remove-color-from-color-table ((a-color-table color-table) &optional color-to-remove)
  "Removes color-to-remove, if specified, or the last color if not."
  (let ((a-color-map (color-map a-color-table)))
    (setf a-color-map
	  (if color-to-remove
	      (remove-if #'(lambda (x) (equal color-to-remove (aref x 3))) a-color-map)
	      (butlast a-color-map)))))


;; internal
      
(defgeneric change-background-color (a-color-table color-symbol))

(defmethod change-background-color ((a-color-table color-table) color-symbol)
  "Changes the current background color to that specified by color-symbol."
  (swap-colors a-color-table color-symbol (aref (aref (color-map a-color-table) 0) 3)))
  
(defgeneric change-foreground-color (a-color-table color-symbol))

(defmethod change-foreground-color ((a-color-table color-table) color-symbol)
  "Changes the current foreground color to that specified by color-symbol."
  (swap-colors a-color-table color-symbol (aref (aref (color-map a-color-table) 1) 3)))

(defgeneric swap-colors (a-color-table color1 color2))

(defmethod swap-colors ((a-color-table color-table) color1 color2)
  "Swaps the position of color1 and color2 in the color table.
   If one of the colors does not exist then nothing happens."
  (let ((a-color-map (color-map a-color-table))
	(index1 (find-a-color a-color-table color1))
	(index2 (find-a-color a-color-table color2)))
    (when (and index1 index2)
      (let ((color-vec1 (copy-seq (aref a-color-map index1)))
	    (color-vec2 (copy-seq (aref a-color-map index2))))
	(setf (aref a-color-map index1) color-vec2)
	(setf (aref a-color-map index2) color-vec1)))))

(defun find-a-color (a-color-table color-specifier)
  "Returns the index of the specified color in the color table or
   NIL if the color cannot be found."
  (let ((a-color-map (color-map a-color-table)))
    (dotimes (i (length a-color-map))
      (let ((a-color (aref a-color-map i)))
	(when (equal color-specifier (aref a-color 3))
	  (return-from find-a-color i))))
    nil))

(defgeneric initialize-color-table (a-color-table foreground-color background-color))

(defmethod initialize-color-table ((a-color-table color-table) foreground-color background-color)
  "Initializes the color table in plot and sets it to match
   our local version of the color table."
  (change-background-color a-color-table background-color)
  (change-foreground-color a-color-table foreground-color)
  (let ((a-color-map (color-map a-color-table)))
    (plscmap0n (length a-color-map))
    (dotimes (i (length a-color-map))
      (let ((a-color (aref a-color-map i)))
	(plscol0 i (aref a-color 0) (aref a-color 1) (aref a-color 2))))))

(defgeneric get-background-color (a-color-table))

(defmethod get-background-color ((a-color-table color-table))
  (aref (aref (color-map a-color-table) 0) 3))

(defgeneric get-foreground-color (a-color-table))

(defmethod get-foreground-color ((a-color-table color-table))
  (aref (aref (color-map a-color-table) 1) 3))

(defun set-background-color ()
  "Switches the pen to the background color."
  (plcol0 0))

; Since the plplot library can only handle drawing one plot at a time
; this variable is set in render-window to the current color table.
; This approach, while not perhaps ideal, does let us avoid having
; to pass a color table into every function that wants to set a color.

(defvar *current-color-table*)

(defun set-foreground-color (color)
  "Switches the pen to the desired foreground color, or the default
   foreground color if the desired color cannot be found."
  (cond
    ((symbolp color)
     (let ((color-index (find-a-color *current-color-table* color)))
       (if color-index
	   (plcol0 color-index)
	   (plcol0 1))))
    (t
     (format t "Failed to set color to: ~A~%" color))))

(defun set-color-by-index (index)
  "Switches the pen to color currently at index. The background color
   is ignored & colors wrap when the index is off the end of the color table."
  (plcol0 (1+ (mod index (1- (length (color-map *current-color-table*)))))))


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
