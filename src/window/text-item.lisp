;;;;
;;;; Functions that are most closely related to the text-item class.
;;;;
;;;; hazen 6/06
;;;;


(in-package #:cl-plplot)

(new-object-defun text-item (the-text &key (text-color *foreground-color*) (text-justification 0.5) (font-size *font-size*))
  "new-text-item, Creates and returns a new text item.
    text is a string specifying the text.
    text-color is a symbol specifying the text color.
    text-justification specifies how to center the string relative to its reference
       point. 0.0 - 1.0, where 0.5 means the string is centered.
    font-size sets the fontsize relative to the default font size.")

(def-edit-method text-item (the-text text-color text-justification font-size)
  "edit-text-item, Edits a text-item object.
    Set the text with :text.
    Set the color of the text with :text-color symbol.
    Set the justification with :text-justification (0.0 = left justified, 1.0 = right justified).
    Set the font-size with :font-size (relative to the default size).")

(defmacro create-esc-function (function-name esc-string-start &optional esc-string-end)
  "creates functions that handle escaping strings for plplot."
  `(defun ,function-name (&rest strings)
     (let ((new-string ,esc-string-start))
       (dolist (string strings)
	 (setf new-string (concatenate 'string new-string string))
	 ,(unless esc-string-end `(setf new-string (concatenate 'string new-string ,esc-string-start))))
       ,(when esc-string-end `(setf new-string (concatenate 'string new-string ,esc-string-end)))
       new-string)))

; these functions follow page 33 in the plplot manual.

(create-esc-function superscript "#u" "#d")

(create-esc-function subscript "#d" "#u")

(defun backspace () "#b")

(defun number-symbol () "##")

(create-esc-function overline "#+" "#+")

(create-esc-function underline "#-" "#-")

(defun greek-char (roman-character)
  (concatenate 'string "#g" (string roman-character)))

(create-esc-function normal-font "#fn")

(create-esc-function roman-font "#fr")

(create-esc-function italic-font "#fi")

(create-esc-function script-font "#fs")

(create-esc-function hershey-char "#(" ")")

(create-esc-function unicode-char "#[" "]")

(defun render-text (a-text-item &optional is-a-3d-window-label)
  "If this text is not the label of a 3d window, set the current color 
   and font size to those specified by the text item. Then process the 
   possibly utf-8 encoded string contained in text item to properly handle 
   non ASCII characters and return the processed string."
  (unless is-a-3d-window-label
    (set-foreground-color (text-color a-text-item))
    (plschr 0 (font-size a-text-item)))
  (let* ((utf8-text (the-text a-text-item))
	 (escaped-string (make-array (* 10 (length utf8-text)) :element-type 'character))
	 (l 0))
    (dotimes (i (length utf8-text))
      (let ((code (char-code (char utf8-text i))))
	(if (> code 128)
	  (let ((converted-char (format nil "#[~A]" code)))
	    (dotimes (j (length converted-char))
	      (setf (char escaped-string l) (char converted-char j))
	      (incf l)))
	  (progn
	    (setf (char escaped-string l) (char utf8-text i))
	    (incf l)))))
    (subseq escaped-string 0 l)))

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
