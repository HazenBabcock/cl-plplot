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
;;;; Define the cl-plplot including what it uses & what it exports
;;;;
;;;; hazen 6/06
;;;;

(in-package #:cl-user)

(defpackage #:cl-plplot
  (:use #:common-lisp
	#:cl-plplot-system)
  (:export #:add-axis-label-to-axis
	   #:add-plot-to-window
	   #:add-text-label-to-window
	   #:backspace
	   #:basic-window
	   #:default-color-table
	   #:edit-axis
	   #:edit-axis-label
	   #:edit-text-item
	   #:edit-text-label
	   #:edit-window
	   #:edit-window-axis
	   #:edit-x-y-plot
	   #:get-cursor
	   #:greek-char
	   #:hershey-char
	   #:italic-font
	   #:new-axis
	   #:new-axis-label
	   #:new-color-table
	   #:new-text-item
	   #:new-text-label
	   #:new-window
	   #:new-x-y-plot
	   #:normal-font
	   #:number-symbol
	   #:overline
	   #:remove-axis-label-from-axis
	   #:remove-plot-from-window
	   #:remove-text-label-from-window
	   #:render
	   #:roman-font
	   #:script-font
	   #:set-color-table
	   #:subscript
	   #:superscript
	   #:underline
	   #:unicode-char
	   #:update-color))
	   

(in-package #:cl-plplot)

(defvar *foreground-color* :black)
(defvar *background-color* :white)
(defvar *font* :normal)
(defvar *font-size* 1.0)
(defvar *axis-properties* '(:draw-bottom/left :draw-top/right :major-tick-labels-below/left :minor-ticks :major-ticks))

