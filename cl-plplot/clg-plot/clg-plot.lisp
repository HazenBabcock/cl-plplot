;;;
;;; Combining clg and cl-plplot to make a nice plotting window.
;;;

(defpackage :clg-plot
  (:use	:common-lisp
	:clg)
  (:export :new-clg-plot))

(in-package :clg-plot)

(defun render-the-plot (cairo-context cl-plplot-window width height)
  "Calls cl-plplot to redraw the plot in the new window."
  (cl-plplot:render cl-plplot-window "extcairo" 
		    :size-x width :size-y height :external-pointer cairo-context))

(defun create-drawing-function (drawing-area cl-plplot-window)
  "Defines the drawing function that is used to redraw the plot."
  #'(lambda ()
      (gdk:with-cairo-context (cr (widget-window drawing-area))
	(multiple-value-bind (width height)
	    (widget-get-size-allocation drawing-area)
	  (cairo:set-source-color cr 1.0 1.0 1.0)
	  (cairo:rectangle cr 0.0 0.0 width height)
	  (cairo:fill cr)
	  (render-the-plot (gffi::foreign-location cr) cl-plplot-window width height)))))

(defun new-clg-plot (cl-plplot-window &key (title "clg-plot") (width-request 480) (height-request 320))
  "Creates a window containing the plot."
  (let* ((menu-bar (make-instance 'menu-bar))
	 (graph-area (make-instance 'drawing-area))
	 (main-window (make-instance 'window
				     :title title :name "clg_plot_window"
				     :width-request width-request :height-request height-request
				     :visible t))
	 (drawing-function (create-drawing-function graph-area cl-plplot-window)))
    
    ; menu setup
    (let ((menu (make-instance 'menu-item :label "File"))
	  (sub-menu (make-instance 'menu)))
      (let ((redraw-item (make-instance 'menu-item :label "Redraw"))
	    (quit-item (make-instance 'menu-item :label "Quit")))
	(signal-connect redraw-item 'activate #'(lambda ()
						  (funcall drawing-function)))
	(signal-connect quit-item 'activate #'(lambda ()
						(widget-destroy main-window)))
	(menu-shell-append sub-menu redraw-item)
	(menu-shell-append sub-menu quit-item))
      (setf (menu-item-submenu menu) sub-menu)
      (menu-shell-append menu-bar menu))

    ; graphics setup
    (signal-connect graph-area 'expose-event
		    #'(lambda (event)
			(declare (ignore event))
			(funcall drawing-function)))

    ; window setup
    (make-instance 'v-box
		   :parent main-window
		   :child (list menu-bar :expand nil :fill nil)
		   :child graph-area)
    (signal-connect main-window 'destroy #'(lambda ()
					     (setf main-window nil)))
    (widget-show-all main-window)))

(clg-init)

;;;;
;;;; Copyright (c) 2008 Hazen P. Babcock
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
