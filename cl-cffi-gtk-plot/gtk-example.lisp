;;;;
;;;; An example of how to integrate cl-plplot with cl-cffi-gtk.
;;;;
;;;; hazen 12/14
;;;;

(asdf:load-system :cl-cffi-gtk)

(defpackage :cl-cffi-gtk-plot
  (:use :gtk 
	:gdk 
	:gdk-pixbuf 
	:gobject
	:glib 
	:gio 
	:pango 
	:cairo 
	:common-lisp))

(in-package :cl-cffi-gtk-plot)

(defun create-menus (window box)
  (let ((menu-bar (gtk-menu-bar-new))
	(plot-item (gtk-menu-item-new-with-label "Plot")))
    (gtk-box-pack-start box menu-bar)
    (gtk-menu-shell-append menu-bar plot-item)
    (let ((plot-menu (gtk-menu-new)))
      (setf (gtk-menu-item-submenu plot-item) plot-menu)
      (let ((curves-item (gtk-menu-item-new-with-label "Curves"))
	    (histogram-item (gtk-menu-item-new-with-label "Histogram"))
	    (interactive-item (gtk-menu-item-new-with-label "Interactive Selection"))
	    (quit-item (gtk-menu-item-new-with-label "Quit")))
	(gtk-menu-shell-append plot-menu curves-item)
	(gtk-menu-shell-append plot-menu histogram-item)
	(gtk-menu-shell-append plot-menu interactive-item)
	(gtk-menu-shell-append plot-menu (gtk-separator-menu-item-new))
	(gtk-menu-shell-append plot-menu quit-item)
	(g-signal-connect quit-item "activate"
			  (lambda (widget)
			    (declare (ignore widget))
			    (gtk-widget-destroy window)))))))

(defun plot (window cr)
  ;; Clear surface
  (cairo-set-source-rgb cr 1.0 1.0 1.0)
  (cairo-paint cr)
  ;; Example is in 1.0 x 1.0 coordinate space
  (cairo-scale cr
	       (gdk-window-get-width window)
	       (gdk-window-get-height window))
  ;; Drawing code goes here
  (cairo-set-line-width cr 0.1)
  (cairo-set-source-rgb cr 1.0 0.0 0.0)
  (cairo-rectangle cr 0.25 0.25 0.5 0.5)
  (cairo-stroke cr)
  ;; Destroy the Cario context
  (cairo-destroy cr)
  t)
  
(defun main ()
  (within-main-loop
    (setf (gtk-settings-gtk-shell-shows-menubar (gtk-settings-get-default))
	  nil)
    (let ((window (make-instance 'gtk-window
				 :type :toplevel
				 :title "GTK Example"))
	  (box (make-instance 'gtk-box
			      :orientation :vertical
			      :spacing 0)))

      (gtk-container-add window box)
      (g-signal-connect window "destroy"
			(lambda (widget)
			  (declare (ignore widget))
			  (leave-gtk-main)))
 
      (create-menus window box)

      (let ((plot-box (make-instance 'gtk-drawing-area
				     :width-request 600
				     :height-request 600)))
	(gtk-box-pack-start box plot-box)
	(g-signal-connect plot-box "draw"
			  (lambda (widget cr)
			    (let ((cr (pointer cr))
				  (plot-window (gtk-widget-window widget)))
			      (plot plot-window cr)))))

      (gtk-widget-show-all window))))

;;;;
;;;; Copyright (c) 2014 Hazen P. Babcock
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
