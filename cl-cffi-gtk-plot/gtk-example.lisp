;;;;
;;;; An example of how to integrate cl-plplot with cl-cffi-gtk.
;;;;
;;;; This uses the PLplot "extcairo" driver.
;;;;
;;;; With the "extcairo" device used here we have to do all the
;;;;   work for interactive selection ourselves. This is in
;;;;   contrast with the "extqt" PLplot device (used in the
;;;;   commonqt-plot example) which takes care of this for us.
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
	:cl-plplot-system
	:common-lisp))

(in-package :cl-cffi-gtk-plot)

(defun make-float-array (length &optional (initial-value 0.0))
  (make-array length :element-type 'float :initial-element initial-value))

(defun button-release (window event plot-func)
  (labels ((scale (x o1 o2 f1 f2)
	     (cond
	       ((< x o1) f1)
	       ((> x o2) f2)
	       (t (+ (* (/ (- x o1) (- o2 o1)) (- f2 f1)) f1)))))
    (let ((x (/ (gdk-event-button-x event) (gdk-window-get-width window)))
	  (y (- 1 (/ (gdk-event-button-y event) (gdk-window-get-height window)))))
      (if (eql plot-func #'plot-curves)
	  (if (< y 0.5)
	      (values (scale x 0.05 0.95 0.0 360.0)
		      (scale y 0.05 0.45 -1.2 1.2))
	      (values (scale x 0.05 0.95 -1.0 1.0)
		      (scale y 0.55 0.95 -1.0 1.0)))
	  (values (scale x 0.16 0.9 1980 1990)
		  (scale y 0.1 0.9 0 35.0))))))

(defun plot-curves (window cr)

  ; Initialize PLplot extcairo plotting device.
  (plsdev "extcairo")
  (plspage 0 0 (gdk-window-get-width window) (gdk-window-get-height window) 0 0)
  (plinit)
  (pl-cmd 26 cr)

  ; 1st plot
  (let ((x (make-float-array 360))
	(s (make-float-array 360))
	(c (make-float-array 360)))
    (dotimes (i 360)
      (setf (aref x i) i)
      (setf (aref s i) (sin (/ (* 3.14159 i) 180.0)))
      (setf (aref c i) (cos (/ (* 3.14159 i) 180.0))))
    (pladv 0)
    (plvpor 0.05 0.95 0.05 0.45)
    (plwind 0.0 360.0 -1.2 1.2)

    (plcol0 2)
    (plbox "bcnst" 0 0 "bcnst" 0 0)

    (plcol0 1)
    (plwidth 2)
    (plline x s)

    (plcol0 3)
    (plwidth 1)
    (pllsty 2)
    (plline x c)
    (pllsty 1)

    (plcol0 2)
    (plmtex "t" 1.0 0.5 0.5 "Sines"))

  ; 2nd plot
  (let ((x (make-float-array 201))
	(s (make-float-array 201))
	(c (make-float-array 201)))
    (dotimes (i 201)
      (setf (aref x i) (+ -1.0 (/ i 100.0)))
      (setf (aref s i) (* (aref x i) (aref x i)))
      (setf (aref c i) (* (aref s i) (aref x i))))
    (plvpor 0.05 0.95 0.55 0.95)
    (plwind -1.0 1.0 -1.0 1.0)
    
    (plcol0 2)
    (plbox "bcnst" 0 0 "bcnst" 0 0)

    (plcol0 1)
    (plwidth 2)
    (plline x s)

    (plcol0 3)
    (plwidth 1)
    (pllsty 2)
    (plline x c)
    (pllsty 1)

    (plcol0 2)
    (plmtex "t" 1.0 0.5 0.5 "Square & Cubic"))

  (plend))

(defun plot-histogram (window cr)

  ; Initialize PLplot extcairo plotting device.
  (plsdev "extcairo")
  (plspage 0 0 (gdk-window-get-width window) (gdk-window-get-height window) 0 0)
  (plinit)
  (pl-cmd 26 cr)

  (labels ((plfbox (x0 y0)
	     (let ((x (vector x0 x0 (1+ x0) (1+ x0)))
		   (y (vector 0.0 y0 y0 0.0)))
	       (plfill x y)
	       (plcol0 1)
	       (pllsty 1)
	       (plline x y))))
    (let ((y0 (vector 5 15 12 24 28 30 20 8 12 3))
	  (pos (vector 0.0 0.25 0.5 0.75 1.0))
	  (red (vector 0.0 0.25 0.5 1.0 1.0))
	  (green (vector 1.0 0.5 0.5 0.5 1.0))
	  (blue (vector 1.0 1.0 0.5 0.25 0.0)))

      (pladv 0)
      (plvsta)
      
      (plcol0 2)

      (plwind 1980.0 1990.0 0.0 35.0)
      (plbox "bc" 1.0 0 "bcnv" 10.0 0)
      (plcol0 2)
      (pllab "Year" "Widget Sales (millions)" "#frPLplot Example 12")

      (plscmap1l 1 pos red green blue nil)
      
      (dotimes (i 10)
	(plcol1 (/ i 9.0))
	(plpsty 0)
	(plfbox (+ 1980 i) (aref y0 i))
	(plptex (+ 1980 i 0.5) (1+ (aref y0 i)) 1.0 0.0 0.5 (format nil "~,0f" (aref y0 i)))
	(plmtex "b" 1.0 (- (* (1+ i) 0.1) 0.05) 0.5 (format nil "~d" (+ 1980 i))))))

  (plend))
    
(defun main ()
  (within-main-loop
    (setf (gtk-settings-gtk-shell-shows-menubar (gtk-settings-get-default))
	  nil)
    (let ((window (make-instance 'gtk-window
				 :type :toplevel
				 :title "GTK Example"))
	  (box (make-instance 'gtk-box
			      :orientation :vertical
			      :spacing 0))
	  (plot-box (make-instance 'gtk-drawing-area
				   :expand t
				   :width-request 600 
				   :height-request 600))
	  (plot-func #'plot-curves)
	  (interactive nil)
	  (mouse-x 10)
	  (mouse-y 10))

      ; Main window
      (gtk-container-add window box)
      (g-signal-connect window "destroy"
			(lambda (widget)
			  (declare (ignore widget))
			  (leave-gtk-main)))
 
      ; Menus
      (let ((menu-bar (make-instance 'gtk-menu-bar))
	    (plot-item (gtk-menu-item-new-with-label "Plot")))
	(gtk-box-pack-start box menu-bar :expand nil)
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
	    (g-signal-connect curves-item "activate"
			      (lambda (widget)
				(declare (ignore widget))
				(setf plot-func #'plot-curves)
				(gtk-widget-queue-draw box)))
	    (g-signal-connect histogram-item "activate"
			      (lambda (widget)
				(declare (ignore widget))
				(setf plot-func #'plot-histogram)
				(gtk-widget-queue-draw box)))
	    (g-signal-connect interactive-item "activate"
			      (lambda (widget)
				(declare (ignore widget))
				(setf interactive t)
				(gdk-window-set-cursor (gtk-widget-window plot-box) (gdk-cursor-new :cross))))
	    (g-signal-connect quit-item "activate"
			      (lambda (widget)
				(declare (ignore widget))
				(gtk-widget-destroy window))))))

      ; Plotting
      (gtk-box-pack-start box plot-box)
      (gtk-widget-add-events plot-box '(:button-press-mask :button-release-mask :pointer-motion-mask))
      (g-signal-connect plot-box "draw"		     
			(lambda (widget cr)
			  (let ((cr (pointer cr))
				(plot-window (gtk-widget-window widget)))
			    ; Clear surface
			    (cairo-set-source-rgb cr 0 0 0)
			    (cairo-paint cr)
			    
			    ; Plot
			    (funcall plot-func plot-window cr)

			    ; Draw crossed lines
			    (when interactive
			      (let ((w-height (gdk-window-get-height plot-window))
				    (w-width (gdk-window-get-width plot-window)))
				(cairo-set-source-rgb cr 0.8 0.8 0.8)
				(cairo-set-line-width cr 1.0)
				(cairo-move-to cr mouse-x 2)
				(cairo-line-to cr mouse-x (- w-height 2))
				(cairo-move-to cr 2 (- w-height mouse-y))
				(cairo-line-to cr (- w-width 2) (- w-height mouse-y))
				(cairo-stroke cr)))

			    ; Cleanup
			    (cairo-destroy cr))))
      (g-signal-connect plot-box "motion-notify-event"
			(lambda (widget event)
			  (declare (ignore widget))
			  (when interactive
			    (setf mouse-x (gdk-event-motion-x event))
			    (setf mouse-y (gdk-event-motion-y event))
			    (gtk-widget-queue-draw box))))
      (g-signal-connect plot-box "button-release-event"
			(lambda (widget event)
			  (let ((plot-window (gtk-widget-window widget)))
			    (when interactive
			      (setf interactive nil)
			      (gdk-window-set-cursor (gtk-widget-window plot-box) nil)
			      (multiple-value-bind (x y) (button-release plot-window event plot-func)
				(let ((m-dialog (gtk-message-dialog-new nil
									'(:modal)
									:info
									:close
									(format nil "Selection: (~,2f ~,2f)" x y))))
				  (gtk-dialog-run m-dialog)
				  (gtk-widget-destroy m-dialog))))

			    (gtk-widget-queue-draw box))))
    
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
