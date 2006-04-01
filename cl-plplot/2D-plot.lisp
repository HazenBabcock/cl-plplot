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
;;;; An attempt at a more lispy interface to plplot. At the moment this
;;;; just makes making some 2D graphs a little simpler. I hope to expand
;;;; this in the future & suggestions are welcome.
;;;;
;;;; In its current form the PLplot library seems to only allow you to draw in only one 
;;;; window at a time with no way to switch between graphing windows. This is reflected
;;;; here where you have to finish your current plot before you can start the next one.
;;;;
;;;; hazen 3/06
;;;;

(defpackage #:cl-plplot
  (:use #:common-lisp
	#:cl-plplot-system)
  (:export #:initialize
	   #:cleanup
	   #:reset-*options*
	   #:set-background-color
	   #:set-foreground-color
	   #:set-axis-ticks
	   #:set-axis-format
	   #:x-y-plot
	   #:get-cursor
	   #:set-axis-labels
	   #:set-device&size
	   #:with-2D-graph))

(in-package #:cl-plplot)

(defstruct options
  "Current plplot options"
  (background-color nil)
  (foreground-color nil)
  (xtick 0)
  (nxsub 0)
  (xopt "bcnst")
  (ytick 0)
  (nysub 0)
  (yopt "bcnst"))

(defparameter *options* nil)


;;; helpers

(defun symbol-to-color (symbol)
  "Translates symbol into a color"
  (cond
    ((equal symbol :black) (vector 0 0 0))
    ((equal symbol :red) (vector 240 50 50))
    ((equal symbol :yellow) (vector 255 255 0))
    ((equal symbol :green) (vector 0 255 0))
    ((equal symbol :aquamarine) (vector 127 255 212))
    ((equal symbol :pink) (vector 255 192 203))
    ((equal symbol :wheat) (vector 245 222 179))
    ((equal symbol :grey) (vector 190 190 190))
    ((equal symbol :brown) (vector 165 42 42))
    ((equal symbol :blue) (vector 0 0 255))
    ((equal symbol :blue-violet) (vector 138 43 226))
    ((equal symbol :cyan) (vector 0 255 255))
    ((equal symbol :turquoise) (vector 64 224 208))
    ((equal symbol :magenta) (vector 255 0 255))
    ((equal symbol :salmon) (vector 250 128 114))
    ((equal symbol :white) (vector 255 255 255))
    (t (progn
	 (format t "Unrecognized color: ~A~%" symbol)
	 (vector 0 0 0)))))

(defun min-max (vector)
  "Returns the minimum and maximum of vector"
  (let ((min (aref vector 0))
	(max (aref vector 0))
	(len (length vector)))
    (dotimes (i len)
      (when (> (aref vector i) max)
	(setf max (aref vector i)))
      (when (< (aref vector i) min)
	(setf min (aref vector i))))
    (values min max)))

(defmacro check-min-max (min max vec)
  "Sets min and max from vec unless they are already both defined"
  (let ((g0 (gensym))
	(g1 (gensym))
	(g2 (gensym)))
    `(unless (and ,min ,max)
       (multiple-value-bind (,g0 ,g1) (min-max ,vec)
	 (let ((,g2 (* 0.05 (- ,g1 ,g0))))
	   (setf ,min (- ,g0 ,g2))
	   (setf ,max (- ,g1 ,g2)))))))

(defun error-bar (vec error)
  "Creates error bar vectors from vec to pass to plplot"
  (let* ((len (length vec))
	 (min-err (make-array len :element-type 'double-float))
	 (max-err (make-array len :element-type 'double-float)))
    (dotimes (i (length vec))
      (let ((err (* 0.5 (aref error i))))
	(setf (aref min-err i) (coerce (- (aref vec i) err) 'double-float))
	(setf (aref max-err i) (coerce (+ (aref vec i) err) 'double-float))))
    (values min-err max-err)))

(defmacro color-setter (slot color)
  "Simplifies setting the foreground and background colors"
  `(cond 
     ((symbolp ,color) (setf ,slot (symbol-to-color ,color)))
     ((and (vectorp ,color) (= 3 (length ,color))) (setf ,slot ,color))
     (t (format t "Malformed color option: ~A~%" ,color))))

(defun set-color (which vec &optional (change-pen t))
  "Sets the color..."
  (plscol0 which (aref vec 0) (aref vec 1) (aref vec 2))
  (when change-pen
    (plcol0 which)))

(defun len-check (x y x-error y-error)
  (let ((good-dims t)
	(x-length (length x)))
    (unless (= x-length (length y))
      (format t "x & y do not have the same number of elements!~%")
      (setf good-dims nil))
    (when (and x-error
	       (not (= x-length (length x-error))))
      (format t "x & x-error do not have the same number of elements!~%")
      (setf good-dims nil))
    (when (and y-error
	       (not (= x-length (length y-error))))
      (format t "x & y-error do not have the same number of elements!~%")
      (setf good-dims nil))
    good-dims))

;;; exported

(defun reset-*options* ()
  "Restores user options to their defaults"
  (setf (options-background-color *options*) (symbol-to-color :white))
  (setf (options-foreground-color *options*) (symbol-to-color :black))
  (setf (options-xtick *options*) 0)
  (setf (options-nxsub *options*) 0)
  (setf (options-xopt *options*) "bcnst")
  (setf (options-ytick *options*) 0)
  (setf (options-nysub *options*) 0)
  (setf (options-yopt *options*) "bcnst")
  nil)

(let ((currently-plotting nil)
      (initialized-plot nil))
  
  (defun initialize ()
  "Sets *options* for the first time, if that hasn't already been done.
   This should not be called directly, it is a helper for the macro with-2D-graph."
    (unless *options*
      (setf *options* (make-options))
      (reset-*options*))
    (if currently-plotting
	(progn
	  (format t "You must finish the current plot before starting a new plot!")
	  nil)
	(progn
	  (setf currently-plotting t)
	  t)))

  (defun cleanup ()
    "Cleans up at the end of a plotting session. 
     This should not be called directly, it is a helper for the macro with-2D-graph."
    (setf currently-plotting nil)
    (setf initialized-plot nil)
    (plend)))

(defun set-background-color (color)
  "Sets the default background color, must be called before x-y-plot"
  (color-setter (options-background-color *options*) color))

(defun set-foreground-color (color)
  "Sets the default foreground color, must be called before x-y-plot"
  (color-setter (options-foreground-color *options*) color))

(defun set-axis-ticks (axis &key (tick-interval 0) (number-subticks 0))
  "Sets the ticks interval and the number of subticks, must be called before x-y-plot"
  (when (not (= 0 tick-interval))
    (if (equal axis :x)
	(setf (options-xtick *options*) tick-interval)
	(setf (options-ytick *options*) tick-interval)))
  (when (not (= 0 number-subticks))
    (if (equal axis :x)
	(setf (options-nxsub *options*) number-subticks)
	(setf (options-nysub *options*) number-subticks))))

(defun set-axis-format (axis &rest settings)
  "Sets the how axis :x or :y is will look based on symbol list settings.
   If you don't want to get the defaults, call this before you call x-y-plot."
  (when settings
    (let ((opt-string ""))
      (labels ((my-cat (char)
		 (setf opt-string (concatenate 'string opt-string (string char)))))
	(dolist (setting settings)
	  (cond
	    ((equal setting :draw) (my-cat "a"))
	    ((equal setting :draw-bottom/left) (my-cat "b"))
	    ((equal setting :draw-top/right) (my-cat "c"))
	    ((equal setting :fixed-point) (my-cat "f"))
	    ((equal setting :major-tick-grid) (my-cat "g"))
	    ((equal setting :minor-tick-grid) (my-cat "h"))
	    ((equal setting :invert-ticks) (my-cat "i"))
	    ((equal setting :log-axis) (my-cat "l"))
	    ((equal setting :major-tick-labels-above/right) (my-cat "m"))
	    ((equal setting :major-tick-labels-below/left) (my-cat "n"))
	    ((equal setting :minor-ticks) (my-cat "s"))
	    ((equal setting :major-ticks) (my-cat "t"))
	    (t (format t "Unrecognized setting ~A~%" setting)))))
      (if (equal axis :x)
	  (setf (options-xopt *options*) opt-string)
	  (setf (options-yopt *options*) opt-string)))))

(let ((currently-plotting nil)
      (initialized-plot nil))
  
  (defun initialize ()
  "Sets *options* for the first time, if that hasn't already been done.
   This should not be called directly, it is a helper for the macro with-2D-graph."
    (unless *options*
      (setf *options* (make-options))
      (reset-*options*))
    (if currently-plotting
	(progn
	  (format t "You must finish the current plot before starting a new plot!")
	  nil)
	(progn
	  (setf currently-plotting t)
	  t)))

  (defun cleanup ()
    "Cleans up at the end of a plotting session. 
     This should not be called directly, it is a helper for the macro with-2D-graph."
    (setf currently-plotting nil)
    (setf initialized-plot nil)
    (plend))

  (defun x-y-plot (x y &key (xmin nil) (xmax nil) (ymin nil) (ymax nil) (line 1) (line-style 1) (symbol-type nil) (color nil) (x-error nil) (y-error nil))
    "Plots the vector x versus the vector y.
     Arguments:
       xmin, xmax, ymin & ymax can be used to specify the axis ranges on the *first* call to this
          function. If they are not specified then default values are chosen based on x & y.
       line should be an integer line width, or zero if no line is desired
       line-style specified what style line to draw (if a line is drawn), this should be a number
          between 1 and 8.
       symbol-type should be set to a number (that specifies a symbol) if you want symbols plotted.
       color is the color to use when plotting the lines and symbols, it should be a symbol
          that specifies a default color (see symbol-to-color) or a RGB triple (0-255). If it
          is not specified the current foreground color will be used.
       x-error should be a vector of the same length as x that contains the size of the error
          bars in x.
       y-error is for error bars in y."
    (unless initialized-plot
      (setf initialized-plot t)
      (check-min-max xmin xmax x)
      (check-min-max ymin ymax y)
      (set-color 0 (options-background-color *options*) nil)
      (plinit)
      (set-color 1 (options-foreground-color *options*))
      (pladv 0)
      (plvpor 0.1 0.9 0.1 0.9)
      (plwind xmin xmax ymin ymax)
      (plbox (options-xopt *options*) (options-xtick *options*) (options-nxsub *options*)
	     (options-yopt *options*) (options-ytick *options*) (options-nysub *options*)))
    (cond
      ((and color (symbolp color)) (set-color 1 (symbol-to-color color)))
      ((and color (vectorp color) (= 3 (length color))) (set-color 1 color))
      (t (set-color 1 (options-foreground-color *options*))))
    (when (len-check x y x-error y-error)
      (when (> line 0)
	(when (and (> line-style 0)
		   (< line-style 9))
	  (pllsty line-style))
	(plwid line)
	(plline x y))
      (when symbol-type
	(if (and (numberp symbol-type)
		 (>= symbol-type 0)
		 (< symbol-type 3000))
	    (plpoin x y symbol-type)
	    (format t "symbol-type must be an integer between 0 & 3000")))
      (when x-error
	(multiple-value-bind (min-error max-error) (error-bar x x-error)
	  (plerrx min-error max-error y)))
      (when y-error
	(multiple-value-bind (min-error max-error) (error-bar y y-error)
	  (plerry x min-error max-error))))))

(defun get-cursor ()
  "Returns the x & y coordinates (as defined by the current plot) of the next mouse click.
   This should be called at least one-call to x-y-plot or you will not have a graph to click on."
  (let ((mouse (plgetcursor)))
    (values (elt mouse 8) (elt mouse 9))))

(defun set-device&size (device file-name x-size y-size)
  "Initializes the plplot session with graphics output to device and window size (in pixels) sizex sizey.
   This should not be called directly, it is a helper for the macro with-2D-graph."
  (plsdev device)
  (when file-name
    (plsfile file-name))
  (plspage 0 0 x-size y-size 0 0)
  (plscmap0n 16)
  (plscmap1n 128))

(defun set-axis-labels (x-label x-fontsize y-label y-fontsize title title-fontsize)
  "Sets the labels on the plot axises.
   This should not be called directly, it is a helper for the macro with-2D-graph."
  (set-color 1 (options-foreground-color *options*))
  (plschr 0 title-fontsize)
  (plmtex "t" (/ 2.5 title-fontsize) 0.5 0.5 title)
  (plschr 0 x-fontsize)
  (plmtex "b" (/ 3.0 x-fontsize) 0.5 0.5 x-label)
  (plschr 0 y-fontsize)
  (plmtex "l" (/ 3.5 y-fontsize) 0.5 0.5 y-label))

(defmacro with-2D-graph ((dev &key (file-name nil) (x-size 600) (y-size 500) (x-label "x") (x-fontsize 1.0) (y-label "y") (y-fontsize 1.0) (title "cl-plplot") (title-fontsize 1.0)) &body body)
  "Handles creation & destruction of a 2D plplot graphing session"
  `(progn
     (when (cl-plplot:initialize)
       (unwind-protect
	    (progn
	      (cl-plplot:set-device&size ,dev ,file-name ,x-size ,y-size)
	      ,@body
	      (cl-plplot:set-axis-labels ,x-label ,x-fontsize ,y-label ,y-fontsize ,title ,title-fontsize))
	 (cl-plplot:cleanup)))))
