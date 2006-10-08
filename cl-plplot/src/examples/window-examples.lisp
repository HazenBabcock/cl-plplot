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
;;;; Examples that demonstrate using cl-plplot to make 2D plots.
;;;;
;;;; hazen 6/06
;;;;

(defpackage :window-examples
  (:use :common-lisp
	:cl-plplot))

(in-package :window-examples)


;;; Helper functions

(defun my-make-vector (dim init-fn)
  (let ((vec (make-array dim :initial-element 0.0 :element-type 'float)))
    (dotimes (i dim)
      (setf (aref vec i) (funcall init-fn i)))
    vec))

(defun my-make-matrix (dim1 dim2 init-fn)
  (let ((mat (make-array (list dim1 dim2) :initial-element 0.0 :element-type 'float)))
    (dotimes (x dim1)
      (dotimes (y dim2)
	(setf (aref mat x y) (funcall init-fn x y))))
    mat))

(defun my-make-bar-graph-data (rows cols)
  (let ((data (make-array (list rows cols) :initial-element 0.0 :element-type 'float)))
    (dotimes (i rows)
      (dotimes (j cols)
	(setf (aref data i j) (+ i j))))
    data))

(defun my-contour-plot-fn (x y)
  (let ((tx (- (* 0.02 x) 0.5))
	(ty (- (* 0.02 y) 0.5)))
    (- (* tx tx) (* ty ty)
       (* (sin (* 7 tx)) (* (cos (* 7 ty)))))))


;; You may need to change these to reflect the plplot drivers available on your system
;; If the Slime REPL hangs when you run one of these examples, it may be because the device
;; was not available. When this happens you should be able to specify a different device 
;; in the inferior lisp buffer.

(defparameter g-dev "aqt")
(defparameter f-dev "pbm")


;;; X-Y-Plots

;; The simplest plot

(defun basic-plot-1 ()
  (let* ((x (my-make-vector 40 #'(lambda(x) (* 0.1 x))))
	 (y (my-make-vector 40 #'(lambda(x) (* (* 0.1 x) (* 0.1 x)))))
	 (p (new-x-y-plot x y))
	 (w (basic-window)))
    (add-plot-to-window w p)
    (render w g-dev)))


;; Here we add our own labels to the plot, change the size & add another piece of data with
;; a heavier red line.

(defun basic-plot-2 ()
  (let* ((x (my-make-vector 40 #'(lambda(x) (* 0.1 x))))
	 (y (my-make-vector 40 #'(lambda(x) (* (* 0.1 x) (* 0.1 x)))))
	 (p1 (new-x-y-plot x y))
	 (p2 (new-x-y-plot x x :color :red :line-width 2))
	 (w (basic-window :x-label "x" :y-label "y" :title "my graph")))
    (add-plot-to-window w p1)
    (add-plot-to-window w p2)
    (render w g-dev :size-x 400 :size-y 300)))


;; Here we change the background and foreground colors & the x axis ticks & the 
;; y axis format and the x axis font size.

(defun basic-plot-3 ()
  (let* ((x (my-make-vector 40 #'(lambda(x) (* 0.1 x))))
	 (y (my-make-vector 40 #'(lambda(x) (* (* 0.1 x) (* 0.1 x)))))
	 (p1 (new-x-y-plot x y :color :blue))
	 (w (basic-window :title "" :foreground-color :red :background-color :black)))
    (edit-window-axis w :x :major-tick-interval 0.5 :minor-tick-number 10
		      :properties '(:draw-bottom/left :major-tick-grid :invert-ticks :major-tick-labels-above/right :major-ticks :minor-ticks))
    (add-plot-to-window w p1)
    (render w g-dev)))


;; Here we demonstrate some of the text capabilities.

(defun basic-plot-4 ()
  (let ((w (basic-window))
	(l1 (new-text-label (new-text-item (roman-font "x" (superscript "2") "!") :font-size 2.0 :text-color :blue) 0.5 0.2))
	(l2 (new-text-label (new-text-item (roman-font "test1 " (italic-font "test2 ") "test3") :font-size 2.0 :text-color :red) 0.5 0.4))
	(l3 (new-text-label (new-text-item (roman-font (unicode-char "967") (unicode-char "968")) :font-size 2.0 :text-color :green) 0.5 0.6)))
    (add-text-label-to-window w l1)
    (add-text-label-to-window w l2)
    (add-text-label-to-window w l3)
    (render w g-dev)))


;; Here we plot one set of data as points & the other as a dashed blue line.

(defun basic-plot-5 ()
  (let* ((x (my-make-vector 40 #'(lambda(x) (* 0.1 x))))
	 (y (my-make-vector 40 #'(lambda(x) (* (* 0.1 x) (* 0.1 x)))))
	 (p1 (new-x-y-plot x y :line-width 0 :symbol-size 6.0 :symbol-type 1))
	 (p2 (new-x-y-plot x x :color :blue :line-style 2))
	 (w (basic-window)))
    (add-plot-to-window w p1)
    (add-plot-to-window w p2)
    (render w g-dev)))


;; Here we make a simple plot & then get the x-y coordinates of the next mouse
;; click (on the plot). Note that the coordinate scale for the mouse click location
;; is the same as those on the axises of the graph. Once we have the mouse
;; location we generate a new graph with the line going through this point
;; by taking advantage of the fact that by setting copy to :nil we have told
;; x-y-plot to store a reference to the vectors x & y rather then copying x & y.

(defun basic-plot-6 ()
  (let* ((x (my-make-vector 40 #'(lambda(x) (* 0.1 x))))
	 (y (my-make-vector 40 #'(lambda(x) (* (* 0.1 x) (* 0.1 x)))))
	 (p (new-x-y-plot x y :copy nil))
	 (w (basic-window)))
    (add-plot-to-window w p)
    (multiple-value-bind (mx my) (get-cursor w g-dev)
      (format t "You clicked : <~,2f, ~,2f>~%" mx my)
      (setf (aref x 20) mx)
      (setf (aref y 20) my))
    (render w g-dev)))


;; Here we make a plot with some error bars in x & y
;; Note that error bar is drawn with the total length given by the error bar
;; vector & centered on the data point.

(defun basic-plot-7 ()
  (let* ((x (my-make-vector 40 #'(lambda(x) (* 0.1 x))))
	 (y (my-make-vector 40 #'(lambda(x) (* (* 0.1 x) (* 0.1 x)))))
	 (x-err (my-make-vector 40 #'(lambda(x) (declare (ignore x)) 0.06)))
	 (y-err (my-make-vector 40 #'(lambda(x) (declare (ignore x)) 1.0)))
	 (p (new-x-y-plot x y :x-error x-err :y-error y-err))
	 (w (basic-window)))
    (add-plot-to-window w p)
    (render w g-dev)))


;; Here we make our own color table with 2-3 colors, set window to use our new 
;; color table instead of the default & then change the foreground color in 
;; the color table.
;;
;; See also: src/window/color-table.lisp for a brief introduction of color handling.

(defun basic-plot-8 ()
  (let* ((c (new-color-table (vector 0 0 0 :color1)))
	 (x (my-make-vector 40 #'(lambda(x) (* 0.1 x))))
	 (y (my-make-vector 40 #'(lambda(x) (* (* 0.1 x) (* 0.1 x)))))
	 (p (new-x-y-plot x y))
	 (w (basic-window)))
    (add-plot-to-window w p)
    (add-color-to-color-table c (vector 255 0 0 :color2))
    (set-color-table w c)
    (render w g-dev)
    (add-color-to-color-table c (vector 255 255 255 :color3))
    (edit-x-y-plot p :color :color2)
    (change-foreground-color c :color1)
    (change-background-color c :color3)
    (render w g-dev)
    (remove-color-from-color-table c :color2)))


;;; Bar graphs

;; Here we make a simple bar graph

(defun bar-graph-1 ()
  (let* ((y (my-make-vector 10 #'(lambda(x) (* (* 0.2 x) (* 0.2 x)))))
	 (b (new-bar-graph nil y :fill-colors (vector :grey)))
	 (w (basic-window)))
    (add-plot-to-window w b)
    (render w g-dev)))


;; Stacked bar graph

(defun bar-graph-2 ()
  (let* ((y (my-make-bar-graph-data 10 3))
	 (b (new-bar-graph nil y :line-colors (vector :black :black :black)))
	 (w (basic-window)))
    (add-plot-to-window w b)
    (render w g-dev)))


;; A Side by side bar graph

(defun bar-graph-3 ()
  (let* ((y (my-make-bar-graph-data 10 3))
	 (b (new-bar-graph nil y :side-by-side t :line-colors (vector :black :black :black)))
	 (w (basic-window)))
    (add-plot-to-window w b)
    (render w g-dev)))


;; Bar graph with custom spacing & widths

(defun bar-graph-4 ()
  (let* ((x (my-make-vector 10 #'(lambda(x) (* 0.1 x))))
	 (y (my-make-vector 10 #'(lambda(x) (- (* (* 0.2 x) (* 0.2 x)) 1))))
	 (s (my-make-vector 10 #'(lambda(x) (+ 0.05 (* 0.005 x)))))
	 (b (new-bar-graph x y :bar-widths s :fill-colors (vector :grey)))
	 (w (basic-window)))
    (add-plot-to-window w b)
    (render w g-dev)))


;; A side by side bar graph with custom widths

(defun bar-graph-5 ()
  (let* ((y (my-make-bar-graph-data 10 3))
	 (s (my-make-vector 10 #'(lambda(x) (+ 0.1 (* 0.05 (sqrt x))))))
	 (b (new-bar-graph nil y :bar-widths s :side-by-side t :line-colors (vector :black :black :black)))
	 (w (basic-window)))
    (add-plot-to-window w b)
    (render w g-dev)))


;;; Contour Plots


;; A simple contour plot

(defun contour-plot-1 ()
  (let ((c (new-contour-plot (my-make-matrix 50 50 #'(lambda (x y) (my-contour-plot-fn x y)))
			     :line-color :blue :line-width 2))
	(w (basic-window)))
    (add-plot-to-window w c)
    (render w g-dev)))


;; The same plot rescaled with filled contours

(defun contour-plot-2 ()
  (let ((c (new-contour-plot (my-make-matrix 50 50 #'(lambda (x y) (my-contour-plot-fn x y)))
			     :x-min 0.0 :x-max 1.0 :y-min 0.0 :y-max 1.0 :fill-type :block
			     :fill-colors (vector :red :grey :blue :yellow :green)))
	(w (basic-window)))
    (add-plot-to-window w c)
    (render w g-dev)))


;; Plotted on a user defined simple grid with smooth shading between contours

(defun contour-plot-3 ()
  (let* ((xp (my-make-vector 50 #'(lambda(x) (+ (* 0.1 x) (* 0.01 x x)))))
	 (yp (my-make-vector 50 #'(lambda(y) (+ (* 0.1 y) (* 0.001 y y)))))
	 (c (new-contour-plot (my-make-matrix 50 50 #'(lambda (x y) (my-contour-plot-fn x y)))
			      :x-mapping xp :y-mapping yp :fill-type :smooth))
	 (w (basic-window)))
    (add-plot-to-window w c)
    (render w g-dev)))


;; Plotted on a more complex user defined grid

(defun contour-plot-4 ()
  (let* ((xp (my-make-matrix 51 51 #'(lambda(x y)
				       (+ (* 0.02 (- x 25) (* 0.01 (+ y 50)))))))
	 (yp (my-make-matrix 51 51 #'(lambda(x y)
				       (declare (ignore x))
				       (* 0.02 y))))
	 (cl (my-make-vector 20 #'(lambda(x) (- (* 0.12 x) 1.0))))
	 (c (new-contour-plot (my-make-matrix 51 51 #'(lambda (x y) (my-contour-plot-fn x y)))
			      :x-mapping xp :y-mapping yp :contour-levels cl))
	 (w (basic-window)))
    (add-plot-to-window w c)
    (render w g-dev)))
