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
;;;; Examples that demonstrate using cl-plplot to make 2D plots
;;;;
;;;; hazen 6/06
;;;;

(defpackage :window-examples
  (:use :common-lisp
	:cl-plplot))

(in-package :window-examples)

(defun my-make-vector (dim init-fn)
  (let ((vec (make-array dim :initial-element 0.0 :element-type 'float)))
    (dotimes (i dim)
      (setf (aref vec i) (funcall init-fn i)))
    vec))

(defun my-make-bar-graph-data (rows cols)
  (let ((data (make-array (list rows cols) :initial-element 0.0 :element-type 'float)))
    (dotimes (i rows)
      (dotimes (j cols)
	(setf (aref data i j) (+ i j))))
    data))

;; You may need to change these to reflect the plplot drivers available on your system
;; If the Slime REPL hangs when you run one of these examples, it may be because the device
;; was not available. When this happens you should be able to specify a different device 
;; in the inferior lisp buffer.

(defparameter g-dev "aqt")
(defparameter f-dev "pbm")


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
	 (p1 (new-x-y-plot x y :color (vector 0 255 0)))
	 (w (basic-window :title "" :foreground-color (vector 255 0 0) :background-color :black)))
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
