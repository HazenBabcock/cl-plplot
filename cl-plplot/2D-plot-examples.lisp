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
;;;; hazen 3/06
;;;;

(defpackage :cl-plplot-examples
  (:use :common-lisp
	:cl-plplot))

(in-package :cl-plplot-examples)

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
  (let ((x (my-make-vector 40 #'(lambda(x) (* 0.1 x))))
	(y (my-make-vector 40 #'(lambda(x) (* (* 0.1 x) (* 0.1 x))))))
    (with-2D-graph (g-dev)
      (x-y-plot x y))))


;; Here we add our own labels to the plot, change the size & add another piece of data with
;; a heavier red line.

(defun basic-plot-2 ()
  (let ((x (my-make-vector 40 #'(lambda(x) (* 0.1 x))))
	(y (my-make-vector 40 #'(lambda(x) (* (* 0.1 x) (* 0.1 x))))))
    (with-2D-graph (g-dev :x-size 400 :y-size 300 :x-label "x axis" :y-label "y axis" :title "my graph")
      (x-y-plot x y)
      (x-y-plot x x :color :red :line 2))))


;; Here we change the background and foreground colors & the x axis ticks & the 
;; y axis format and the x axis font size.
;;
;; Note: We reset *options* so that the next plot won't have the same funny axis
;; ticks marks, etc.

(defun basic-plot-3 ()
  (let ((x (my-make-vector 40 #'(lambda(x) (* 0.1 x))))
	(y (my-make-vector 40 #'(lambda(x) (* (* 0.1 x) (* 0.1 x))))))
    (with-2D-graph (g-dev :x-fontsize 2.0)
      (set-background-color :black)
      (set-foreground-color (vector 255 0 0))
      (set-axis-ticks :x :tick-interval 0.5 :number-subticks 10)
      (set-axis-format :x :draw-bottom/left :major-tick-grid :invert-ticks :major-tick-labels-above/right :major-ticks :minor-ticks)
      (x-y-plot x y :color (vector 0 255 0)))
    (reset-*options*)))


;; Here we plot one set of data as points & the other as a dashed blue line

(defun basic-plot-4 ()
  (let ((x (my-make-vector 40 #'(lambda(x) (* 0.1 x))))
	(y (my-make-vector 40 #'(lambda(x) (* (* 0.1 x) (* 0.1 x))))))
    (with-2D-graph (g-dev)
      (x-y-plot x y :line 0 :symbol-type 1)
      (x-y-plot x x :color :blue :line-style 2))))


;; Here we make a simple plot & then get the x-y coordinates of the next mouse
;; click (on the plot). Note that the coordinate scale for the mouse click location
;; is the same as those on the axises of the graph.

(defun basic-plot-5 ()
  (let ((x (my-make-vector 40 #'(lambda(x) (* 0.1 x))))
	(y (my-make-vector 40 #'(lambda(x) (* (* 0.1 x) (* 0.1 x))))))
    (with-2D-graph (g-dev)
      (x-y-plot x y)
      (multiple-value-bind (mx my) (get-cursor)
	(format t "You clicked : <~,2f, ~,2f>~%" mx my)))))


;; Here we make a plot with some error bars in x & y
;; Note that error bar is drawn with the total length given by the error bar
;; vector & centered on the data point.

(defun basic-plot-6 ()
  (let ((x (my-make-vector 40 #'(lambda(x) (* 0.1 x))))
	(y (my-make-vector 40 #'(lambda(x) (* (* 0.1 x) (* 0.1 x)))))
	(x-err (my-make-vector 40 #'(lambda(x) (declare (ignore x)) 0.06)))
	(y-err (my-make-vector 40 #'(lambda(x) (declare (ignore x)) 1.0))))
    (with-2D-graph (g-dev)
      (x-y-plot x y :line 0 :x-error x-err :y-error y-err))))


;; Here we output the graph to a file device rather than a display device

(defun basic-plot-7 ()
  (let ((x (my-make-vector 40 #'(lambda(x) (* 0.1 x))))
	(y (my-make-vector 40 #'(lambda(x) (* (* 0.1 x) (* 0.1 x))))))
    (with-2D-graph (f-dev :file-name "test.pbm")
      (x-y-plot x y))))


;; Here we make a simple bar graph

(defun bar-graph-1 ()
  (let ((data (my-make-bar-graph-data 10 5)))
    (with-2D-graph (g-dev)
      (bar-graph data :line 2))))


;; The same graph with the bars side-by-side

(defun bar-graph-2 ()
  (let ((data (my-make-bar-graph-data 10 5)))
    (with-2D-graph (g-dev)
      (bar-graph data :filled nil :side-by-side t))))


;; Specify our own scaling for the x and y axises

(defun bar-graph-3 ()
  (let ((x (my-make-vector 10 #'(lambda(x) (* 0.1 x))))
	(data (my-make-bar-graph-data 10 3)))
    (with-2D-graph (g-dev)
      (bar-graph data :x-data x :ymin -0.1 :ymax 20.0 :side-by-side t))))


;; Labeling an axis with the text of your choice, add a some text labels into the graph

(defun bar-graph-4 ()
  (let ((x (my-make-vector 50 #'(lambda(x) (* 0.1 x))))
	(hype (my-make-vector 50 #'(lambda(x) (* 0.01 x x))))
	(reality (my-make-vector 4 #'(lambda(x) (+ (sqrt x) 1.0)))))
    (with-2D-graph (g-dev :x-label "year" :y-label "profits" :title "Company X")
      (set-axis-format :x :draw-bottom/left :draw-top/right :major-ticks :minor-ticks)
      (bar-graph reality :ymin 0.0 :ymax 10.0 :colors '(:blue))
      (x-y-plot x hype :line 2 :color :red)
      (add-text-label "hype" 2.1 4.9 :delta-x 0.1 :delta-y 0.5)
      (add-text-label "reality" 3.0 3.0)
      (text-label-axis (vector "2001" "2002" "2003" "2004") (vector 0.125 0.375 0.625 0.875)
		       :orientation :perpendicular :offset 2.0))
    (reset-*options*)))


;; Combining a bar graph and a x-y plot, fill the bars with the color grey

(defun combo-graph-1 ()
  (let ((x (my-make-vector 20 #'(lambda(x) (* 0.1 x))))
	(y (my-make-vector 20 #'(lambda(x) (- (* 0.1 (- x 5.0) (- x 5.0)) 3.0))))
	(e (my-make-vector 20 #'(lambda(x) (declare (ignore x)) 0.5))))
    (with-2D-graph (g-dev)
      (bar-graph y :x-data x :colors '(:grey))
      (x-y-plot x y :line 0 :y-error e))))




