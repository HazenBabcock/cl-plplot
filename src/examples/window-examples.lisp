;;;;
;;;; Examples that demonstrate using cl-plplot to make 2D & 3D plots.
;;;;
;;;; hazen 12/06
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

(defparameter g-dev "xwin")

;;; X-Y-Plots

;; The simplest plot

(defun basic-plot-1 ()
  (let* ((x (my-make-vector 40 #'(lambda(x) (* 0.1 x))))
	 (y (my-make-vector 40 #'(lambda(x) (* (* 0.1 x) (* 0.1 x)))))
	 (p (new-x-y-plot x y))
	 (w (basic-window)))
    (add-plot-to-window w p)
    (render w g-dev)))

;; The same plot with a user defined y-axis range

(defun basic-plot-1.1 ()
  (let* ((x (my-make-vector 40 #'(lambda(x) (* 0.1 x))))
	 (y (my-make-vector 40 #'(lambda(x) (* (* 0.1 x) (* 0.1 x)))))
	 (p (new-x-y-plot x y))
	 (w (basic-window :y-axis-min -5.0 :y-axis-max 50.0)))
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
;    (render w "png" :filename "/Users/hbabcock/test.png" :size-x 400 :size-y 300)))


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
;;
;; Note that once you click you also have to press return..

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
    (edit-x-y-plot p :color :color3)
    (edit-window w :foreground-color :color1 :background-color :color2)
    (render w g-dev)
    (remove-color-from-color-table c :color2)
    nil))


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


;; The same as contour-plot-3, but with a gray scale color table.

(defun contour-plot-5 ()
  (let* ((ct (new-extended-color-table :control-points (vector #(0.0 0 0 0) #(1.0 255 255 255))))
	 (xp (my-make-vector 50 #'(lambda(x) (+ (* 0.1 x) (* 0.01 x x)))))
	 (yp (my-make-vector 50 #'(lambda(y) (+ (* 0.1 y) (* 0.001 y y)))))
	 (c (new-contour-plot (my-make-matrix 50 50 #'(lambda (x y) (my-contour-plot-fn x y)))
			      :x-mapping xp :y-mapping yp :fill-type :smooth))
	 (w (basic-window)))
    (add-plot-to-window w c)
    (set-color-table w ct)
    (render w g-dev)))


;; Use PLplot's ability to grid data to convert your (x,y,z) data into a
;; plottable 2D grid.

(defun contour-plot-6 ()
  (let* ((x (my-make-vector 1000 #'(lambda(x)
				    (declare (ignore x))
				    (- (random 4.0) 2.0))))
	 (y (my-make-vector 1000 #'(lambda(y)
				    (declare (ignore y))
				    (- (random 4.0) 2.0))))
	 (z (make-array 1000 :initial-element 0.0 :element-type 'float))
	 (xgrid (my-make-vector 21 #'(lambda(x) (- (* 0.2 x) 2.0))))
	 (ygrid (my-make-vector 21 #'(lambda(x) (- (* 0.2 x) 2.0))))
	 (p (new-x-y-plot x y :line-width 0 :symbol-type 2 :symbol-size 0.75))
	 (w (basic-window)))
    (dotimes (i (length z))
      (let ((tx (aref x i))
	    (ty (aref y i)))
	(setf (aref z i) (- (* tx tx) (* ty ty) (* (sin tx) (* (cos ty)))))))
    (let* ((d (x-y-z-data-to-grid (list x y z) xgrid ygrid :algorithm :grid-nnli))
	   (c (new-contour-plot d :x-mapping xgrid :y-mapping ygrid :fill-type :block)))
      (add-plot-to-window w c)
      (add-plot-to-window w p)
      (render w g-dev))))


;; Mixing different plot types is also possible, though care must be taken
;; to draw them in the right order.

(defun mixed-plot-1 ()
  (let* ((x (my-make-vector 40 #'(lambda(x) (* 0.1 x))))
	 (y (my-make-vector 40 #'(lambda(x) (* (* 0.1 x) (* 0.1 x)))))
	 (p (new-x-y-plot x y :line-width 2))
	 (c (new-contour-plot (my-make-matrix 50 50 #'(lambda (x y) (my-contour-plot-fn x y)))
			      :x-min 0.0 :x-max 4.0 :y-min 0.0 :y-max 15.0 :fill-type :block
			      :fill-colors (vector :red :grey :blue :yellow :green)))
	 (title (new-text-item "..." :font-size 1.5)) ; create a text object for the title
	 (l (new-axis-label title :top 1.5))          ; create an axis label containing the title object
	 (w (basic-window)))
    (add-plot-to-window w p)
    (add-plot-to-window w c)
    (edit-window w :title l)  ; replace the default title object with our own title object
    (edit-text-item title :the-text "Wrong Order?") ; change the text in the title object
    (render w g-dev)
    (bring-to-front w p)
    (edit-text-item title :the-text "Right Order?")
    (render w g-dev)))


;; Roll your own custom plot type & have it get drawn like any other plot type

(defun custom-plot-type-1 ()
  (let ((cp (new-custom-plot-object
	     #'(lambda ()
		 (vector 0.0 4.0 0.0 4.0))
	     #'(lambda (plot-number)
		 (declare (ignore plot-number))
		 (set-foreground-color :red)
		 (cl-plplot-system:plfill (vector 1.0 1.2 2.8 3.0)
					  (vector 1.0 3.0 3.0 1.0)))))
	(w (basic-window)))
    (add-plot-to-window w cp)
    (render w g-dev)))


;;; 3D mesh plots

;; A simple 3D mesh plot

(defun 3d-plot-1 ()
  (let ((c (new-3d-mesh nil nil (my-make-matrix 50 50 #'(lambda (x y) (my-contour-plot-fn x y)))
			:line-color :blue))
	(w (basic-3d-window :altitude 30 :azimuth 60)))
    (add-plot-to-window w c)
    (render w g-dev)))

;; The same plot with a custom z axis range

(defun 3d-plot-1.1 ()
  (let ((c (new-3d-mesh nil nil (my-make-matrix 50 50 #'(lambda (x y) (my-contour-plot-fn x y)))
			:line-color :blue))
	(w (basic-3d-window :z-axis-min -2.0 :z-axis-max 2.0 :altitude 30 :azimuth 60)))
    (add-plot-to-window w c)
    (render w g-dev)))

;; The same plot with (default) cantours drawn in the x-y plane

(defun 3d-plot-2 ()
  (let ((c (new-3d-mesh nil nil (my-make-matrix 50 50 #'(lambda (x y) (my-contour-plot-fn x y)))
			:line-color :blue :contour-options :base-contour))
	(w (basic-3d-window :altitude 30 :azimuth 60)))
    (add-plot-to-window w c)
    (render w g-dev)))


;; The same plot with (default) cantours drawn in the x-y plane and magnitude
;; coloring on the plot. Additionally, only draw lines in between points in
;; the x direction.

(defun 3d-plot-3 ()
  (let ((c (new-3d-mesh nil nil (my-make-matrix 50 50 #'(lambda (x y) (my-contour-plot-fn x y)))
			:grid-type :grid-x :contour-options :both))
	(w (basic-3d-window :altitude 30 :azimuth 60)))
    (add-plot-to-window w c)
    (render w g-dev)))


;; The same plot with magnitude coloring on the plot. Additionally a "curtain"
;; is drawn around the plot.

(defun 3d-plot-4 ()
  (let ((c (new-3d-mesh nil nil (my-make-matrix 50 50 #'(lambda (x y) (my-contour-plot-fn x y)))
			:contour-options :magnitude-contour :curtain t))
	(w (basic-3d-window :altitude 30 :azimuth 60)))
    (add-plot-to-window w c)
    (render w g-dev)))


;; A simple demonstration 3D text labels.

(defun 3d-plot-5 ()
  (let ((l1 (new-3D-text-label (new-text-item (roman-font "Label1") :font-size 2.0 :text-color :blue) 0.5 1.0 0.1
			       :text-dy -1.0))
	(l2 (new-3D-text-label (new-text-item (roman-font "Label2") :font-size 2.0 :text-color :red) 1.5 1.0 0.1
			       :text-dx 1.0
			       :text-sz 1.0))
	(w (basic-3d-window :altitude 30 :azimuth 60
			    :x-axis-min 0 :x-axis-max 2.0
			    :y-axis-min 0 :y-axis-max 2.0
			    :z-axis-min 0 :z-axis-max 2.0)))
    (add-text-label-to-window w l1)
    (add-text-label-to-window w l2)
    (render w g-dev)))


;;; Surface plots

;; A simple surface plot

(defun surface-plot-1 ()
  (let ((c (new-surface-plot nil nil (my-make-matrix 50 50 #'(lambda (x y) (my-contour-plot-fn x y)))
			:line-color :blue))
	(w (basic-3d-window :altitude 30 :azimuth 60)))
    (add-plot-to-window w c)
    (render w g-dev)))

;; The same plot with a curtain and coloring according to the magnitude in z

(defun surface-plot-2 ()
  (let ((c (new-surface-plot nil nil (my-make-matrix 50 50 #'(lambda (x y) (my-contour-plot-fn x y)))
			     :surface-options '(:curtain :magnitude-coloring)))
	(w (basic-3d-window :altitude 30 :azimuth 60)))
    (add-plot-to-window w c)
    (render w g-dev)))


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
