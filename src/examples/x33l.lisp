;;;;
;;;; PLplot example 33
;;;;
;;;; hazen 02/14
;;;;

(in-package :plplot-examples)

(defun example33 (&optional (dev default-dev))
  (plsdev dev)

  ;; legend examples.
  (let ((position-options (vector (+ pl-position-left pl-position-top pl-position-outside)
				  (+ pl-position-top pl-position-outside)
				  (+ pl-position-right pl-position-top pl-position-outside)
				  (+ pl-position-right pl-position-outside)
				  (+ pl-position-right pl-position-bottom pl-position-outside)
				  (+ pl-position-bottom pl-position-outside)
				  (+ pl-position-left pl-position-bottom pl-position-outside)
				  (+ pl-position-left pl-position-outside)
				  (+ pl-position-left pl-position-top pl-position-inside)
				  (+ pl-position-top pl-position-inside)
				  (+ pl-position-right pl-position-top pl-position-inside)
				  (+ pl-position-right pl-position-inside)
				  (+ pl-position-right pl-position-bottom pl-position-inside)
				  (+ pl-position-bottom pl-position-inside)
				  (+ pl-position-left pl-position-bottom pl-position-inside)
				  (+ pl-position-left pl-position-inside))))
    (plinit)

    ; page 1
    (pladv 0)
    (plvpor 0.25 0.75 0.25 0.75)
    (plwind 0.0 1.0 0.0 1.0)
    (plbox "bc" 0.0 0 "bc" 0.0 0)
    (plsfont pl-fci-sans -1 -1)
    (plmtex "t" 8.0 0.5 0.5 "The 16 standard legend positions with")
    (plmtex "t" 6.0 0.5 0.5 "the same (0.05) offset in x and y")
    (plsfont pl-fci-mono -1 -1)
    (plscol0a 15 32 32 32 0.7)
    (dotimes (k 16)
      (pllegend (+ pl-legend-background pl-legend-bounding-box) (aref position-options k) 
		0.05 0.05 0.1 15 
		1 1 0 0
		(vector (+ pl-legend-line pl-legend-symbol)) 
		1.0 1.0 2.0
		1 (vector (1+ (mod k 8))) (vector (format nil "~2,'0d" k))
		nil nil nil nil
		(vector (1+ (mod k 8))) (vector 1) (vector 1.0)
		(vector (1+ (mod k 8))) (vector 1) (vector 4) (vector "#(728)")))

    ; page 2
    (pladv 0)
    (plvpor 0.25 0.75 0.25 0.75)
    (plwind 0.0 1.0 0.0 1.0)
    (plbox "bc" 0.0 0 "bc" 0.0 0)
    (plsfont pl-fci-sans -1 -1)
    (plmtex "t" 8.0 0.5 0.5 "The effect of nrow, ncolumn, PL_LEGEND_ROW_MAJOR,")
    (plmtex "t" 6.0 0.5 0.5 "and position for the same legend data")
    (let ((opt-base (+ pl-legend-background pl-legend-bounding-box))
	  (opt-array (make-int-array 7 (+ pl-legend-line pl-legend-symbol)))
	  (line-styles (make-int-array 7 1))
	  (line-widths (make-float-array 7 1.0))
	  (symbol-scales (make-float-array 7 1.0))
	  (symbol-numbers (make-int-array 7 2))
	  (symbols (vector "#(728)" "#(728)" "#(728)" "#(728)" "#(728)" "#(728)" "#(728)"))
	  (text (vector "" "" "" "" "" "" ""))
	  (text-colors (make-int-array 7 0))
	  (line-colors (make-int-array 7 0))
	  (symbol-colors (make-int-array 7 0)))
      (dotimes (k 7)
	(setf (aref text k) (format nil "~2,'0d" k)
	      (aref text-colors k) (1+ (mod k 8))
	      (aref line-colors k) (1+ (mod k 8))
	      (aref symbol-colors k) (1+ (mod k 8))))
      (plsfont pl-fci-mono -1 -1)
      (plscol0a 15 32 32 32 0.7)
      (pllegend opt-base (+ pl-position-top pl-position-outside) 0.0 0.1
		0.05 15 1 1 1 7
		opt-array 1.0 1.0 2.0
		1 text-colors text
		nil nil nil nil
		line-colors line-styles line-widths
		symbol-colors symbol-scales symbol-numbers symbols)
      (pllegend opt-base (+ pl-position-bottom pl-position-outside) 0.0 0.1
		0.05 15 1 1 1 7
		opt-array 1.0 1.0 2.0
		1 text-colors text
		nil nil nil nil
		line-colors line-styles line-widths
		symbol-colors symbol-scales symbol-numbers symbols)
      (pllegend opt-base (+ pl-position-left pl-position-outside) 0.1 0.0
		0.05 15 1 1 7 1
		opt-array 1.0 1.0 2.0
		1 text-colors text
		nil nil nil nil
		line-colors line-styles line-widths
		symbol-colors symbol-scales symbol-numbers symbols)
      (pllegend opt-base (+ pl-position-right pl-position-outside) 0.1 0.0
		0.05 15 1 1 7 1
		opt-array 1.0 1.0 2.0
		1 text-colors text
		nil nil nil nil
		line-colors line-styles line-widths
		symbol-colors symbol-scales symbol-numbers symbols)
      (pllegend opt-base (+ pl-position-left pl-position-top pl-position-inside) 0.0 0.0
		0.05 15 1 1 6 2
		opt-array 1.0 1.0 2.0
		1 text-colors text
		nil nil nil nil
		line-colors line-styles line-widths
		symbol-colors symbol-scales symbol-numbers symbols)
      (pllegend (+ opt-base pl-legend-row-major) (+ pl-position-right pl-position-top pl-position-inside) 0.0 0.0
		0.05 15 1 1 6 2
		opt-array 1.0 1.0 2.0
		1 text-colors text
		nil nil nil nil
		line-colors line-styles line-widths
		symbol-colors symbol-scales symbol-numbers symbols)

      (pllegend (+ opt-base pl-legend-row-major) (+ pl-position-bottom pl-position-inside) 0.0 0.0
		0.05 15 1 1 3 3
		opt-array 1.0 1.0 2.0
		1 text-colors text
		nil nil nil nil
		line-colors line-styles line-widths
		symbol-colors symbol-scales symbol-numbers symbols))

    ; page 3
    (pladv 0)
    (plvpor 0.0 1.0 0.0 0.9)
    (plwind 0.0 1.0 0.0 1.0)
    (plsfont pl-fci-sans -1 -1)
    (plmtex "t" 2.0 0.5 0.5 "Demonstrate legend alignment")
    (let* ((x 0.1)
	   (y 0.1)
	   (nturn 4)
	   (nlegend 0)
	   (nrow 0)
	   (ncolumn 0)
	   (position (+ pl-position-top pl-position-left pl-position-subpage))
	   (opt-base (+ pl-legend-background pl-legend-bounding-box))
	   (opt opt-base)
	   (line-styles (make-int-array 7 1))
	   (line-widths (make-float-array 7 1.0))
	   (symbol-scales (make-float-array 7 1.0))
	   (symbol-numbers (make-int-array 7 2))
	   (symbols (vector "#(728)" "#(728)" "#(728)" "#(728)" "#(728)" "#(728)" "#(728)"))
	   (text (vector "" "" "" "" "" "" ""))
	   (text-colors (make-int-array 7 0))
	   (line-colors (make-int-array 7 0))
	   (symbol-colors (make-int-array 7 0)))
      (dotimes (i 9)
	(incf nlegend (if (<= i nturn) 1 -1))
	(setf nlegend (if (> nlegend 1) nlegend 1))
	(dotimes (k nlegend)
	  (setf (aref text k) (format nil "~2,'0d" k))
	  (setf (aref text-colors k) (1+ (mod k 8)))
	  (setf (aref line-colors k) (1+ (mod k 8)))
	  (setf (aref symbol-colors k) (1+ (mod k 8))))
	(plsfont pl-fci-mono -1 -1)
	(plscol0a 15 32 32 32 0.70)
	(setf nrow (if (< nlegend 3) nlegend 3))
	(multiple-value-bind (legend-width legend-height)
	    (pllegend opt position x y
		      0.025 15 1 1 nrow ncolumn
		      (make-int-array nlegend (+ pl-legend-line pl-legend-symbol))
		      1.0 1.0 1.5
		      1 text-colors text
		      nil nil nil nil
		      line-colors line-styles line-widths
		      symbol-colors symbol-scales symbol-numbers symbols)

	  (if (= i nturn)
	      (progn
		(setf position (+ pl-position-top pl-position-right pl-position-subpage))
		(setf opt opt-base)
		(setf x (- 1.0 x))
		(incf y legend-height))
	      (progn
		(incf x legend-width)
		(incf y legend-height))))))

    ; page 4

    (let* ((max-height 0.0)
	   (xstart 0.0)
	   (ystart 0.1)
	   (x xstart)
	   (y ystart)
           (text-scale 0.9)
	   (special-symbols (vector "✰" "✴" "✱" "✽" "✦"))
	   (position (+ pl-position-top pl-position-left))
	   (opt (+ pl-legend-background pl-legend-bounding-box pl-legend-text-left))
	   (opt-array (make-int-array 5 0))
	   (line-styles (make-int-array 7 1))
	   (line-widths (make-float-array 7 1.0))
	   (symbol-scales (make-float-array 7 1.0))
	   (symbol-numbers (make-int-array 7 2))
	   (symbols (vector "*" "*" "*" "*" "*" "*" "*"))
	   (text (vector "" "" "" "" "" "" ""))
	   (text-colors (make-int-array 7 0))
	   (line-colors (make-int-array 7 0))
	   (symbol-colors (make-int-array 7 0))
	   (box-colors (make-int-array 7 0))
	   (box-patterns (make-int-array 7 0))
	   (box-scales (make-float-array 7 0.0))
	   (box-line-widths (make-float-array 7 1.0)))
      (pladv 0)
      (plvpor 0.0 1.0 0.0 0.9)
      (plwind 0.0 1.0 0.0 1.0)
      (plsfont pl-fci-sans -1 -1)
      (plmtex "t" 2.0 0.5 0.5 "Demonstrate Various Kinds of Legends")

      (setf (aref opt-array 0) pl-legend-none)
      (setf (aref text 0) "None")
      (setf (aref text-colors 0) 1)

      (setf (aref opt-array 1) pl-legend-color-box)
      (setf (aref text 1) "Box")
      (setf (aref text-colors 1) 2)
      (setf (aref box-colors 1) 2)
      (setf (aref box-patterns 1) 0)
      (setf (aref box-scales 1) 0.8)
      (setf (aref box-line-widths 1) 1.0)

      (setf (aref opt-array 2) pl-legend-line)
      (setf (aref text 2) "Line")
      (setf (aref text-colors 2) 3)
      (setf (aref line-colors 2) 3)
      (setf (aref line-styles 2) 1)
      (setf (aref line-widths 2) 1.0)

      (setf (aref opt-array 3) pl-legend-symbol)
      (setf (aref text 3) "Symbol")
      (setf (aref text-colors 3) 4)
      (setf (aref symbol-colors 3) 4)
      (setf (aref symbol-scales 3) text-scale)
      (setf (aref symbol-numbers 3) 4)
      (setf (aref symbols 3) (aref special-symbols 2))

      (setf (aref opt-array 4) (+ pl-legend-symbol pl-legend-line))
      (setf (aref text 4) "L & S")
      (setf (aref text-colors 4) 5)
      (setf (aref line-colors 4) 5)
      (setf (aref line-styles 4) 1)
      (setf (aref line-widths 4) 1.0)
      (setf (aref symbol-colors 4) 5)
      (setf (aref symbol-scales 4) text-scale)
      (setf (aref symbol-numbers 4) 4)
      (setf (aref symbols 4) (aref special-symbols 2))

      (plscol0a 15 32 32 32 0.7)
      (multiple-value-bind (legend-width legend-height)
	  (pllegend opt position x y
		    0.1 15 1 1 0 0
		    opt-array 1.0 text-scale 2.0
		    0.0 text-colors text
		    box-colors box-patterns box-scales box-line-widths
		    line-colors line-styles line-widths
		    symbol-colors symbol-scales symbol-numbers symbols)
	(setf max-height (if (> legend-height max-height) legend-height max-height))
	(incf x legend-width))

      (dotimes (i 5)
	(setf (aref opt-array i) pl-legend-symbol)
	(setf (aref text i) (format nil "Symbol ~a" (aref special-symbols i)))
	(setf (aref text-colors i) (1+ i))
	(setf (aref symbol-colors i) (1+ i))
	(setf (aref symbol-scales i) text-scale)
	(setf (aref symbol-numbers i) 4)
	(setf (aref symbols i) (aref special-symbols i)))

      (multiple-value-bind (legend-width legend-height)
	  (pllegend opt position x y
		    0.1 15 1 1 0 0
		    opt-array 1.0 text-scale 2.0
		    0.0 text-colors text
		    nil nil nil nil
		    nil nil nil
		    symbol-colors symbol-scales symbol-numbers symbols)
	(setf max-height (if (> legend-height max-height) legend-height max-height))
	(incf x legend-width))

      (dotimes (i 5)
	(setf (aref opt-array i) pl-legend-symbol)
	(setf (aref text i) (format nil "Symbol Number ~a" (+ i 2)))
	(setf (aref text-colors i) (1+ i))
	(setf (aref symbol-colors i) (1+ i))
	(setf (aref symbol-scales i) text-scale)
	(setf (aref symbol-numbers i) (+ i 2))
	(setf (aref symbols i) (aref special-symbols 2)))

      (multiple-value-bind (legend-width legend-height)
	  (pllegend opt position x y
		    0.1 15 1 1 0 0
		    opt-array 1.0 text-scale 2.0
		    0.0 text-colors text
		    nil nil nil nil
		    nil nil nil
		    symbol-colors symbol-scales symbol-numbers symbols)
	(setf max-height (if (> legend-height max-height) legend-height max-height))
	(incf x legend-width))

      (dotimes (i 5)
	(setf (aref opt-array i) pl-legend-color-box)
	(setf (aref text i) (format nil "Box Color ~a" (+ i 1)))
	(setf (aref text-colors i) (1+ i))
	(setf (aref box-colors i) (1+ i))
	(setf (aref box-patterns i) 0)
	(setf (aref box-scales i) 0.8)
	(setf (aref box-line-widths i) 1.0))

      (setf x xstart)
      (incf y max-height)

      (multiple-value-bind (legend-width legend-height)
	  (pllegend opt position x y
		    0.1 15 1 1 0 0
		    opt-array 1.0 text-scale 2.0
		    0.0 text-colors text
		    box-colors box-patterns box-scales box-line-widths
		    nil nil nil
		    nil nil nil nil)
	(setf max-height (if (> legend-height max-height) legend-height max-height))
	(incf x legend-width))

      (dotimes (i 5)
	(setf (aref opt-array i) pl-legend-color-box)
	(setf (aref text i) (format nil "Box Pattern ~a" (+ i 1)))
	(setf (aref text-colors i) 2)
	(setf (aref box-colors i) 2)
	(setf (aref box-patterns i) i)
	(setf (aref box-scales i) 0.8)
	(setf (aref box-line-widths i) 1.0))

      (multiple-value-bind (legend-width legend-height)
	  (pllegend opt position x y
		    0.1 15 1 1 0 0
		    opt-array 1.0 text-scale 2.0
		    0.0 text-colors text
		    box-colors box-patterns box-scales box-line-widths
		    nil nil nil
		    nil nil nil nil)
	(setf max-height (if (> legend-height max-height) legend-height max-height))
	(incf x legend-width))

      (dotimes (i 5)
	(setf (aref opt-array i) pl-legend-color-box)
	(setf (aref text i) (format nil "Box Line Width ~a" (+ i 1)))
	(setf (aref text-colors i) 2)
	(setf (aref box-colors i) 2)
	(setf (aref box-patterns i) 3)
	(setf (aref box-scales i) 0.8)
	(setf (aref box-line-widths i) (1+ i)))

      (multiple-value-bind (legend-width legend-height)
	  (pllegend opt position x y
		    0.1 15 1 1 0 0
		    opt-array 1.0 text-scale 2.0
		    0.0 text-colors text
		    box-colors box-patterns box-scales box-line-widths
		    nil nil nil
		    nil nil nil nil)
	(setf max-height (if (> legend-height max-height) legend-height max-height))
	(incf x legend-width))

      (dotimes (i 5)
	(setf (aref opt-array i) pl-legend-line)
	(setf (aref text i) (format nil "Line Color ~a" (+ i 1)))
	(setf (aref text-colors i) (1+ i))
	(setf (aref line-colors i) (1+ i))
	(setf (aref line-styles i) 1)
	(setf (aref line-widths i) 1.0))

      (setf x xstart)
      (incf y max-height)

      (multiple-value-bind (legend-width legend-height)
	  (pllegend opt position x y
		    0.1 15 1 1 0 0
		    opt-array 1.0 text-scale 2.0
		    0.0 text-colors text
		    nil nil nil nil
		    line-colors line-styles line-widths
		    nil nil nil nil)
	(setf max-height (if (> legend-height max-height) legend-height max-height))
	(incf x legend-width))

      (dotimes (i 5)
	(setf (aref opt-array i) pl-legend-line)
	(setf (aref text i) (format nil "Line Style ~a" (+ i 1)))
	(setf (aref text-colors i) 2)
	(setf (aref line-colors i) 2)
	(setf (aref line-styles i) (1+ i))
	(setf (aref line-widths i) 1.0))

      (multiple-value-bind (legend-width legend-height)
	  (pllegend opt position x y
		    0.1 15 1 1 0 0
		    opt-array 1.0 text-scale 2.0
		    0.0 text-colors text
		    nil nil nil nil
		    line-colors line-styles line-widths
		    nil nil nil nil)
	(setf max-height (if (> legend-height max-height) legend-height max-height))
	(incf x legend-width))

      (dotimes (i 5)
	(setf (aref opt-array i) pl-legend-line)
	(setf (aref text i) (format nil "Line Width ~a" (+ i 1)))
	(setf (aref text-colors i) 2)
	(setf (aref line-colors i) 2)
	(setf (aref line-styles i) 1)
	(setf (aref line-widths i) (1+ i)))

      (multiple-value-bind (legend-width legend-height)
	  (pllegend opt position x y
		    0.1 15 1 1 0 0
		    opt-array 1.0 text-scale 2.0
		    0.0 text-colors text
		    nil nil nil nil
		    line-colors line-styles line-widths
		    nil nil nil nil)
	(setf max-height (if (> legend-height max-height) legend-height max-height))
	(incf x legend-width)))

    ;; color bar examples.
    (let ((colorbar-option-kinds (vector pl-colorbar-shade
					 (+ pl-colorbar-shade pl-colorbar-shade-label)
					 pl-colorbar-image
					 pl-colorbar-gradient))
	  (colorbar-option-kind-labels (vector "Shade colorbars"
					       "Shade colorbars with custom labels"
					       "Image colorbars"
					       "Gradient colorbars"))
	  (colorbar-position-options (vector pl-position-left
					     pl-position-right
					     pl-position-top
					     pl-position-bottom))
	  (colorbar-position-option-labels (vector "Left"
						   "Right"
						   "Top"
						   "Bottom"))
	  (colorbar-label-options (vector pl-colorbar-label-left
					  pl-colorbar-label-right
					  pl-colorbar-label-top
					  pl-colorbar-label-bottom))
	  (colorbar-label-option-labels (vector "Label left"
						"Label right"
						"Label top"
						"Label bottom"))
	  (colorbar-cap-options (vector pl-colorbar-cap-none
					pl-colorbar-cap-low
					pl-colorbar-cap-high
					(+ pl-colorbar-cap-low pl-colorbar-cap-high)))
	  (colorbar-cap-option-labels (vector "No caps"
					      "Low cap"
					      "High cap"
					      "Low and high caps"))
	  (values-small (vector -1.0d-200 1.0d-200))
	  (values-uneven (vector -1.0d-200 2.0d-200 2.6d-200 3.4d-200 6.0d-200 7.0d-200 8.0d-200 9.0d-200 10.0d-200))
	  (values-even (vector -2.0d-200 -1.0d-200 0.0d-200 1.0d-200 2.0d-200 3.0d-200 4.0d-200 5.0d-200 6.0d-200)))
      (labels ((plcolorbar-example-page (kind-i label-i cap-i cont-color cont-width n-values values)
		 (pladv 0)
		 (dotimes (position-i 4)
		   (let* ((ticks (vector 0.0))
			  (sub-ticks (vector 0.0))
			  (low-cap-color 0.0)
			  (high-cap-color 1.0)
			  (label-opts (vector 0))
			  (position (aref colorbar-position-options position-i))
			  (opt (+ (aref colorbar-option-kinds kind-i)
				  (aref colorbar-label-options label-i)
				  (aref colorbar-cap-options cap-i)))
			  (vertical (+ (logand position pl-position-left) (logand position pl-position-right)))
			  (ifn (+ (logand position pl-position-left) (logand position pl-position-bottom)))
			  (x (if (/= vertical 0) 0.0 0.0))
			  (y (if (/= vertical 0) 0.0 0.0))
			  (x-length (if (/= vertical 0) 0.05 0.5))
			  (y-length (if (/= vertical 0) 0.5 0.05))
			  (axis-opts (vector (if (/= ifn 0)
						 (if (or (= cont-color 0) (= cont-width 0.0))
						     "uwtivn"
						     "uwxvn")
						 (if (or (= cont-color 0) (= cont-width 0.0))
						     "uwtivm"
						     "uwxvm"))))
			  (alabel (vector (format nil "~a, ~a" 
						  (aref colorbar-position-option-labels position-i)
						  (aref colorbar-label-option-labels label-i))))
			  (values-arr (make-float-array (list 1 n-values))))
		     (plschr 0.0 0.75)
		     (plsmaj 0.0 0.5)
		     (plsmin 0.0 0.5)
		     
		     (plvpor 0.20 0.80 0.20 0.80)
		     (plwind 0.0 1.0 0.0 1.0)
		     (plscol0a 15 0 0 0 0.2)
		     (dotimes (i n-values)
		       (setf (aref values-arr 0 i) (aref values i)))

		     ; This will throw a floating point exception depending on which output device you use.
		     (handler-case
			 (plcolorbar (+ opt pl-colorbar-bounding-box pl-colorbar-background) position
				     x y x-length y-length
				     15 1 1
				     low-cap-color high-cap-color
				     cont-color cont-width
				     label-opts alabel
				     axis-opts
				     ticks sub-ticks
				     (vector n-values) values-arr)
		       (error () (format t "plcolorbar problem~%")))

		     (plschr 0.0 1.0)
		     (plsmaj 0.0 1.0)
		     (plsmin 0.0 1.0))
		   (plvpor 0.0 1.0 0.0 1.0)
		   (plwind 0.0 1.0 0.0 1.0)
		   (plptex 0.5 0.5 0.0 0.0 0.5 (format nil "~a - ~a"
						       (aref colorbar-option-kind-labels kind-i)
						       (aref colorbar-cap-option-labels cap-i)))))
	       (plcolorbar-example (palette kind-i cont-color cont-width n-values values)
		 (plspal1 palette 1)
		 (dotimes (label-i 4)
		   (dotimes (cap-i 4)
		     (plcolorbar-example-page kind-i label-i cap-i cont-color cont-width n-values values)))))
	(plscolbg 70 185 70)
	(plscmap1-range 0.01 0.99)

	(dotimes (i 2)
	  (plcolorbar-example "cmap1_blue_yellow.pal" (+ i 2) 0 0 2 values-small))
	(dotimes (i 2)
	  (plcolorbar-example "cmap1_blue_yellow.pal" i 4 2 9 values-even))
	(dotimes (i 2)
	  (plcolorbar-example "cmap1_blue_yellow.pal" i 0 0 9 values-uneven))))

    (plend1)))

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
