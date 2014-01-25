;;;;
;;;; Define the classes that we'll use in cl-plplot
;;;;
;;;; hazen 6/06
;;;;

(in-package #:cl-plplot)

(def-plplot-class color-table ()
  (color-map))

(def-plplot-class extended-color-table ()
  (control-points
   color-table-size))

(def-plplot-class text-item ()
  (the-text
   (text-color *foreground-color*)
   (text-justification 0.5)
   (font-size *font-size*)))

(def-plplot-class text-label ()
  (label-text-item
   text-x
   text-y
   text-dx
   text-dy))

(def-plplot-class 3D-text-label (text-label)
  (text-z
   text-dz
   text-sx
   text-sy
   text-sz))

(def-plplot-class axis-label ()
  (axis-text-item
   side
   displacement
   (location 0.5)
   (orientation :parallel)))

(def-plplot-class 3D-axis-label (axis-label)
  ((primary/secondary :primary)))

(def-plplot-class axis ()
  (axis-min
   axis-max
   (major-tick-interval 0)
   (minor-tick-number 0)
   (properties *axis-properties*)
   axis-labels))

(def-plplot-class plot ()
  (min-max-function
   render-function))

(def-plplot-class x-y-plot (plot)
  (data-x
   data-y
   line-width
   line-style
   symbol-size
   symbol-type
   color
   x-error
   y-error))

(def-plplot-class bar-graph (plot)
  (data-x
   data-array
   bar-widths
   side-by-side
   line-colors
   fill-colors
   line-width
   Filled))

(def-plplot-class contour-plot (plot)
  (data
   contour-levels
   fill-colors
   fill-type
   line-color
   line-width
   x-mapping
   y-mapping))

(def-plplot-class 3D-plot (plot)
  (data-x
   data-y
   data-z
   contour-levels
   line-width
   line-style
   line-color))

(def-plplot-class 3D-mesh (3D-plot)
  (grid-type
   contour-options
   curtain))

(def-plplot-class surface-plot (3D-plot)
  (light-source
   surface-options))

(def-plplot-class window ()
  (x-axis
   y-axis
   title
   (window-line-width 1.0)
   (window-font-size *font-size*)
   (foreground-color *foreground-color*)
   (background-color *background-color*)
   (viewport-x-min 0.1)
   (viewport-x-max 0.9)
   (viewport-y-min 0.1)
   (viewport-y-max 0.9)
   plots
   text-labels
   color-table
   extended-color-table))

(def-plplot-class 3D-window (window)
  (z-axis
   (altitude 60)
   (azimuth 30)))

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
