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
;;;;
;;;; cl-plplot system declaration
;;;;
;;;; hazen 6/06
;;;;

(in-package #:cl-user)

(defpackage #:cl-plplot.asdf
  (:use #:cl
        #:asdf))

(in-package #:cl-plplot.asdf)

(defsystem #:cl-plplot
  :name "cl-plplot"
  :author "Hazen Babcock <hbabcockos1@mac.com>"
  :version "0.2.0"
  :licence "MIT"
  :description "Interface to the PLplot Scientific Plotting Library"
  :components 
  ((:module
    :src/system
    :components ((:file "loadlib")
                 (:file "defcfun" :depends-on ("loadlib"))
                 (:file "misc" :depends-on ("defcfun"))
                 (:file "api" :depends-on ("misc"))))
   (:module
    :src/window 
    :components ((:file "package")
                 (:file "utility-functions" :depends-on ("package"))
    		 (:file "macros" :depends-on ("package"))
		 (:file "classes" :depends-on ("macros"))
		 (:file "color-table" :depends-on ("classes"))
                 (:file "extended-color-table" :depends-on ("classes"))
		 (:file "text-item" :depends-on ("classes"))
		 (:file "text-label" :depends-on ("text-item"))
		 (:file "axis-label" :depends-on ("text-item"))
		 (:file "axis" :depends-on ("axis-label"))
		 (:file "plot" :depends-on ("classes"))
		 (:file "x-y-plot" :depends-on ("plot"))
		 (:file "bar-graph" :depends-on ("plot"))
    		 (:file "contour-plot" :depends-on ("plot"))
		 (:file "window" :depends-on ("plot" "axis" "color-table" "extended-color-table")))
    :depends-on (:src/system)))
  :depends-on (:cffi))
