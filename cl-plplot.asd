;;;;
;;;; cl-plplot system declaration
;;;;
;;;; hazen 2/14
;;;;

(in-package #:cl-user)

(defpackage #:cl-plplot.asdf
  (:use #:cl
        #:asdf))

(in-package #:cl-plplot.asdf)

(defsystem #:cl-plplot
  :name "cl-plplot"
  :author "Hazen Babcock <hbabcock@mac.com>"
  :version "0.7.0"
  :licence "MIT"
  :description "Interface to the PLplot Scientific Plotting Library"
  :components 
  ((:module
    :src/system
    :components ((:file "loadlib")
		 (:file "types" :depends-on ("loadlib"))
                 (:file "pl-defcfun" :depends-on ("types"))
                 (:file "misc" :depends-on ("pl-defcfun"))
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
                 (:file "3D-mesh" :depends-on ("plot"))
                 (:file "surface-plot" :depends-on ("3D-mesh"))
		 (:file "window" :depends-on ("plot" "axis" "color-table" "extended-color-table"))
		 (:file "3D-window" :depends-on ("window")))
    :depends-on (:src/system))
   (:module
    :src/extras
    :components ((:file "vmessage"))
    :depends-on (:src/window)))
  :depends-on (:cffi))

(defsystem #:plplot-examples
  :name "plplot-examples"
  :author "Hazen Babcock <hbabcock@mac.com>"
  :version "0.1.0"
  :licence "MIT"
  :description "PLplot standard examples in Lisp"
  :components 
  ((:module
    :src/examples
    :components ((:file "plplot-examples")
		 (:file "x00l" :depends-on ("plplot-examples"))
                 (:file "x01l" :depends-on ("plplot-examples"))
                 (:file "x02l" :depends-on ("plplot-examples"))
                 (:file "x03l" :depends-on ("plplot-examples"))
                 (:file "x04l" :depends-on ("plplot-examples"))
                 (:file "x05l" :depends-on ("plplot-examples"))
                 (:file "x06l" :depends-on ("plplot-examples"))
                 (:file "x07l" :depends-on ("plplot-examples"))
                 (:file "x08l" :depends-on ("plplot-examples"))
                 (:file "x09l" :depends-on ("plplot-examples"))
                 (:file "x10l" :depends-on ("plplot-examples"))
                 (:file "x11l" :depends-on ("plplot-examples"))
                 (:file "x12l" :depends-on ("plplot-examples"))
                 (:file "x13l" :depends-on ("plplot-examples"))
                 (:file "x14l" :depends-on ("plplot-examples"))
                 (:file "x15l" :depends-on ("plplot-examples"))
                 (:file "x16l" :depends-on ("plplot-examples"))
                 (:file "x17l" :depends-on ("plplot-examples"))
                 (:file "x18l" :depends-on ("plplot-examples"))
                 (:file "x19l" :depends-on ("plplot-examples"))
                 (:file "x20l" :depends-on ("plplot-examples")) ; Requires cl-png
                 (:file "x21l" :depends-on ("plplot-examples")) ; SBCL specific NAN handling?
                 (:file "x22l" :depends-on ("plplot-examples"))
                 (:file "x23l" :depends-on ("plplot-examples"))
                 (:file "x24l" :depends-on ("plplot-examples")) ; Requires unicode handling.
                 (:file "x25l" :depends-on ("plplot-examples"))
                 (:file "x26l" :depends-on ("plplot-examples"))
                 (:file "x27l" :depends-on ("plplot-examples"))
                 (:file "x28l" :depends-on ("plplot-examples"))
                 (:file "x29l" :depends-on ("plplot-examples"))
                 (:file "x30l" :depends-on ("plplot-examples")) ; Can trigger floating point exceptions.
                 (:file "x31l" :depends-on ("plplot-examples"))
                 (:file "x32l" :depends-on ("plplot-examples"))
                 (:file "x33l" :depends-on ("plplot-examples"))))) ; Can trigger floating point exceptions.
  :depends-on (:cl-plplot :png))

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
