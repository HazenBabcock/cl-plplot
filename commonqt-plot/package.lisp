;;;;
;;;;
;;;; Define the commonqt-plot package. This is a convenience package that has
;;;; everything set up so that one can use commonqt and cl-plplot-system together.
;;;;
;;;; hazen 7/14
;;;;

(in-package :cl-user)

(defpackage :commonqt-plot
  (:use :common-lisp
	:qt
	:cl-plplot-system))

(in-package :commonqt-plot)

; Load commonqt reader macro.
(named-readtables:in-readtable :qt)

; Load the plplotqt smoke library (libsmokeplplotqt.so on linux).
(pushnew (asdf:system-source-directory :commonqt-plot) cffi::*foreign-library-directories* :test #'equal)
(ensure-smoke :plplotqt)

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
