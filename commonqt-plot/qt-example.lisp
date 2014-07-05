;;;;
;;;; An example of how to integrate cl-plplot with commonqt.
;;;;
;;;; hazen 07/14
;;;;

(defpackage :qt-example
  (:use :cl :qt))

(in-package :qt-example)
(named-readtables:in-readtable :qt)

(pushnew #P"/home/hbabcock/OpenSource/cl-plplot/cl-qt-plot/" cffi::*foreign-library-directories* :test #'equal)
(ensure-smoke :plplotqt)

(defclass qplot ()
  ()
  (:metaclass qt-class)
  (:qt-superclass "QMainWindow"))

(defmethod initialize-instance :after ((instance qplot) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (let ((plot (#_new QtExtWidget 600 400 instance)))
    (#_setWindowTitle instance "Qt Example")
    (#_setCentralWidget instance plot)))

(defun main ()
  (with-main-window (window (make-instance 'qplot))
    (#_setGeometry window 100 100 600 400)))

  ;(format t "hello?")
  ;(with-main-window (window (#_new QWidget))))

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
