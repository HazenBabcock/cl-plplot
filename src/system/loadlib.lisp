;;;;
;;;; Defines the cl-plot package.
;;;; Loads the plplot library (libplplotd).
;;;;
;;;; hazen 3/06
;;;;

(in-package #:cl-user)

(defpackage #:cl-plplot-system
  (:use #:common-lisp
	#:cffi))

(in-package #:cl-plplot-system)

(defun load-libraries ()
  ; linux
  (pushnew #P"/usr/local/lib/" *foreign-library-directories* :test #'equal)
  ; OS-X / fink
  (pushnew #P"/sw/lib/" *foreign-library-directories* :test #'equal)
  ; OS-X / darwinports
  (pushnew #P"/opt/local/lib/" *foreign-library-directories* :test #'equal)

  (let ((lib-loaded nil))
    
    ; Try and load version 5.11+ PLplot library first.
    (handler-case
	(progn
	  (define-foreign-library libplplot
	    (t (:default "libplplot")))
	  (use-foreign-library libplplot)
	  (setf lib-loaded t))
      (error ()))

    ; Try and load version 5.10- PLplot library.
    (when (not lib-loaded)
      (define-foreign-library libplplot
	(t (:default "libplplotd")))
      (use-foreign-library libplplot))))

(load-libraries)

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
