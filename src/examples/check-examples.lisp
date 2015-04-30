;;;;
;;;; Checks that the lisp examples match the current PLplot examples.
;;;; This is designed to work on a unix like system. Temporary files 
;;;; may be created in your current working directory with the names 
;;;; temp_lisp* and temp_plplot*
;;;;
;;;; This requires the cairo family of drivers and ImageMagick (to
;;;; do the picture comparison).
;;;;
;;;; hazen 04/15
;;;;

(in-package :plplot-examples)

(defparameter diff-command "compare -metric AE -fuzz 1% ")
(defparameter lisp-file "temp_lisp.png")
(defparameter plplot-examples-dir (concatenate 'string "/usr/local/share/plplot" (plgver) "/examples/c/"))
(defparameter plplot-file "temp_plplot.png")
(defparameter rm-command "rm ")
(defparameter tail-command "tail -n +9 ")

(defun check-example (lisp-example-fn plplot-example)
  (let ((differs nil))

    ; Remove old files.
    (trivial-shell:shell-command (concatenate 'string
					      rm-command
					      lisp-file "* "
					      plplot-file "* "
					      "temp_diff.png*"))
					      
    ; Create lisp version of the example.
    (plsfnam lisp-file)
    (multiple-value-bind (fam num bmax) (plgfam)
      (declare (ignore fam num))
      (plsfam 1 1 bmax))
    (funcall lisp-example-fn "pngcairo")

    ; Create plplot (C) version of the example.
    (trivial-shell:shell-command (concatenate 'string
					      plplot-examples-dir
					      plplot-example
					      " -dev pngcairo -fam -o "
					      plplot-file))

    ; Check that both examples have the same number of pages.
    (let ((lisp-npages (1- (do ((i 1 (+ i 1)))
			       ((not (probe-file (format nil "~a.~a" lisp-file i))) i))))
	  (plplot-npages (1- (do ((i 1 (+ i 1)))
				 ((not (probe-file (format nil "~a.~a" plplot-file i))) i)))))
      (when (not (= lisp-npages plplot-npages))
	(format t "Number of pages differ, ~a vs ~a.~%" lisp-npages plplot-npages)))

    ; Compare the outputs using image diff.
    (do ((i 1 (+ i 1)))
	((not (probe-file (format nil "~a.~a" lisp-file i))))
      (multiple-value-bind (ret output err-output exit-status)
	  (trivial-shell:shell-command (concatenate 'string
						    diff-command
						    lisp-file "." (write-to-string i) " " 
						    plplot-file "." (write-to-string i)
						    " temp_diff.png." (write-to-string i)))
	(declare (ignore ret err-output exit-status))
	(let ((diffs (parse-integer (string-trim '(#\space #\return #\newline) output))))
	  (when (not (= diffs 0))
	    (setf differs t)
	    (format t "~a pixels differ in page ~a.~%" (string-trim '(#\space #\return #\newline) output) i)))))
      
    differs))


;;;;
;;;; Copyright (c) 2015 Hazen P. Babcock
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
