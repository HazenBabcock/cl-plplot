;;;;
;;;; Checks that the lisp examples match the current PLplot examples.
;;;; This is designed to work on a unix like system. Temporary files 
;;;; may be created in your current working directory with the names 
;;;; temp_lisp* and temp_plplot*
;;;;
;;;; hazen 04/15
;;;;

(in-package :plplot-examples)

;(defparameter cmp-command "cmp -s -i 190 ")
(defparameter diff-command "diff -q ")
(defparameter lisp-file "temp_lisp.ps")
(defparameter plplot-examples-dir (concatenate 'string "/usr/local/share/plplot" (plgver) "/examples/c/"))
(defparameter plplot-file "temp_plplot.ps")
(defparameter rm-command "rm ")
(defparameter tail-command "tail -n +9 ")

(defun check-example (lisp-example-fn plplot-example)
  (let ((differs nil))

    ; Create lisp version of the example.
    (plsfnam lisp-file)
    (funcall lisp-example-fn "psc")

    ; Create plplot (C) version of the example.
    (trivial-shell:shell-command (concatenate 'string
					      plplot-examples-dir
					      plplot-example
					      " -dev psc -o "
					      plplot-file))

    ; Remove time stamps.
    (trivial-shell:shell-command (concatenate 'string
					      tail-command
					      lisp-file " > "
					      lisp-file "t"))

    (trivial-shell:shell-command (concatenate 'string
					      tail-command
					      plplot-file " > "
					      plplot-file "t"))

    ; Compare the outputs using diff.
    (when (> (length (trivial-shell:shell-command (concatenate 'string
							       diff-command
							       lisp-file "t "
							       plplot-file "t")))
	     0)
      (setf differs t))

    ; Remove temporary files if the files match.
    (if differs
      (format t "~%Lisp and plplot files differ~%")
      (trivial-shell:shell-command (concatenate 'string
						rm-command
						lisp-file " " lisp-file "t "
						plplot-file " " plplot-file "t")))

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
