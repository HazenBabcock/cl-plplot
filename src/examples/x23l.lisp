;;;;
;;;; PLplot example 23
;;;;
;;;; hazen 07/10
;;;;

(in-package :plplot-examples)

(defun example23 (&optional (dev default-dev))
  (plsdev dev)
  (let ((greek (vector "#gA" "#gB" "#gG" "#gD" "#gE" "#gZ" "#gY" "#gH" "#gI" "#gK" "#gL" "#gM"
		       "#gN" "#gC" "#gO" "#gP" "#gR" "#gS" "#gT" "#gU" "#gF" "#gX" "#gQ" "#gW"
		       "#ga" "#gb" "#gg" "#gd" "#ge" "#gz" "#gy" "#gh" "#gi" "#gk" "#gl" "#gm"
		       "#gn" "#gc" "#go" "#gp" "#gr" "#gs" "#gt" "#gu" "#gf" "#gx" "#gq" "#gw"))
	(type1 (vector #x0020 #x0021 #x0023 #x0025 #x0026
		       #x0028 #x0029 #x002b #x002c #x002e
		       #x002f #x0030 #x0031 #x0032 #x0033
		       #x0034 #x0035 #x0036 #x0037 #x0038
		       #x0039 #x003a #x003b #x003c #x003d
		       #x003e #x003f #x005b #x005d #x005f
		       #x007b #x007c #x007d #x00a9 #x00ac
		       #x00ae #x00b0 #x00b1 #x00d7 #x00f7
		       #x0192 #x0391 #x0392 #x0393 #x0394
		       #x0395 #x0396 #x0397 #x0398 #x0399
		       #x039a #x039b #x039c #x039d #x039e
		       #x039f #x03a0 #x03a1 #x03a3 #x03a4
		       #x03a5 #x03a6 #x03a7 #x03a8 #x03a9
		       #x03b1 #x03b2 #x03b3 #x03b4 #x03b5
		       #x03b6 #x03b7 #x03b8 #x03b9 #x03ba
		       #x03bb #x03bc #x03bd #x03be #x03bf
		       #x03c0 #x03c1 #x03c2 #x03c3 #x03c4
		       #x03c5 #x03c6 #x03c7 #x03c8 #x03c9
		       #x03d1 #x03d2 #x03d5 #x03d6 #x2022
		       #x2026 #x2032 #x2033 #x203e #x2044
		       #x2111 #x2118 #x211c #x2122 #x2126
		       #x2135 #x2190 #x2191 #x2192 #x2193
		       #x2194 #x21b5 #x21d0 #x21d1 #x21d2
		       #x21d3 #x21d4 #x2200 #x2202 #x2203
		       #x2205 #x2206 #x2207 #x2208 #x2209
		       #x220b #x220f #x2211 #x2212 #x2215
		       #x2217 #x221a #x221d #x221e #x2220
		       #x2227 #x2228 #x2229 #x222a #x222b
		       #x2234 #x223c #x2245 #x2248 #x2260
		       #x2261 #x2264 #x2265 #x2282 #x2283
		       #x2284 #x2286 #x2287 #x2295 #x2297
		       #x22a5 #x22c5 #x2320 #x2321 #x2329
		       #x232a #x25ca #x2660 #x2663 #x2665
		       #x2666))
	(title (vector "#<0x10>PLplot Example 23 - Greek Letters"
		       "#<0x10>PLplot Example 23 - Type 1 Symbol Font Glyphs by Unicode (a)"
		       "#<0x10>PLplot Example 23 - Type 1 Symbol Font Glyphs by Unicode (b)"
		       "#<0x10>PLplot Example 23 - Type 1 Symbol Font Glyphs by Unicode (c)"
		       "#<0x10>PLplot Example 23 - Number Forms Unicode Block"
		       "#<0x10>PLplot Example 23 - Arrows Unicode Block (a)"
		       "#<0x10>PLplot Example 23 - Arrows Unicode Block (b)"
		       "#<0x10>PLplot Example 23 - Mathematical Operators Unicode Block (a)"
		       "#<0x10>PLplot Example 23 - Mathematical Operators Unicode Block (b)"
		       "#<0x10>PLplot Example 23 - Mathematical Operators Unicode Block (c)"
		       "#<0x10>PLplot Example 23 - Mathematical Operators Unicode Block (d)"))
	(lo (vector #x0
		    #x0
		    #x40
		    #x80
		    #x2153
		    #x2190
		    #x21d0
		    #x2200
		    #x2240
		    #x2280
		    #x22c0))
	(hi (vector #x30
		    #x40
		    #x80
		    #xA6
		    #x2184
		    #x21d0
		    #x2200
		    #x2240
		    #x2280
		    #x22c0
		    #x2300))
	(nxcells (vector 12
			 8
			 8
			 8
			 8
			 8
			 8
			 8
			 8
			 8
			 8))
	(nycells (vector 8
			 8
			 8
			 8
			 8
			 8
			 8
			 8
			 8
			 8
			 8))
	(offset (vector 0
			0
			64
			128
			0
			0
			0
			0
			0
			0
			0))
	(fci (vector #x80000000
		     #x80000001
		     #x80000002
		     #x80000003
		     #x80000004
		     #x80000010
		     #x80000011
		     #x80000012
		     #x80000013
		     #x80000014
		     #x80000020
		     #x80000021
		     #x80000022
		     #x80000023
		     #x80000024
		     #x80000100
		     #x80000101
		     #x80000102
		     #x80000103
		     #x80000104
		     #x80000110
		     #x80000111
		     #x80000112
		     #x80000113
		     #x80000114
		     #x80000120
		     #x80000121
		     #x80000122
		     #x80000123
		     #x80000124))
	(family (vector "sans-serif"
			"serif"
			"monospace"
			"script"
			"symbol"))
	(style (vector "upright"
		       "italic"
		       "oblique"))
	(weight (vector "medium"
			"bold")))
    (plinit)
    (dotimes (page 11)
      (pladv 0)
      (plvpor 0.02d0 0.98d0 0.02d0 0.90d0)
      (plwind 0.0 1.0 0.0 1.0)
      (multiple-value-bind (xmin xmax ymin ymax) (plgspa)
	(declare (ignore xmin xmax))
	(plschr 0.0 0.8)
	(let ((ycharacter-scale (/ 1.0 (- ymax ymin))))
	  (multiple-value-bind (chardef charht) (plgchr)
	    (declare (ignore chardef))
	    (let ((yoffset (* charht ycharacter-scale))
		  (deltax (/ 1.0 (aref nxcells page)))
		  (deltay (/ 1.0 (aref nycells page)))
		  (length (- (aref hi page) (aref lo page)))
		  (slice 0))
	      (plcol0 2)
	      (plbox "bcg" deltax 0 "bcg" deltay 0)
	      (plcol0 15)
	      (do ((j (1- (aref nycells page)) (- j 1)))
		  ((< j -1))
		(let ((y (* (+ 0.5 j) deltay)))
		  (dotimes (i (aref nxcells page))
		    (let ((x (* (+ 0.5 i) deltax)))
		      (when (< slice length)
			(let ((cmdstring
			       (cond
				 ((= page 0)
				  (format nil "#~A" (aref greek slice)))
				 ((and (>= page 1) (<= page 3))
				  (format nil "~(##[0x~4,'0x]~)" (aref type1 (+ (aref offset page) slice))))
				 (t
				  (format nil "~(##[0x~4,'0x]~)" (+ (aref lo page) slice))))))
			  (plptex x (+ y yoffset) 1 0 0.5 (subseq cmdstring 1))
			  (plptex x (- y yoffset) 1 0 0.5 cmdstring))))
		    (incf slice))))))))
      (plschr 0.0 1.0)
      (plmtex "t" 1.5 0.5 0.5 (aref title page)))
    (format t "For example 23 prior to page 12 the FCI is 0x~x~%" (plgfci))
    (multiple-value-bind (ifamily istyle iweight) (plgfont)
      (format t "For example 23 prior to plage 12 the font family, style and weight are ~A ~A ~A~%" ifamily istyle iweight))
    (do ((page 11 (+ page 1)))
	((>= page 16))
      (let ((dy 0.030))
	(pladv 0)
	(plvpor 0.02 0.98 0.02 0.90)
	(plwind 0.0 1.0 0.0 1.0)
	(plsfci 0)
	(cond
	  ((= page 11)
	   (plmtex "t" 1.5 0.5 0.5 "#<0x10>PLplot Example 23 - Set Font with plsfci"))
	  ((= page 12)
	   (plmtex "t" 1.5 0.5 0.5 "#<0x10>PLplot Example 23 - Set Font with plsfont"))
	  ((= page 13)
	   (plmtex "t" 1.5 0.5 0.5 "#<0x10>PLplot Example 23 - Set Font with ##<0x8nnnnnnn> construct"))
	  ((= page 14)
	   (plmtex "t" 1.5 0.5 0.5 "#<0x10>PLplot Example 23 - Set Font with ##<0xmn> constructs"))
	  (t
	   (plmtex "t" 1.5 0.5 0.5 "#<0x10>PLplot Example 23 - Set Font with ##<FCI COMMAND STRING/> constructs")))
	(plschr 0.0 0.75)
	(dotimes (i 30)
	  (let* ((family-index (mod i 5))
		 (style-index (mod (floor (/ i 5)) 3))
		 (weight-index (mod (floor (/ (/ i 5) 3)) 2))
		 (string (cond
			   ((= page 11)
			    (progn
			      (plsfci (aref fci i))
			      (format nil "Page 12, ~A, ~A, ~A:  The quick brown fox jumps over the lazy dog"
				      (aref family family-index)
				      (aref style style-index)
				      (aref weight weight-index))))
			   ((= page 12)
			    (progn
			      (plsfont family-index style-index weight-index)
			      (format nil "Page 13, ~A, ~A, ~A:  The quick brown fox jumps over the lazy dog"
				      (aref family family-index)
				      (aref style style-index)
				      (aref weight weight-index))))
			   ((= page 13)
			    (format nil "Page 14, ~A, ~A, ~A:  #<0x~x>The quick brown fox jumps over the lazy dog"
				    (aref family family-index)
				    (aref style style-index)
				    (aref weight weight-index)
				    (aref fci i)))
			   ((= page 14)
			    (format nil "Page 15, ~A, ~A, ~A:  #<0x~1x0>#<0x~1x1>#<0x~1x2>The quick brown fox jumps over the lazy dog"
				    (aref family family-index)
				    (aref style style-index)
				    (aref weight weight-index)
				    family-index
				    style-index
				    weight-index))
			   (t
			    (format nil "Page 16, ~A, ~A, ~A:  #<~A/>#<~A/>#<~A/>The quick brown fox jumps over the lazy dog"
				    (aref family family-index)
				    (aref style style-index)
				    (aref weight weight-index)
				    (aref family family-index)
				    (aref style style-index)
				    (aref weight weight-index))))))
	    (plptex 0.0 (- 1.0 (* (+ i 0.5) dy)) 1.0 0.0 0.0 string)))
	(plschr 0.0 1.0)))
    (plcol0 1)
    (plend1)))

;;;;
;;;; Copyright (c) 2010 Hazen P. Babcock
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
