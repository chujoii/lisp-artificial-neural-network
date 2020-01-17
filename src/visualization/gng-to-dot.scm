; coding: utf-8

;;;; gng-to-dot.scm ---  Convert network generating from growing-neural-gas to DOT for graphviz



;;; Copyright (C) 2019-2020 Roman V. Prikhodchenko



;;; Author: Roman V. Prikhodchenko <chujoii@gmail.com>



;;;    This file is part of lisp-artificial-neural-network.
;;;
;;;    lisp-artificial-neural-network is free software: you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License as published by
;;;    the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version.
;;;
;;;    lisp-artificial-neural-network is distributed in the hope that it will be useful,
;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;    GNU General Public License for more details.
;;;
;;;    You should have received a copy of the GNU General Public License
;;;    along with lisp-artificial-neural-network.  If not, see <http://www.gnu.org/licenses/>.



;;; Keywords: algorithm neuron network machine learning growing neural gas DOT graphviz



;;; Usage:

;; guile gng-to-dot.scm
;; or
;; ./gng-to-dot.scm



;;; History:

;; Project started at 2019-08(Aug)-28



;;; Code:

(use-modules (ice-9 format))

(load "../growing-neural-gas.scm")
(load "../../../battery-scheme/dir-and-file.scm")

(define *compass-point-n*  0)
(define *compass-point-ne* 1)
(define *compass-point-e*  2)
(define *compass-point-se* 3)
(define *compass-point-s*  4)
(define *compass-point-sw* 5)
(define *compass-point-w*  6)
(define *compass-point-nw* 7)
(define *compass-point-c*  8)

(define *color-list* (list ; based on https://en.wikipedia.org/wiki/Web_colors
		      "White"
		      ;"Silver" ; similar to Gray
		      "Gray"
		      ;"Black" ; fixme: need change font color for contrast
		      ;"Red" ; already used for border (border=red if value out of limit)
		      "Maroon"
		      "Yellow"
		      "Olive"
		      "Lime"
		      ;"Green" ; already used for border (border=green if value inside limit)
		      "Aqua"
		      "Teal"
		      "Blue"
		      "Navy"
		      "Fuchsia"
		      "Purple"))

;; Convert gng-conn-ages to simple list
;; unconnected node doesn't show
(define (convert-gng-conn-ages-to-simple-list gng)
  (define (iter-x x conn-ages)
    (if (null? conn-ages)
	'()
	(append (iter-y x 0 (car conn-ages)) (iter-x (1+ x) (cdr conn-ages)))))

  (define (iter-y x y line)
    (if (null? line)
	'()
	(if (or (> x y) (< (car line) *initial-connection-age*))
	    (iter-y x (1+ y) (cdr line))
	    (cons (list x y) (iter-y x (1+ y) (cdr line))))))

  (iter-x 0 (map get-neuron-conn-age gng)))


;; DOT format for using graphviz
(define (list-to-string-dot-format conn-list)
  (if (null? conn-list)
      ""
      (string-append (string-join (map number->string (car conn-list)) " -- ")
		     ";\n"
		     (list-to-string-dot-format (cdr conn-list)))))


(define (in-limit? limits weights)
  (if (null? weights)
      #t ;;(and (<= lo-lim0            weight0) (<= weight0              hi-lim0))
      (and (<= (caar limits) (car weights)) (<= (car weights) (cadar limits))
	   (in-limit? (cdr limits) (cdr weights)))))


;; from index and positions generate node":compass_point".
;;
;; result graph in png rotated as GraphViz want, but it can be solved:
;; add "GraphViz portPos" as node:compass_point
;; If a compass point is used, it must have the form "n","ne","e","se","s","sw","w","nw","c"
;;
;; for example to node 44 add ":ne":
;; (port-position 44   (-1   44    -1    -1    -1    -1    -1    -1    -1))
;; possible positions: "n", "ne", "e",  "se", "s",  "sw", "w",  "nw", "c"
(define (port-position index list-of-port-positions)
  (define (iter counter pos)
    (if (null? pos)
	""
	(if (= index (car pos))
	    (cond ((= counter 0) ":n")
		  ((= counter 1) ":ne")
		  ((= counter 2) ":e")
		  ((= counter 3) ":se")
		  ((= counter 4) ":s")
		  ((= counter 5) ":sw")
		  ((= counter 6) ":w")
		  ((= counter 7) ":nw")
		  ((= counter 8) ":c")
		  (else ":-"))
	    (iter (1+ counter) (cdr pos)))))

  (iter 0 list-of-port-positions))



(define (number-to-group-color number group)
  (define (iter group-counter grp)
    (if (null? grp)
	"White"
	(if (not (memv number (car grp))) ; search in list
	    (iter (1+ group-counter) (cdr grp)); not finded
	    (list-ref *color-list* (remainder group-counter (length *color-list*)))))) ; find element in list

  (format #t "~d ~a\n" number group)
  (iter 0 group))



;; from list: index-column-list=(0 2) weights=((1 2 3 4) (5 6 7 8) (9 10 11 10))
;; generate string:
;; 0 [tooltip="1 3"]
;; 1 [tooltip="5 7"]
;; 2 [tooltip="9 11"]
;;
;; weight-limits in format: ((lo-lim0 hi-lim0) (lo-lim1 hi-lim1) (lo-lim2 hi-lim2) ... (lo-limN hi-limN))
;; from list: weight-limits=((4 9) (4 9) (4 9) (4 9)) weights=((1 2 3 4) (5 6 7 8) (9 10 11 10))
;;                                                              f f f t   t t t t   t  f  f  f
;; (if (and (<= lo-lim0 weight0) (<= weight0 hi-lim0) (<= lo-lim1 weight1) (<= weight1 hi-lim1) ...)
;;     color=green
;;     color=default-black)
;; generate string:
;; 0 [color=black]
;; 1 [tooltip=green]
;; 2 [tooltip=black]
;;
;;
;; for use list-of-port-positions: see function port-position
(define (convert-gng-to-string-node-attributes index-column-list list-of-port-positions list-of-groups weight-limits current-sensor-weight weights utilities)
  (define (iter counter w diameter-node)
    (if (null? w)
	""
	(string-append (number->string counter)
		       (port-position counter list-of-port-positions)
		       " [tooltip=\""
		       (string-join (map (lambda (x) (format #f "~,2f" x)) (list-from-index-list index-column-list (car w))) " ")
		       "\""
		       (if (not (in-limit? weight-limits (car w))) ", color=darkred" "")
		       ", style=filled, fillcolor=" (number-to-group-color counter list-of-groups)
		       (format #f ", width=~,2f" (car diameter-node))
		       "]\n"
		       (iter (1+ counter) (cdr w) (cdr diameter-node)))))

  (let ((Umin (cdr (extremum utilities <)))
	(Umax (cdr (extremum utilities >))))
    (string-append "c [tooltip=\""
		   (string-join (map (lambda (x) (format #f "~,2f" x)) (list-from-index-list index-column-list current-sensor-weight)) " ") ; tooltip for current node
		   "\", shape=box, color=black, fillcolor=" (if (in-limit? weight-limits current-sensor-weight) "green" "red") ", style=filled, fontcolor=white];\n"
		   ;;  D   - Dmin    U   - Umin
		   ;; ----------- = -----------
		   ;; Dmax - Dmin   Umax - Umin
		   (iter 0 weights (map (lambda (u) (+ (/ (* (- u Umin) (- *Dmax* *Dmin*)) (- Umax Umin)) *Dmin*)) utilities)))))



;; copy of convert-gng-to-string-node-attributes
(define (weights-to-string weights)
  (if (null? weights)
      ""
      (string-append (string-join (map (lambda (x) (format #f "~,2f" x)) (car weights)) " ")
		     "\n"
		     (weights-to-string (cdr weights)))))



(define (add-head-tail winners body tooltip)
  (string-append "graph ai {\n"
		 "graph [" *image-size* ", " *image-dpi* ", " *image-ratio* "];\n"
		 "node [shape=circle, color=darkgreen];\n"
		 "edge [color=darkgrey];\n"
		 "sep=\"+11\";\n"      ; Adding additional space around the nodes
		 "esep=\"+10\";\n"      ; Adding space for edge. Margin used around polygons for purposes of SPLINE edge routing. Should normally be strictly less than sep.
		 "splines=" *edge-splines* ";\n"   ; Controls how, and if, edges are represented. True = nice edges, but increase CPU load (false=line (time=3.23s), polyline (time=10.40s), curved (time=3.25s), ortho (time=3.22s), true=spline (time=10.35s), compound for fdp)
		 "overlap=scalexy;\n" ; Determines if and how node overlaps should be removed.
		 "fixedsize=true;\n"  ; When drawn, a node’s actual size is the greater of the requested size and the area needed for its text label, unless fixedsize=true, in which case the width and height values are enforced.
		 "\n"
		 "c -- " (number->string (car winners)) ";\n"
		 "c -- " (number->string (cadr winners)) ";\n"
		 tooltip
		 "\n\n"
		 body "}\n"))



(define (gng-to-dot-file list-for-print-tooltip list-of-port-positions list-of-groups limits-of-weight current-sensor-weight winners gng filename)
  (display-to-file filename
		   (add-head-tail
				  winners
				  (list-to-string-dot-format (convert-gng-conn-ages-to-simple-list gng))
				  (convert-gng-to-string-node-attributes list-for-print-tooltip
									 list-of-port-positions
									 list-of-groups
									 limits-of-weight
									 current-sensor-weight
									 (map get-neuron-weight gng)
									 (map get-neuron-utility-factor gng)))))
