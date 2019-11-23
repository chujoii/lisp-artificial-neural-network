#!/usr/bin/guile -s
!#
; coding: utf-8

;;;; gng-to-dot.scm ---  Convert network generating from growing-neural-gas to DOT for graphviz



;;; Copyright (C) 2019 Roman V. Prikhodchenko



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

;; Project started at 2019-11(Nov)-21



;;; Code:

(use-modules (ice-9 format))

(load "../growing-neural-gas.scm")
(load "../../../../util/battery-scheme/dir-and-file.scm")

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


;; from list: (0 2) ((1 2 3 4) (5 6 7 8) (9 10 11 10))
;; generate string:
;; 0 [tooltip="1 3"]
;; 1 [tooltip="5 7"]
;; 2 [tooltip="9 11"]
(define (convert-gng-to-string-tooltip index-column-list weights)
  (define (inc counter w)
    (if (null? w)
	""
	(string-append (number->string counter)
			     " [tooltip=\""
			     (string-join (map (lambda (x) (format #f "~,2f" x)) (car w)) " ")
			     "\"]\n"
			     (inc (1+ counter) (cdr w)))))

  (inc 0 (map (lambda (x) (list-from-index-list index-column-list x)) weights)))



(define (add-head-tail winners body tooltip)
  (string-append "graph ai {\n"
		 "node [shape=circle];\n"
		 "\n"
		 "c [label=\"c\", shape=box, color=blue];\n"
		 "c -- " (number->string (car winners)) ";\n"
		 "c -- " (number->string (cadr winners)) ";\n"
		 tooltip
		 "\n\n"
		 body "}\n"))



(define (gng-to-dot-file list-for-print-tooltip winners gng filename)
  (display-to-file filename
		 (add-head-tail winners
				(list-to-string-dot-format (convert-gng-conn-ages-to-simple-list gng))
				(if (null? list-for-print-tooltip) ""
				    (convert-gng-to-string-tooltip list-for-print-tooltip (map get-neuron-weight gng))))))
