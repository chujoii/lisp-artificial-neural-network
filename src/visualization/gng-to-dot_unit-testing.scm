#!/usr/bin/guile -s
!#
; coding: utf-8

;;;; gng-to-dot_unit-testing.scm ---  Unit testing for growing-neural-gas visualization with graphviz



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



;;; Keywords: algorithm neuron network machine learning growing neural gas visualization graphviz



;;; Usage:

;; guile gng-to-dot_unit-testing.scm
;; or
;; ./gng-to-dot_unit-testing.scm



;;; History:

;; Project started at 2019-08(Aug)-28



;;; Code:

(use-modules (ice-9 format))

(load "gng-to-dot.scm")

;;; Sensor (input units)
(define *example-sensor* (list 0 0 5 7))

;; from sensor you get: (a b c d);
;; for view in tooltip only "a" and "c" set list to *list-for-print-tooltip*==(0 2)
(define *list-for-print-tooltip* (list 0 2))

;; list of node number:
(define *port-positions* (list -1    5    -1    2    -1    -1   -1    -1   -1))
;; possible positions:         "n", "ne", "e", "se", "s", "sw", "w", "nw", "c"

;; limit for weights in format:
;; ((lo-lim0 hi-lim0) (lo-lim1 hi-lim1) (lo-lim2 hi-lim2) ... (lo-limN hi-limN))
(define *limit-weight* (list (list -10 10) (list -20 10) (list 0 10) (list 0 10)))

(define *example-gng* (list (list (list  -9.8  -19.6  -29.4  -39.2) (list -1  1 -1 -1 -1 -1  3) 0.45)
			    (list (list  -7.8  -17.6  -27.4  -37.2) (list  1 -1  0 -1 -1 -1 -1)  0.5)
			    (list (list   2.1    2.2    2.3    2.4) (list -1  0 -1 -1 -1 -1  3)  0.4)
			    (list (list -65.9 -164.8 -263.7 -362.6) (list -1 -1 -1 -1  4 -1 -1)  0.2)
			    (list (list  -1.8  -11.6  -21.4  -31.2) (list -1 -1 -1  4 -1 -1 -1)  0.3)
			    (list (list   5.1    5.2    5.3    5.4) (list -1 -1 -1 -1 -1 -1 -1)  0.7)
			    (list (list  -3.9   -8.7  -13.5  -18.4) (list  3 -1  3 -1 -1 -1 -1) 0.45)))

(format #t "\nsimple 7 neurons:\n")
(map print-neuron *example-gng*)

(define *gng-conn-list* (convert-gng-conn-ages-to-simple-list *example-gng*))
(format #t "\nconvert gng-conn-ages to simple list: ~a\n" *gng-conn-list*)

(define *string-body-dot* (list-to-string-dot-format *gng-conn-list*))
(format #t "\nlist of connection ready for print:\n~a\n" *string-body-dot*)

(define *node-attributes* (convert-gng-to-string-node-attributes *list-for-print-tooltip* '() *limit-weight* (map get-neuron-weight *example-gng*)))
(format #t "\ntest node attributes: tooltip (weight) and color:\n~a\n" *node-attributes*)

(define *string-dot* (add-head-tail (if (in-limit? *limit-weight* *example-sensor*) "green" "red") *winners* *string-body-dot* *node-attributes*))
(format #t "\nDOT ready for print:\n
# Graphviz example:
#
# dot -Tpng test.gv >! test.png
# dot -Tcmapx test.gv >! test.cmapx
#
# instead dot:
# neato (nodes < 100)
# fdp
# sfdp (nodes < 100 K)

~a\n" *string-dot*)

(format #t "weights:\n~a\n" (weights-to-string (map get-neuron-weight *example-gng*)))

(format #t "correct (for node 5) GraphViz portPos=\":ne\";\t\tcalculated=\"~a\"\n"
	(port-position 5 *port-positions*))
(format #t "correct (for node 3) GraphViz portPos=\"\" (empty);\tcalculated=\"~a\"\n"
	(port-position 3 *port-positions*))

(define (generate-port-positions limit-weight gng)
  ;; list of node number:
  ;;(define *port-positions* (list -1    -1    -1   -1    -1    -1   -1    -1   -1)) == (make-list 9 -1)
  ;; possible positions:           "n", "ne", "e", "se", "s", "sw", "w", "nw", "c"
  (set! *port-positions* (make-list 9 -1)) ;; fixme: very strange: set (clear) and list-set
  (list-set! *port-positions*
	     *compass-point-c* ;; "c" (center)
	     (car (find-index-of-two-minimal (calculate-distance-weight-sensor (map (lambda (x) (/ (apply + x) 2.0)) limit-weight) gng)))) ;; find nearest node for calculated center (calculation based on limit-weight)
  *port-positions*)

(format #t "\nsearch center based on limits (~a)\nin node weights\n" *limit-weight*)
(map print-neuron *example-gng*)
(format #t "see last element in list (\"c\"): ~a\n" (generate-port-positions *limit-weight* *example-gng*))

(gng-to-dot-file *list-for-print-tooltip* *port-positions* *limit-weight* *example-sensor* *winners* *example-gng* "test.gv")


