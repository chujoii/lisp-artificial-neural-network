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

;; Project started at 2019-11(Nov)-21



;;; Code:

(use-modules (ice-9 format))

(load "gng-to-dot.scm")

(define *example-gng* (list (list (list -9.8 -19.6 -29.4 -39.2) (list -1 1 -1 -1 -1 -1 3) 0.45)
			    (list (list -7.800000000000001 -17.6 -27.4 -37.2) (list 1 -1 0 -1 -1 -1 -1) 0.5)
			    (list (list 2.1 2.2 2.3 2.4) (list -1 0 -1 -1 -1 -1 3) 0.4)
			    (list (list -65.9 -164.8 -263.7 -362.6) (list -1 -1 -1 -1 4 -1 -1) 0.2)
			    (list (list -1.8000000000000007 -11.600000000000001 -21.4 -31.200000000000003) (list -1 -1 -1 4 -1 -1 -1) 0.3)
			    (list (list 5.1 5.2 5.3 5.4) (list -1 -1 -1 -1 -1 -1 -1) 0.7)
			    (list (list -3.8500000000000005 -8.700000000000001 -13.549999999999999 -18.400000000000002) (list 3 -1 3 -1 -1 -1 -1) 0.45)))

(format #t "\nsimple 7 neurons:\n")
(map print-neuron *example-gng*)

(define *gng-conn-list* (convert-gng-conn-ages-to-simple-list *example-gng*))
(format #t "\nconvert gng-conn-ages to simple list: ~a\n" *gng-conn-list*)

(define *string-body-dot* (list-to-string-dot-format  *gng-conn-list*))
(format #t "\nlist of connection ready for print:\n~a\n" *string-body-dot*)

(define *string-dot* (add-head-tail *winner* *string-body-dot*))
(format #t "\nDOT ready for print:\n
# Graphviz example:
#
# dot  -Tpng test.gv > thisfile.png
# instead dot:
# neato (nodes < 100)
# fdp
# sfdp (nodes < 100 K)

~a\n" *string-dot*)
