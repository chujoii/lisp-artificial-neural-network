#!/usr/bin/guile -s
!#
; coding: utf-8

;;;; example.scm ---  simple usage of growing-neural-gas



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



;;; Keywords: algorithm neuron network machine learning growing neural gas



;;; Usage:

;; guile example.scm
;; or
;; ./example.scm



;;; History:

;; Project started at 2019-11(Nov)-15



;;; Code:

(use-modules (ice-9 format))

(load "../../../growing-neural-gas.scm")
(load "../../../../../../util/battery-scheme/print-list.scm")

(set! *random-state* (seed->random-state 0.12345))

;;; Use #t for debug print, or #f for silent
(define *debug-print* #t)

(define *dimension-of-sensor* 4)

;;; Sensor (input units)
(define example-sensor (list 10 20 30 40))


(define *initial-gng* (add-neuron (make-neuron *dimension-of-sensor* (list 0))
				  (add-neuron (make-neuron *dimension-of-sensor* (list 0))
					      '())))

(define *example-gng* (add-neuron (make-neuron *dimension-of-sensor* (list 0))
				  (add-neuron (make-neuron *dimension-of-sensor* (list 0))
					      (add-neuron (make-neuron *dimension-of-sensor* (list 0))
							  (add-neuron (make-neuron *dimension-of-sensor* (list 0))
								      (add-neuron (make-neuron *dimension-of-sensor* (list 0))
										  (add-neuron (make-neuron *dimension-of-sensor* (list 0))
											      '())))))))
(format #t "simple 2 initial neurons:\n\tweight\n\tage\n\tlocal-error\n")
;(if *debug-print* (print-list-without-bracket *initial-gng*))
(update-neuron-age 0 1 + 1 *initial-gng*)
(if *debug-print* (print-list-without-bracket *initial-gng*))

(format #t "\nsimple 6 neurons (see ../../../growing-neural-gas.scm), displayed only age:\n")
;(if *debug-print* (print-list-without-bracket *example-gng*))
(update-neuron-age 0 1 + 1 *example-gng*)
(update-neuron-age 1 2 + 2 *example-gng*)
(update-neuron-age 2 0 + 3 *example-gng*)
(update-neuron-age 3 4 + 4 *example-gng*)
(if *debug-print* (print-list-without-bracket (map cadr *example-gng*)))

(format #t "Euclidean distance between point (1.0 2.0) and (3.0 4.0): ~7,2f\n"
	(euclidean-distance (list 1.0 2.0) (list 3.0 4.0)))
