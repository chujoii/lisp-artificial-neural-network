#!/usr/bin/guile -s
!#
; coding: utf-8

;;;; unit-testing.scm ---  Unit testing for growing-neural-gas code



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

;; guile unit-testing.scm
;; or
;; ./unit-testing.scm



;;; History:

;; Project started at 2019-11(Nov)-15



;;; Code:

(use-modules (ice-9 format))

(load "../../../growing-neural-gas.scm")
(load "../../../../../../util/battery-scheme/print-list.scm")

(set! *random-state* (seed->random-state 0.12345))

;;; Use #t for debug print, or #f for silent
(define *debug-print* #t)

;;; adaptation coefficients for weight
(define *eps-winner*    0.01)
(define *eps-neighbour* 0.0001)

(define *dimension-of-sensor* 4)

;;; Sensor (input units)
(define *example-sensor* (list 10 20 30 40))


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






(format #t "\nsimple 6 neurons (see ../../../growing-neural-gas.scm):\n")
(if *debug-print* (print-list-without-bracket *example-gng*))

(format #t "update weight neuron number 3 (all weight +10):\n")
(update-neuron-weight 3 (lambda (step weights) (map (lambda (y) (+ y step)) weights)) 10 *example-gng*)
(if *debug-print* (print-list-without-bracket *example-gng*))

(format #t "\ndisplayed only updated age:\n")
(update-neuron-age 0 1 + 1 *example-gng*)
(update-neuron-age 1 2 + 2 *example-gng*)
(update-neuron-age 2 0 + 3 *example-gng*)
(update-neuron-age 3 4 + 4 *example-gng*)
(if *debug-print* (print-list-without-bracket (map get-neuron-age *example-gng*)))

(update-neuron-local-error 3 + 0.1 *example-gng*)
(format #t "\ndisplayed only updated local error (see neuron number 3, count from 0):\n")
(if *debug-print* (print-list-without-bracket (map get-neuron-local-error *example-gng*)))



(format #t "Calculate distance between Weight and Sensor:(~7,2f ~7,2f ~7,2f ~7,2f ...) compare with:\n"
	(euclidean-distance  (get-neuron-weight (car *example-gng*)) *example-sensor*)
	(euclidean-distance  (get-neuron-weight (cadr *example-gng*)) *example-sensor*)
	(euclidean-distance  (get-neuron-weight (caddr *example-gng*)) *example-sensor*)
	(euclidean-distance  (get-neuron-weight (cadddr *example-gng*)) *example-sensor*))
(format #t "~a\n" (calculate-distance-weight-sensor *example-sensor* *example-gng*))



(format #t "\nFind indexes of two minimal elementt (first correct answer, then calculated answer)\n")
;
(format #t "(0 1) ~a\n" (find-index-of-two-minimal (list 1 1)))
(format #t "(0 1) ~a\n" (find-index-of-two-minimal (list 1 2)))
(format #t "(1 0) ~a\n" (find-index-of-two-minimal (list 2 1)))
;
(format #t "(0 1) ~a\n" (find-index-of-two-minimal (list 1 1 2)))
(format #t "(0 2) ~a\n" (find-index-of-two-minimal (list 1 2 1)))
(format #t "(1 2) ~a\n" (find-index-of-two-minimal (list 2 1 1)))
;
(format #t "(0 1) ~a\n" (find-index-of-two-minimal (list 1 2 3)))
(format #t "(0 2) ~a\n" (find-index-of-two-minimal (list 1 3 2)))
(format #t "(1 0) ~a\n" (find-index-of-two-minimal (list 2 1 3)))
(format #t "(2 0) ~a\n" (find-index-of-two-minimal (list 2 3 1)))
(format #t "(1 2) ~a\n" (find-index-of-two-minimal (list 3 1 2)))
(format #t "(2 1) ~a\n" (find-index-of-two-minimal (list 3 2 1)))

