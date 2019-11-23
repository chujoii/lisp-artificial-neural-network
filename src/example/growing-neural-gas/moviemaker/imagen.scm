#!/usr/bin/guile -s
!#
; coding: utf-8

;;;; imagen.scm ---  script for growing-neural-gas to generate result image



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



;;; Keywords: algorithm neuron network machine learning growing neural gas image



;;; Usage:

;; ./datagen.scm | ./imagen.scm



;;; History:

;; Project started at 2019-11(Nov)-23



;;; Code:

(use-modules (ice-9 format))
(use-modules (ice-9 rdelim))

(load "../../../growing-neural-gas.scm")
(load "../../../visualization/gng-to-dot.scm")

;;; Use #t for debug print, or #f for silent
(define *debug-print* #f)

;;; adaptation coefficients for weight and local-error
(define *eps-winner*   10.0)
(define *eps-neighbour* 1.0)
(define *eps-local-error* 0.5)

(define *limit-conn-age* 3)

;; adaptation step (add neuron in each *lambda-step* to network)
(define *lambda-step* 1)

;; epoch counter
(define *epoch* 1)

(define *limit-network-size* 100)

(define *dimension-of-sensor* 2)

(define *initial-gng* (add-neuron (make-neuron *dimension-of-sensor* 1)
				  (add-neuron (make-neuron *dimension-of-sensor* 0)
					      '())))
(update-neuron-conn-age 0 1 + 1 *initial-gng*) ;; need create link beetwin first neuron!

(map print-neuron *initial-gng*)

(let ((data (map string->number (string-split (read-line) #\space)))) ; fixme: need check input data
  (map print-neuron (growing-neural-gas *epoch* data *initial-gng*)))
