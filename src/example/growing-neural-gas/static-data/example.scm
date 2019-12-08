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

;; Project started at 2019-08(Aug)-28



;;; Code:

(use-modules (ice-9 format))

(load "../../../growing-neural-gas.scm")



(set! *random-state* (seed->random-state 0.12345))

;;; Use #t for debug print, or #f for silent
(define *debug-print* #t)

;;; adaptation coefficients for weight and local-error
(define *eps-winner*   0.1)
(define *eps-neighbour* 0.01)
(define *eps-local-error* 0.5)
(define *factor-beta-decrease-local-error* 0.9999)

(define *limit-conn-age* 3)

;; Adaptation step (add neuron in each *lambda-step* to network)
;;
;; Inactived nodes may appear if lambda=very_small (high frequency
;; insertion).
;;
;; For lambda=big start to learning goes very smoothly, but rough
;; clusters are created
(define *lambda-step* 1)

;; epoch counter
(define *epoch* 1)

(define *limit-network-size* 100)

(define *dimension-of-sensor* 4)

;;; Sensor (input units)
(define *example-sensor* (list 10 20 30 40))

;;; use Euclidean space for distance calculation
(define *functions-mixed-space* '())
;;
;; if use non-Euclidean space, for example last sensor = angle (in degree, where 0=360)
;; (define *functions-mixed-space* (list euclidean-distance euclidean-distance euclidean-distance angle-distance))

(define *initial-gng* (add-neuron (make-neuron *dimension-of-sensor* 1)
				  (add-neuron (make-neuron *dimension-of-sensor* 0)
					      '())))

(define *example-gng* (add-neuron (make-neuron *dimension-of-sensor* 5)
				  (add-neuron (make-neuron *dimension-of-sensor* 4)
					      (add-neuron (make-neuron *dimension-of-sensor* 3)
							  (add-neuron (make-neuron *dimension-of-sensor* 2)
								      (add-neuron (make-neuron *dimension-of-sensor* 1)
										  (add-neuron (make-neuron *dimension-of-sensor* 0)
											      '())))))))
(format #t "simple 2 initial neurons:\n\tweight\t\t\t\tconn-age\tlocal-error\n")
(if *debug-print* (map print-neuron *initial-gng*))
(format #t "\nupdate neuron connection age for 0 1\n")
(update-neuron-conn-age 0 1 + 1 *initial-gng*) ;; need create link beetwin first neuron!
(if *debug-print* (map print-neuron *initial-gng*))


(format #t "\nsimple 6 neurons (see ../../../growing-neural-gas.scm):\n")
(if *debug-print* (map print-neuron *example-gng*))
(format #t "\nupdate connection-age:\n")
(update-neuron-conn-age 0 1 + 1 *example-gng*) ;; need create link beetwin first neuron!
(update-neuron-conn-age 1 2 + 2 *example-gng*)
(update-neuron-conn-age 2 0 + 3 *example-gng*)
(update-neuron-conn-age 3 4 + 4 *example-gng*)
(if *debug-print* (map print-neuron *example-gng*))


(format #t "\n\n\nResult of one step working \"growing neural gas\":\n\n")
(format #t "Sensor ~a:\n\n" *example-sensor*)

(map print-neuron (growing-neural-gas *epoch* *example-sensor* *example-gng*))
(newline)
