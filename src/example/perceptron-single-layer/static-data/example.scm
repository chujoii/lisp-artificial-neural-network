#!/usr/bin/guile -s
!#
; coding: utf-8

;;;; example.scm ---  simple usage of perceptron



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



;;; Keywords: algorithm perceptron neuron network machine learning



;;; Usage:

;; guile example.scm
;; or
;; ./example.scm



;;; History:

;; Project started at 2019-08(Aug)-28



;;; Code:

(load "../../../perceptron.scm")

;;; Use #t for debug print, or #f for silent
(define *debug-print* #t)

;; error value
(define *error-value* 0.1)

;;; Sensor (input units)
(define example-sensor (list 10 20 30 40 50 60))

;;; weight: Sensor (input units) --- Association (hidden units)
;;; A.S
(define example-weight-sa
  (list (list 1.1 1.2 1.3 1.4 1.5 1.6)
	(list 2.1 2.2 2.3 2.4 2.5 2.6)
	(list 3.1 3.2 3.3 3.4 3.5 3.6)
	(list 4.1 4.2 4.3 4.4 4.5 4.6)
	(list 5.1 5.2 5.3 5.4 5.5 5.6)))

;;; weight: Association (hidden units) --- Response (output units)
;;; R.A
(define example-weight-ar
  (list (list 11.11 11.22 11.33 11.44 11.55)
	(list 22.11 22.22 22.33 22.44 22.55)
	(list 33.11 33.22 33.33 33.44 33.55)
	(list 44.11 44.22 44.33 44.44 44.55)))

;;; threshold: Association
(define example-threshold-a (list 1.0 1000.0 1.0 1.0 9000.0))
(define example-threshold-r (list 100.0 100.0 1.0 1.0))

;; step correction weight
(define correction 1)

(format #t "simple calculate one layer perceptron\n")
(format #t "~a\n\n\n"
	(perceptron-sar example-sensor
			example-weight-sa transfer-function-step example-threshold-a
			example-weight-ar transfer-function-step example-threshold-r))

(format #t "repeat previous code for simple calculations,\nwith update weight of one layer perceptron\n")
(format #t "~a\n"
	(calculate-weight-sar example-sensor
			      example-weight-sa transfer-function-step example-threshold-a
			      example-weight-ar transfer-function-step example-threshold-r
			      (list 0 1 0 1)
			      correction
			      *error-value*))

(format #t "see ../../../../doc/img/fig-1.pdf\n")
