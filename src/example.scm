#!/usr/bin/guile -s
!#
; coding: utf-8

;;;; example.scm ---  simple usage of perceptron



;;; Copyright (C) 2019 Roman V. Prikhodchenko



;;; Author: Roman V. Prikhodchenko <chujoii@gmail.com>



;;;    This file is part of lisp-perceptron.
;;;
;;;    lisp-perceptron is free software: you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License as published by
;;;    the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version.
;;;
;;;    lisp-perceptron is distributed in the hope that it will be useful,
;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;    GNU General Public License for more details.
;;;
;;;    You should have received a copy of the GNU General Public License
;;;    along with lisp-perceptron.  If not, see <http://www.gnu.org/licenses/>.



;;; Keywords: algorithm perceptron neuron network machine learning



;;; Usage:

;; guile example.scm
;; or
;; ./example.scm



;;; History:

;; Project started at 2019-08(Aug)-29



;;; Code:

(load "perceptron.scm")

;;; Sensor (input units)
(define example-sensor (list 10 20 30 40 50))

;;; weight: Sensor (input units) --- Association (hidden units)
;;; A.S
(define example-weight-sa
  (list (list 1.1 1.2 1.3 1.4 1.5)
	(list 2.1 2.2 2.3 2.4 2.5)
	(list 3.1 3.2 3.3 3.4 3.5)
	(list 4.1 4.2 4.3 4.4 4.5)))

;;; weight: Association (hidden units) --- Response (output units)
;;; R.A
(define example-weight-ar
  (list (list 11.11 11.22 11.33 11.44)
	(list 22.11 22.22 22.33 22.44)
	(list 33.11 33.22 33.33 33.44)))

;;; threshold: Association
(define example-threshold-a (list 1.0 10000.0 1.0 10000.0))
(define example-threshold-r (list 30.0 30.0 100.0))

;; simple calculate one layer perceptron
(display (perceptron-sar example-sensor
			 example-weight-sa transfer-function-step    example-threshold-a
			 example-weight-ar transfer-function-step example-threshold-r))
(newline)

;; simple update weight of one layer perceptron
(display (calculate-weight-sar example-sensor
			       example-weight-sa transfer-function-step    example-threshold-a
			       example-weight-ar transfer-function-step example-threshold-r
			       (list 0 1 0)))
(newline)
