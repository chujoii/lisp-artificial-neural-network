#!/usr/bin/guile -s
!#
; coding: utf-8

;;;; perceptron-random-data-learn-by-formula.scm ---  simple usage of perceptron for learn



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

;; guile perceptron-random-data-learn-by-formula.scm
;; or
;; ./perceptron-random-data-learn-by-formula.scm



;;; History:



;;; Code:

(load "../perceptron.scm")

(set! *random-state* (random-state-from-platform))

;;; Dimensoin of sensor in format: (list mean standard-deviation)
(define dim-sensor (list (list 230.0 23.0)   ;; 230 В ±10 %
			 (list 230.0 23.0)
			 (list 230.0 23.0)
			 (list  50.0  0.2))) ;; 50 ±0,2 Гц


;;; weight: Sensor (input units) --- Association (hidden units)
;;; A.S
;(define example-weight-sa

;;; weight: Association (hidden units) --- Response (output units)
;;; R.A
;(define example-weight-ar


;;; threshold: Association
;(define example-threshold-a (list 1.0 1000.0 1.0 1.0 9000.0))
;(define example-threshold-r (list 100.0 100.0 1.0 1.0))

(define (get-sensor-data dimension)
  (define (iter dim data)
    (if (null? dim)
	data
	(iter (cdr dim) (append data
				(list (+ (caar dim) (* (cadar dim) (random:normal))))))))
				;; (caar dim) mean of normal distribution
				;; (cadar dim) standard-deviation

  (iter dimension '()))
  

(display (get-sensor-data dim-sensor))
(newline)

