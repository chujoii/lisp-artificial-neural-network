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
(load "../../../../util/battery-scheme/list.scm")

(set! *random-state* (random-state-from-platform))

;;; Set dimensoin of sensor in format: (list mean standard-deviation)
(define dimension-sensor (list (list 230.0 23.0)   ;; 230 В ±10 %
			 (list 230.0 23.0)
			 (list 230.0 23.0)
			 (list  50.0  0.2))) ;; 50 ±0,2 Гц
;;; Set number of output units
(define number-response 3)









;;; Calculate approximate value of number hidden
;;; Nhidden = (2/3)Nin + Nout
(define number-association (ceiling (+ (/ (* 2 (length dimension-sensor)) 3)  number-response)))

;;; weight: Association (hidden units) --- Response (output units)
;;; R.A
;(define example-weight-ar


;;; threshold: Association
;(define example-threshold-a (list 1.0 1000.0 1.0 1.0 9000.0))
;(define example-threshold-r (list 100.0 100.0 1.0 1.0))

(define (generate-random-sensor-data dimension)
    (if (null? dimension)
	'()
	(cons (+ (caar dimension) (* (cadar dimension) (random:normal)))
	      ;; (caar dimension) mean of normal distribution
	      ;; (cadar dimension) standard-deviation
	      (generate-random-sensor-data (cdr dimension)))))


;;; weight: Sensor (input units) --- Association (hidden units)
;;; A.S
(define (set-random-weight-sa numin numhid)
    (if (<= numhid 0)
	'()
	(cons (create-list-of-n-element-filled-by-evaluated-function numin random:uniform) (set-random-weight-sa numin (- numhid 1)))))

(display "Sensor (input) layer size = ")
(display (length dimension-sensor))
(newline)

(display "Association (hidden) layer size = ")
(display number-association)
(newline)

(display "Response (output) layer size = ")
(display number-response)
(newline)

(display "Weight Sensor-Response ")
(display (set-random-weight-sa (length dimension-sensor) number-association))
(newline)

(display "Random generated Sensors value ")
(display (generate-random-sensor-data dimension-sensor))
(newline)
