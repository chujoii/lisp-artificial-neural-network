#!/usr/bin/guile -s
!#
; coding: utf-8

;;;; example.scm ---  simple usage already learned perceptron.
;;;; Very strange situation: you have simple formula, but use intricate perceptron


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



;;; Keywords: algorithm perceptron neuron network machine learning



;;; Usage:
;; learn: ./machine-learning.scm.scm
;; result write to file knowledge-base.scm (use format from original example)
;; example.scm [Ua] [Ub] [Uc] [F]
;;
;; correct voltage from 207.0 to 253.0 (230 V ±10 %)
;; correct frequency from 49.8 to 50.2 (50 ±0,2 Hz)
;;
;; for example:
;; ./example.scm 220.0 230.0 123.4 50.0
;; alert!
;;
;; ./example.scm 220.0 230.0 240.0 50.0
;; all ok


;;; History:



;;; Code:





(load "../../../perceptron.scm")

;;; Use #t for debug print, or #f for silent
(define *debug-print* #t)

(load "knowledge-base.scm")

;;; Sensor (input units)
(define sensor (map string->number (cdr (command-line))))
(format #t "~a\n" sensor)


(format #t "simple calculate one layer perceptron\n~a\n\n\n"
	(perceptron-sar sensor
			weight-sa transfer-function-step threshold-a
			learned-weight-ar transfer-function-sigmoid threshold-r))



