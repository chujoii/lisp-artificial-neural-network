#!/usr/bin/guile -s
!#
; coding: utf-8

;;;; perceptron-random-data-learn-by-formula.scm ---  simple usage of perceptron for learn.
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

;; guile perceptron-random-data-learn-by-formula.scm
;; or
;; ./perceptron-random-data-learn-by-formula.scm



;;; History:



;;; Code:

(load "../../../perceptron.scm")
(load "../../../../../../util/battery-scheme/list.scm")

(set! *random-state* (random-state-from-platform))

;;; Use #t for debug print, or #f for silent
(define *debug-print* #t)


;; error value
(define *error-value* 0.4)

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



(define (generate-random-sensor-data dimension)
  (if (null? dimension)
      '()
      (cons (+ (caar dimension) (* (cadar dimension) (random:normal)))
	    ;; (caar dimension) mean of normal distribution
	    ;; (cadar dimension) standard-deviation
	    (generate-random-sensor-data (cdr dimension)))))


;;; weight: Sensor (input units) --- Association (hidden units)
;;; A.S
(define (set-random-weight-sa numin numout)
  (if (<= numout 0)
      '()
      (cons (create-list-of-n-element-filled-by-evaluated-function numin random:normal) (set-random-weight-sa numin (- numout 1)))))

;;; threshold: Association
(define threshold-a (create-list-of-n-element-filled-by-evaluated-function number-association random:uniform))

;;; weight: Association (hidden units) --- Response (output units)
;;; R.A
(define (set-random-weight-ar numin numout)
  (if (<= numout 0)
      '()
      (cons (create-list-of-n-element-filled-by-evaluated-function numin random:uniform) (set-random-weight-sa numin (- numout 1)))))


;;; threshold: Response
(define threshold-r (create-list-of-n-element-filled-by-evaluated-function number-response random:uniform))


;;; formula for correct answer
;;; Very strange situation: you have simple formula, but use intricate perceptron
(define (formula_for_correct_answer dimension sensor-data)
  (define (iter dim sen)
    (if (null? dim)
	'()
	;; caar dimension = value; cdar dimension = d_value; car sensor-data = real_value
	(cons (if (and (>   (apply + (car dim))   (car sen))
		       (<   (apply - (car dim))   (car sen)))
		  0 1) ; 0 all ok; 1 error
	      (iter (cdr dim) (cdr sen)))))

  (let ((result (iter dimension sensor-data))) ; very strange
    (list (if (>= (apply + result) *error-value*) 1 0)   ; 1 for breakage
	  0                                              ; unknown: before breakage state
	  (if (< (apply + result) *error-value*) 1 0)))) ; 1 for normal







(format #t "Sensor (input) layer size = ~d\n\n" (length dimension-sensor))
(format #t  "Association (hidden) layer size = ~d\n\n" number-association)
(format #t "Response (output) layer size = ~d\n\n" number-response)
(define weight-sa (set-random-weight-sa (length dimension-sensor) number-association))
(format #t "Weight Sensor-Association ~a\n\n" weight-sa)
(format #t "Threshold Association ~a\n\n" threshold-a)
(define initial-weight-ar (set-random-weight-ar number-association number-response))
(format #t "Weight Association-Response ~a\n\n" initial-weight-ar)
(format #t "Threshold Response ~a\n\n" threshold-r)









(format #t "simple calculate one layer perceptron\nwith update weight of one layer perceptron\n")
(define (cycle-learn weight-ar limit correction-scale)
  (format #t "---------------------------=[ ~d ~e]=---------------------------\n" limit (/ limit correction-scale))
  (let ((data (generate-random-sensor-data dimension-sensor)))
    (if *debug-print* (format #t "Random generated Sensors value ~a\n" data))
    (if (<= limit 0)
	weight-ar
	(cycle-learn (calculate-weight-sar data
					   weight-sa transfer-function-step threshold-a
					   weight-ar transfer-function-sigmoid threshold-r
					   (formula_for_correct_answer dimension-sensor data)
					   (/ limit correction-scale)
					    *error-value*)
		     (- limit 1) correction-scale))))


(define learned-weight-ar (cycle-learn initial-weight-ar 20 100.0))


(newline)
(format #t "Association (hidden) layer size = ~d\n\n" number-association)
(format #t "Response (output) layer size = ~d\n\n" number-response)
(format #t "Weight Sensor-Association ~a\n\n" weight-sa)
(format #t "Threshold Association ~a\n\n" threshold-a)
(format #t "Initial Weight Association-Response ~a\n\n" initial-weight-ar)
(format #t "Learned Weight Association-Response ~a\n\n" learned-weight-ar)
(format #t "Threshold Response ~a\n\n" threshold-r)
