#!/usr/bin/guile -s
!#
; coding: utf-8

;;;; perceptron.scm ---  simple perceptron



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

;; use it



;;; History:

;; Project started at 2019-08(Aug)-29



;;; Code:

(set! *random-state* (random-state-from-platform))

;;; The "signal" is sent, i.e. the output is set to one, if the activation meets the threshold
(define *perceptron-threshold* 1.0)

;;; Sensor (input units)
(define sensor (list 100 200 300 400 500))

;;; Association (hidden units)
;;; A.S
(define weight-sa
  (list (list 1.1 1.2 1.3 1.4 1.5)
	(list 2.1 2.2 2.3 2.4 2.5)
	(list 3.1 3.2 3.3 3.4 3.5)
	(list 4.1 4.2 4.3 4.4 4.5)))

;;; Response (output units)
;;; R.A
(define weight-ar
  (list (list 1.1 1.2 1.3 1.4)
	(list 2.1 2.2 2.3 2.4)
	(list 3.1 3.2 3.3 3.4)))




;;; Transfer function (activation function)
(define (transfer-function-step signal)
  (if (> signal *perceptron-threshold*) 1 0))

(define (transfer-function-linear signal)
  (cond ((< signal 0.0) 0) ;; fixme: level=const
	((> signal 1.0) 1) ;; fixme: level=const
	(else signal)))

;;  (/ 1.0 (+ 1.0 (exp (* -t signal))))
;; t = 0   : sigmoid equal to step function
;; t = 0,5 : dx~(0,1)
(define (transfer-function-sigmoid signal)
  (/ 1.0 (+ 1.0 (exp (* -0.5 signal))))) ;; fixme: t=const




(define (calculate-layer in weight transfer-function)
  (define (iter list-in weight-in result-list)
    (if (null? weight-in)
	result-list
	(iter list-in
	      (cdr weight-in)
	      (append result-list (list (transfer-function (apply + (map * list-in (car weight-in)))))))))
  
  (iter in weight '()))




(display (calculate-layer (calculate-layer sensor weight-sa transfer-function-step) weight-ar transfer-function-sigmoid))
(newline)


