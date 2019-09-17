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

(use-modules (ice-9 format))

(set! *random-state* (random-state-from-platform))

;;; Transfer function (activation function)
(define (transfer-function-step signal threshold)
  (if (> signal threshold) 1 0))

(define (transfer-function-linear signal threshold)
  (cond ((< signal 0.0) 0) ;; fixme: level=const
	((> signal 1.0) 1) ;; fixme: level=const
	(else signal)))

;;  (/ 1.0 (+ 1.0 (exp (* -t signal))))
;; t = 0   : sigmoid equal to step function
;; t = 0,5 : dx~(0,1)
(define (transfer-function-sigmoid signal threshold)
  (/ 1.0 (+ 1.0 (exp (* -1 threshold signal)))))

(define (unittest-transfer-function transfer-function threshold)
  (define (iter a b)
    (if (< a b)
	(begin (format #t "~1,1f ~2,1f\n" a (transfer-function a threshold))
	       (iter (+ a 0.2) b))
	(newline)))

  (iter -3.0 3.0))

;; uncomment for test
;(unittest-transfer-function transfer-function-step 1.0)
;(unittest-transfer-function transfer-function-linear 1.0)
;(unittest-transfer-function transfer-function-sigmoid 1.0)


;; Sensor                    Association
;;
;; Sensor --- weight-sa --- [> sum --- transfer-function ---]
;; Sensor --- weight-sa __/
;;
;; calculate artificial neuron layer
(define (calculate-neuron-layer sensor weight transfer-function threshold)
  (define (iter list-in weight-in threshold-in result-list)
    (if (not (null? weight-in))                          ;; fixme debug print
	(begin (display (map * list-in (car weight-in))) ;; fixme (+) -> (*)
	       (display " sum=")
	       (display (apply + (map * list-in (car weight-in))))
	       (display " activated=")
	       (display (transfer-function (apply + (map * list-in (car weight-in)))
					   (car threshold-in)))
	       (newline)))

    (if (null? weight-in)
	result-list
	(iter list-in
	      (cdr weight-in)
	      (cdr threshold-in)
	      (append result-list (list (transfer-function (apply + (map * list-in (car weight-in)))
							   (car threshold-in)))))))

  (iter sensor weight threshold '()))



;;  Sensor                    Association                                    Response
;;
;;  Sensor --- weight-sa --- [> sum --- transfer-function ---] <--- weight-ar Response
;;  Sensor --- weight-sa __/                                    \__ weight-ar Response
;;  (first step                                               )
;; (second step                                                                      )
;;
;; perceptron with one layer (Sensor Association Response)
(define (perceptron-sar sensor
			weight-sa transfer-function-a threshold-a
			weight-ar transfer-function-r threshold-r)
  (calculate-neuron-layer (calculate-neuron-layer sensor weight-sa transfer-function-a threshold-a)
			  weight-ar transfer-function-r threshold-r))


;; machine-learning max-calculation-error
;; return weight-ar
(define (calculate-weight-sar sensor
			      weight-sa transfer-function-a threshold-a
			      weight-ar transfer-function-r threshold-r
			      real-result)
  (define (iter-by-r association-in weight-in-row response-in response-real weight-out) ;; by response in weight list (row)
    (if (null? weight-in-row)
	weight-out
	(begin
	  (display "c: ")
	  (display (car weight-in-row))
	  (if (= (car response-in) (car response-real))
	      (display " correct weight")
	      (display " need update weight"))
	  (newline)
	  (iter-by-r association-in (cdr weight-in-row) (cdr response-in) (cdr response-real)
		     (append weight-out (list
					 (if (= (car response-in) (car response-real))
					     (car weight-in-row)
					     (iter-by-a association-in (car weight-in-row) (car response-in) '()))))))))

  (define (iter-by-a a-in weight-in-column r-in w-out) ;; by association in weight list (column)
    (if (null? weight-in-column)
	w-out
	(begin
	  (display " w:")
	  (display (car weight-in-column))
	  (display " a:")
	  (display (car a-in))
	  (display " r:")
	  (display r-in)
	  (display " ?:")
		     (if (= (car a-in) 1)
			 (if (= r-in 1)
			     (display "w-1")
			     (display "w+1"))
			 (display "w"))
	  (newline)
	  (iter-by-a (cdr a-in) (cdr weight-in-column) r-in
		     (append
		      w-out
		       (list (+ (car weight-in-column)
			 (if (= (car a-in) 1)
			     (if (= r-in 1)
				 -1
				 1)
			     0)))
		      )))))


  (let ((tmp-association (calculate-neuron-layer sensor weight-sa transfer-function-a threshold-a)))
    (let ((tmp-response (calculate-neuron-layer tmp-association weight-ar transfer-function-r threshold-r)))
      (begin (display "tmp association layer")
	     (display tmp-association) (newline)
	     (display "tmp response layer")
	     (display tmp-response) (newline)
	     (newline)
	     (iter-by-r tmp-association weight-ar tmp-response real-result '())))))
