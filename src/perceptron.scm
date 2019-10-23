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

(define (unittest-transfer-function transfer-function threshold   from step to)
  (if (< from to)
      (begin (format #t "~1,1f ~2,1f\n" from (transfer-function from threshold))
	     (unittest-transfer-function transfer-function threshold (+ from step) step to))
      (newline)))


;; uncomment for test
					;(unittest-transfer-function transfer-function-step 1.0   -3.0 0.2 3.0)
					;(unittest-transfer-function transfer-function-linear 1.0   -3.0 0.2 3.0)
					;(unittest-transfer-function transfer-function-sigmoid 1.0   -3.0 0.2 3.0)


;; Sensor                    Association
;;
;; Sensor --- weight-sa --- [> sum --- transfer-function ---]
;; Sensor --- weight-sa __/
;;
;; calculate artificial neuron layer
(define (calculate-neuron-layer list-in weight transfer-function threshold)
  (if *debug-print*
      (if (not (null? weight))
	  (format #t "~a sum=~1,2f activated=~1,2f\n"
		  (map * list-in (car weight))
		  (apply + (map * list-in (car weight)))
		  (transfer-function (apply + (map * list-in (car weight)))
				     (car threshold)))))

  (if (null? weight)
      '()
      (cons (transfer-function (apply + (map * list-in (car weight)))
			       (car threshold))
	    (calculate-neuron-layer list-in
				    (cdr weight)
				    transfer-function
				    (cdr threshold)))))



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
  (define (iter-by-r association-in weight-in-row response-in response-real) ;; by response in weight list (row)
    (if (null? weight-in-row)
	'()
	(begin
	  (if *debug-print*
	      (format #t "c: ~a ~a\n"
		      (car weight-in-row)
		      (if (= (car response-in) (car response-real))
			  " correct weight"
			  " need update weight")))

	  (cons (if (= (car response-in) (car response-real))
		    (car weight-in-row)
		    (iter-by-a association-in (car weight-in-row) (car response-in)))
		(iter-by-r association-in (cdr weight-in-row) (cdr response-in) (cdr response-real))))))

  (define (iter-by-a a-in weight-in-column r-in) ;; by association in weight list (column)
    (if (null? weight-in-column)
	'()
	(begin
	  (format #t "\tw:~1,1f\ta:~d\tr:~1,1f\t?:~a\n"
		  (car weight-in-column)
		  (car a-in)
		  r-in
		  (if (= (car a-in) 1)
		      (if (= r-in 1)
			  "m"
			  "p")
		      "o"))

	  (cons (+ (car weight-in-column)
		   (if (= (car a-in) 1)
		       (if (= r-in 1)
			   -1
			   1)
		       0))
		(iter-by-a (cdr a-in) (cdr weight-in-column) r-in)))))


  (let ((tmp-association (calculate-neuron-layer sensor weight-sa transfer-function-a threshold-a)))
    (let ((tmp-response (calculate-neuron-layer tmp-association weight-ar transfer-function-r threshold-r)))
      (begin
	(format #t "Temporary: association layer ~a\tresponse layer ~a\n"
		tmp-association tmp-response)
	(iter-by-r tmp-association weight-ar tmp-response real-result)))))
