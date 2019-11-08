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
(load "../../../util/battery-scheme/print-list.scm")

;;; Transfer function (activation function)
(define (transfer-function-step signal threshold)
  (- signal threshold))

(define (transfer-function-linear signal threshold)
  (- (cond ((< signal 0.0) 0) ;; fixme: level=const
	((> signal 1.0) 1) ;; fixme: level=const
	(else signal))
  (/ threshold 2)))

;;  (/ 1.0 (+ 1.0 (exp (* -t signal))))
;; t = 0   : sigmoid equal to step function
;; t = 0,5 : dx~(0,1)
(define (transfer-function-sigmoid signal threshold)
  (- (/ 1.0 (+ 1.0 (exp (* -1 threshold signal))))
     (/ threshold 2)))

(define (unittest-transfer-function transfer-function threshold   from step to)
  (if (< from to)
      (begin (format #t "~1,1f ~2,1f\n" from (transfer-function from threshold))
	     (unittest-transfer-function transfer-function threshold (+ from step) step to))
      (newline)))


;; uncomment for test
;(format #t "transfer-function-step\n") (unittest-transfer-function transfer-function-step 1.0   -3.0 0.2 3.0)
;(format #t "transfer-function-linear\n") (unittest-transfer-function transfer-function-linear 1.0   -3.0 0.2 3.0)
;(format #t "transfer-function-sigmoid\n") (unittest-transfer-function transfer-function-sigmoid 1.0   -3.0 0.2 3.0)

(define (normalize-element element)
  (if (>= element 0.0) 1 0))

(define (normalize-list list)
  (if (null? list)
      '()
      (cons (normalize-element (car list))
	    (normalize-list (cdr list)))))


;; random value: -1 or 0 or +1
(define (random:mop)
  (- (random 3) 1))

;; Sensor                    Association
;;
;; Sensor --- weight-sa --- [> sum --- transfer-function ---]
;; Sensor --- weight-sa __/
;;
;; calculate artificial neuron layer
(define (calculate-neuron-layer list-in weight transfer-function threshold)
  (if *debug-print*
      (if (not (null? weight))
	  (format #t "in=~a in*w=~a sum=~1,2f activated=~d (~1,2f)\n"
		  list-in
		  (map * list-in (car weight))
		  (apply + (map * list-in (car weight)))
		  (if (>= (transfer-function (apply + (map * list-in (car weight)))
					     (car threshold)) 0.0)
		      1 0)
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
  (normalize-list (calculate-neuron-layer (normalize-list (calculate-neuron-layer sensor weight-sa transfer-function-a threshold-a))
					  weight-ar transfer-function-r threshold-r)))


;; machine-learning max-calculation-error
;; return weight-ar
(define (calculate-weight-sar sensor
			      weight-sa transfer-function-a threshold-a
			      weight-ar transfer-function-r threshold-r
			      real-result ;; correct real result from teacher
			      correction-epoch
			      error-value)
  (define (iter-by-r association-in weight-in-row response-in response-real) ;; by response in weight list (row)
    (if (null? weight-in-row)
	'()
	(begin
	  (if *debug-print*
	      (format #t "weight: ~a ~a\n"
		      (car weight-in-row)
		      (if (< (abs (- (normalize-element (car response-in)) (car response-real))) error-value)
			  " good weight"
			  " need update weight")))

	  (cons (if (< (abs (- (normalize-element (car response-in)) (car response-real))) error-value)
		    (car weight-in-row)
		    (iter-by-a association-in (car weight-in-row) (car response-in) correction-epoch))
					; fixme: need change "correction-epoch" to value, that depend of response difference
					; correction = (response_threshold - sum(Ai*Wj)) / Num_of_weight_lines(j)
					; ?or?
					; correction = f(Epoch_counter)

		(iter-by-r association-in (cdr weight-in-row) (cdr response-in) (cdr response-real))))))

  (define (iter-by-a a-in weight-in-column r-in correction-speed) ;; by association in weight list (column)
    (if (null? weight-in-column)
	'()
	(begin
	  (format #t (if *debug-print* "\tw:~7,2f\ta:~d\tr:~7,2f\t(dw=~7,2f)\t?:~a\n" "~3*~a")
		      (car weight-in-column)
		      (car a-in)
		      r-in
		      correction-speed ; fixme: const -> threshold
		      (if (>= (car a-in) 0.0) ; fixme: const -> threshold
			  (if (>= r-in 0.0) ; fixme: const -> threshold
			      "m"
			      "p")
			  "o"))

	  (cons (+ (car weight-in-column)
		   (if (>= (car a-in) 0.0) ;; check association: activated neuron or not?
		       (if (>= r-in 0.0)   ;; check response: activated neuron or not?
			   (* correction-speed -1)
			   correction-speed)
		       0))
		(iter-by-a (cdr a-in) (cdr weight-in-column) r-in correction-speed)))))


  (let ((tmp-association (calculate-neuron-layer sensor weight-sa transfer-function-a threshold-a)))
    (let ((tmp-response (calculate-neuron-layer (normalize-list tmp-association) weight-ar transfer-function-r threshold-r)))
      (begin
	(if *debug-print*
	    (format #t "\nTemporary: association layer ~a\tresponse layer ~a\nnormalized: association layer ~a\tresponse layer ~a\n"
		    tmp-association tmp-response
		    (normalize-list tmp-association) (normalize-list tmp-response)))
	(iter-by-r tmp-association weight-ar tmp-response real-result)))))
