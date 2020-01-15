#!/usr/bin/guile -s
!#
; coding: utf-8

;;;; perceptron-random-data-learn-by-formula.scm ---  simple usage of perceptron for learn.
;;;; Very strange situation: you have simple formula, but use intricate perceptron


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

;; guile perceptron-random-data-learn-by-formula.scm
;; or
;; ./perceptron-random-data-learn-by-formula.scm



;;; History:



;;; Code:

(load "../../../perceptron.scm")
(load "../../../../../battery-scheme/list.scm")
(load "../../../../../battery-scheme/print-list.scm")

(set! *random-state* (random-state-from-platform))
;(set! *random-state* (seed->random-state 1.2345))

(define epoch-limit 200)
(define start-weight-step 100.0)

;;; Use #t for debug print, or #f for silent
(define *debug-print* #f)


;; error value
(define *error-value* 0.01)

;; increase standard-deviation of normal random distribution for best learn practics
;; random normal deviation = (* width-deviation (cadar dimension) (random:normal))
; 0.7
(define width-deviation 1.0)

;;; Set dimensoin of sensor in format: (list mean standard-deviation)
(define dimension-sensor (list (list 230.0 23.0)   ;; 230 В ±10 %
			       (list 230.0 23.0)
			       (list 230.0 23.0)
			       (list  50.0  0.2))) ;; 50 ±0,2 Гц
;;; Set number of output units
(define number-response 1)









;;; Calculate approximate value of number hidden
;;; Nhidden = (2/3)Nin + Nout
;(define number-association (ceiling (+ (/ (* 2 (length dimension-sensor)) 3)  number-response)))
(define number-association 4)


(define (generate-random-sensor-data dimension)
  (if (null? dimension)
      '()
      (cons (+ (caar dimension) (* width-deviation (cadar dimension) (random:normal))) ; increase standard-deviation of normal random distribution for best learn practics
	    ;; (caar dimension) mean of normal distribution
	    ;; (cadar dimension) standard-deviation
	    (generate-random-sensor-data (cdr dimension)))))


;;; weight: Sensor (input units) --- Association (hidden units)
;;; A.S
(define (set-random-weight-sa numin numout)
  (if (<= numout 0)
      '()
      (cons (create-list-of-n-element-filled-by-evaluated-function numin random:mop) (set-random-weight-sa numin (- numout 1)))))

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
(define (formula-for-correct-answer dimension sensor-data)
  (define (compare dim sen)
    (if (null? dim)
	'()
	;; caar dimension = value; cdar dimension = d_value; car sensor-data = real_value
	(cons (if (and (>   (apply + (car dim))   (car sen))
		       (<   (apply - (car dim))   (car sen)))
		  0 1) ; 0 all ok; 1 error
	      (compare (cdr dim) (cdr sen)))))

  (let ((result (compare dimension sensor-data))) ; very strange
    (if *debug-print* (format #t "formula result ~a => ~a\n" result (if (>= (apply + result) *error-value*) "1 breakage" "0 normal")))
    (list (if (>= (apply + result) *error-value*) 1 0))))   ; 0 for normal; 1 for breakage







(format #t "Sensor (input) layer size = ~d\n\n" (length dimension-sensor))
(format #t  "Association (hidden) layer size = ~d\n\n" number-association)
(format #t "Response (output) layer size = ~d\n\n" number-response)
(define weight-sa (set-random-weight-sa (length dimension-sensor) number-association))
(format #t "Weight Sensor-Association\n") (print-list-without-bracket weight-sa)
(format #t "\nThreshold Association ~a\n\n" threshold-a)
(define initial-weight-ar (set-random-weight-ar number-association number-response))
(format #t "Weight Association-Response\n") (print-list-without-bracket initial-weight-ar)
(format #t "\nThreshold Response ~a\n\n" threshold-r)









(format #t "simple calculate one layer perceptron\nwith update weight of one layer perceptron\n")
(define learn-ok 0)
(define learn-err 0)

(define (cycle-learn weight-ar limit correction-scale)
  (format #t "\n---------------------------=[ ~d ~e]=---------------------------\t" limit (/ limit correction-scale))
  (let ((data (generate-random-sensor-data dimension-sensor)))
    (if *debug-print* (format #t "Random generated Sensors value ~a\n" data))
    (if (> limit 0)
	(if (= (car (formula-for-correct-answer dimension-sensor data)) 0); 0 for normal; 1 for breakage
	    (set! learn-ok (1+ learn-ok))
	    (set! learn-err (1+ learn-err))))
    (format #t "Random generated Sensors value ~a ~d Nok=~d Nerr=~d\t" data (car (formula-for-correct-answer dimension-sensor data)) learn-ok learn-err)
    (if (<= limit 0)
	weight-ar
	(cycle-learn (calculate-weight-sar data
					   weight-sa transfer-function-step threshold-a
					   weight-ar transfer-function-step threshold-r
					   (formula-for-correct-answer dimension-sensor data)
					   (/ limit correction-scale)
					   *error-value*)
		     (- limit 1) correction-scale))))

(define learned-weight-ar (cycle-learn initial-weight-ar epoch-limit start-weight-step))

(format #t "\ntesting Ua=207.0--253.0, Ub=207.0--253.0, Uc=207.0--253.0, F=49.8--50.2:\n")
(define test-ok 0)
(define test-err 0)

(define (testing)
  (do ((ua 200.0 (+ ua 2.0)))
      ((>= ua 260.0))
    (do ((ub 200.0 (+ ub 2.0)))
	((>= ub 260.0))
      (do ((uc 200.0 (+ uc 2.0)))
	  ((>= uc 260.0))
	(do ((fa 49.0 (+ fa 0.1)))
	    ((>= fa 51.0))
	  (let ((tmp-formula (car (formula-for-correct-answer dimension-sensor (list ua ub uc fa))))
		(tmp-perceptron (car (perceptron-sar (list ua ub uc fa)
						     weight-sa transfer-function-step threshold-a
						     learned-weight-ar transfer-function-step threshold-r))))
	    (if *debug-print*
		(format #t "~f ~f ~f ~3,1f\t~a\n" ua ub uc fa
			(if (= tmp-formula tmp-perceptron)
			    "ok" "mismatch"))
		(if (= tmp-formula tmp-perceptron)
		    (display "=")
		    (display "#")))

	    (if (= tmp-formula tmp-perceptron)
		(set! test-ok (1+ test-ok))
		(set! test-err (1+ test-err)))))))))

(testing)

(format #t "\nnumber of learn measurements (epoch) = ~d;\tnumber of breakage examples = ~d (~6,2f%);\tnumber of normal examples = ~d (~6,2f%);\n\n"
	(+ learn-err learn-ok) ; == epoch-limit

	learn-err
	(/ (* 100.0 learn-err) (+ learn-err learn-ok))

	learn-ok
	(/ (* 100.0 learn-ok) (+ learn-err learn-ok)))

(format #t "testing Ua=207.0--253.0, Ub=207.0--253.0, Uc=207.0--253.0, F=49.8--50.2:\n")
(format #t "number of test measurements = ~d;\tmismatch calculations = ~d (~6,2f%);\tgood calculations = ~d (~6,2f%);\n\n"
	(+ test-err test-ok)

	test-err
	(/ (* 100.0 test-err) (+ test-err test-ok))

	test-ok
	(/ (* 100.0 test-ok) (+ test-err test-ok)))

(newline)
(format #t "Association (hidden) layer size = ~d\n\n" number-association)
(format #t "Response (output) layer size = ~d\n\n" number-response)
(format #t "Weight Sensor-Association\n") (print-list-without-bracket weight-sa)
(format #t "\nThreshold Association ~a\n\n" threshold-a)
(format #t "\nLearned Weight Association-Response\n") (print-list-without-bracket learned-weight-ar)
(format #t "\nThreshold Response ~a\n\n" threshold-r)
