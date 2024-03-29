#!/usr/bin/guile -s
!#
; coding: utf-8

;;;; unit-testing.scm ---  Unit testing for growing-neural-gas code



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



;;; Keywords: algorithm neuron network machine learning growing neural gas



;;; Usage:

;; guile unit-testing.scm
;; or
;; ./unit-testing.scm



;;; History:

;; Project started at 2019-08(Aug)-28



;;; Code:

(use-modules (ice-9 format))

(load "../../../growing-neural-gas.scm")
(load "../../../../../battery-scheme/dir-and-file.scm")


(set! *random-state* (seed->random-state 0.12345))

;;; Use #t for debug print, or #f for silent
(define *debug-print* #t)

;;; adaptation coefficients for weight and local-error
(define *eps-winner*   0.1)
(define *eps-neighbour* 0.01)
(define *eps-local-error* 0.5)
(define *factor-beta-decrease-local-error* 0.1)

(define *limit-conn-age* 3)

;; Adaptation step (add neuron in each *lambda-step* to network)
;;
;; Inactived nodes may appear if lambda=very_small (high frequency
;; insertion).
;;
;; For lambda=big start to learning goes very smoothly, but rough
;; clusters are created
(define *lambda-step* 1)

(define *limit-network-size* 100)

(define *dimension-of-sensor* 4)

;;; Sensor (input units)
(define *example-sensor* (list 10 20 30 40))


(format #t "simple 2 initial neurons:\n\tweight\t\t\t\tconn-age\tlocal-error\n")
(define *initial-gng* (add-neuron (make-neuron *dimension-of-sensor* 1)
				  (add-neuron (make-neuron *dimension-of-sensor* 0)
					      '())))
(map print-neuron *initial-gng*) (newline)

(format #t "\nupdate neuron connection age for 0 1\n")
(update-neuron-conn-age 0 1 + 1 *initial-gng*) ;; need create link beetwin first neuron!
(map print-neuron *initial-gng*)

(format #t "\nsimple 6 neurons (see ../../../growing-neural-gas.scm):\n")
(define *example-gng* (add-neuron (make-neuron *dimension-of-sensor* 5)
				  (add-neuron (make-neuron *dimension-of-sensor* 4)
					      (add-neuron (make-neuron *dimension-of-sensor* 3)
							  (add-neuron (make-neuron *dimension-of-sensor* 2)
								      (add-neuron (make-neuron *dimension-of-sensor* 1)
										  (add-neuron (make-neuron *dimension-of-sensor* 0)
											      '())))))))
(map print-neuron *example-gng*)

(format #t "\nmanual change weight (random:normal too hard for human readable):\n")
(list-set! (list-ref *example-gng* 0) *index-neuron-weight* (list 0.1 0.2 0.3 0.4))
(list-set! (list-ref *example-gng* 1) *index-neuron-weight* (list 1.1 1.2 1.3 1.4))
(list-set! (list-ref *example-gng* 2) *index-neuron-weight* (list 2.1 2.2 2.3 2.4))
(list-set! (list-ref *example-gng* 3) *index-neuron-weight* (list 3.1 3.2 3.3 3.4))
(list-set! (list-ref *example-gng* 4) *index-neuron-weight* (list 4.1 4.2 4.3 4.4))
(list-set! (list-ref *example-gng* 5) *index-neuron-weight* (list 5.1 5.2 5.3 5.4))
(map print-neuron *example-gng*)

(format #t "\nupdate weight neuron number 3 (new_weight_vector = weight_vector + eps*(veight_vector - sensor_vector)):\n")
(update-neuron-weight-vector 3 (lambda (weights step) (sum-sub-vectors + weights (mul-div-vector-const * (sum-sub-vectors - weights *example-sensor*) step))) *eps-winner* *example-gng*)
(map print-neuron *example-gng*)

(format #t "\nupdate connection age:\n")
(update-neuron-conn-age 0 1 + 1 *example-gng*) ;; need create link beetwin first neuron!
(update-neuron-conn-age 1 2 + 2 *example-gng*)
(update-neuron-conn-age 2 0 + 3 *example-gng*)
(update-neuron-conn-age 3 4 + 4 *example-gng*)
(map print-neuron *example-gng*)

(update-neuron-local-error 0 + 0.9 *example-gng*)
(update-neuron-local-error 1 + 0.5 *example-gng*)
(update-neuron-local-error 2 + 0.8 *example-gng*)
(update-neuron-local-error 3 + 0.2 *example-gng*)
(update-neuron-local-error 4 + 0.3 *example-gng*)
(update-neuron-local-error 5 + 0.7 *example-gng*)
(format #t "\nupdate local error (last column):\n")
(map print-neuron *example-gng*)



(format #t "\nCalculate distance between Weight (neuron number 3) and Sensor ~a:\n(~7,2f ~7,2f ~7,2f ~7,2f ...) compare with:\n"
	*example-sensor*
	(euclidean-distance-vector  (get-neuron-weight (car *example-gng*)) *example-sensor*)
	(euclidean-distance-vector  (get-neuron-weight (cadr *example-gng*)) *example-sensor*)
	(euclidean-distance-vector  (get-neuron-weight (caddr *example-gng*)) *example-sensor*)
	(euclidean-distance-vector  (get-neuron-weight (cadddr *example-gng*)) *example-sensor*))
(format #t "~a\n" (calculate-distance-weight-sensor *example-sensor* *example-gng*))


(format #t "\n\n\nsimple 3 neurons with angle:\n")
(define *example-gng-with-angle* (add-neuron (make-neuron *dimension-of-sensor* 2)
					     (add-neuron (make-neuron *dimension-of-sensor* 1)
							 (add-neuron (make-neuron *dimension-of-sensor* 0)
								     '()))))
(list-set! (list-ref *example-gng-with-angle* 0) *index-neuron-weight* (list 0.1 0.2 270 0.4))
(list-set! (list-ref *example-gng-with-angle* 1) *index-neuron-weight* (list 1.1 1.2 10 1.4))
(list-set! (list-ref *example-gng-with-angle* 2) *index-neuron-weight* (list 2.1 2.2 300 2.4))
(map print-neuron *example-gng-with-angle*)

(define *example-sensor-with-angle* (list 10             20                 350            40))
;;                                        x              x                  alpha          x
(define *functions-mixed-space* (list euclidean-distance euclidean-distance angle-distance euclidean-distance))
(format #t "\nSensor with angle ~a:\n" *example-sensor-with-angle*)
(format #t "\nMinimal distance will be for neuron number 1 (count from 0), because 10 degree nearest to 350 degree (distance=20 degree):\n")
(format #t "Distance between Weight (with angle) and Sensor (with angle) calculations (also with respect to angle):\n")
(format #t "~a\n" (calculate-distance-in-mixed-space-weight-sensor *functions-mixed-space* *example-sensor-with-angle* *example-gng-with-angle*))


(format #t "\nFind indexes of two minimal elementt (first correct answer, then calculated answer)\n")
;
(format #t "(0 1) ~a\n" (find-index-of-two-minimal (list 1 1)))
(format #t "(0 1) ~a\n" (find-index-of-two-minimal (list 1 2)))
(format #t "(1 0) ~a\n" (find-index-of-two-minimal (list 2 1)))
;
(format #t "(0 1) ~a\n" (find-index-of-two-minimal (list 1 1 2)))
(format #t "(0 2) ~a\n" (find-index-of-two-minimal (list 1 2 1)))
(format #t "(1 2) ~a\n" (find-index-of-two-minimal (list 2 1 1)))
;
(format #t "(0 1) ~a\n" (find-index-of-two-minimal (list 1 2 3)))
(format #t "(0 2) ~a\n" (find-index-of-two-minimal (list 1 3 2)))
(format #t "(1 0) ~a\n" (find-index-of-two-minimal (list 2 1 3)))
(format #t "(2 0) ~a\n" (find-index-of-two-minimal (list 2 3 1)))
(format #t "(1 2) ~a\n" (find-index-of-two-minimal (list 3 1 2)))
(format #t "(2 1) ~a\n" (find-index-of-two-minimal (list 3 2 1)))


(format #t "\nsimple 6 neurons (repeat):\n")
(map print-neuron *example-gng*)
(format #t "\nprint neighbour for neuron number 2:\n(number 0 (conn-age=2), number 1 (conn-age = 1), nc, nc, nc, nc):\n~a\n" (get-neuron-conn-age (list-ref *example-gng* 2)))
(format #t "\nupdate weight for this neurons (0 and 1):\n")
(update-neighbours-weights (lambda (weights step) (sum-sub-vectors + weights (mul-div-vector-const * (sum-sub-vectors - weights *example-sensor*) step))) (list 2 1 -1 -1 -1 -1) *eps-neighbour* *example-gng*)
(map print-neuron *example-gng*)

(format #t "\nprint neighbour for neuron number 3:\n(nc, nc, nc, nc, number 4 (conn-age = 3), nc):\n~a\n" (get-neuron-conn-age (list-ref *example-gng* 3)))
(format #t "\nupdate weight for this neuron (4):\n")
(update-neighbours-weights (lambda (weights step) (sum-sub-vectors + weights (mul-div-vector-const * (sum-sub-vectors - weights *example-sensor*) step))) (list -1 -1 -1 -1 3 -1) *eps-neighbour* *example-gng*)
(map print-neuron *example-gng*)

(format #t "\nincrease by 1 all connections form neuron print neighbour for winner (for example neuron number 0 (use different neuron only for example)):\n")
(inc-neighbours-conn-age 0 *example-gng*)
(map print-neuron *example-gng*)

(format #t "\nincrease by 1 all connections form neuron print neighbour (for example neuron number 3 (use different neuron only for example)):\n")
(inc-neighbours-conn-age 3 *example-gng*)
(map print-neuron *example-gng*)

(format #t "\nset connection to 0 (*initial-connection-age*) between two winners (1 2):\n")
(update-neuron-conn-age 1 2 * *initial-connection-age* *example-gng*)
;; or: (update-neuron-conn-age 1 2 (lambda (ignored-value val) val) *initial-connection-age* *example-gng*)
(map print-neuron *example-gng*)

(format #t "\ncopy artificial neural network (because next two manipulation decrease size of net):\n")
(format #t "\nif age > limit (~d), then remove connection:\n" *limit-conn-age*)
(define *example2-gng* (remove-old-conn-age *limit-conn-age* *example-gng*))
(map print-neuron *example2-gng*)

(define *k-utility* 2)
(format #t "\nset k-utility to ~f\n" *k-utility*)
(format #t "remove neurons with min utility-factor (first element (if utility-factor equal)):\n")
(set! *example2-gng* (find-and-del-neuron-with-min-utility-factor *k-utility* *example2-gng*))
(map print-neuron *example2-gng*)

(format #t "repeat deletion:\n")
(set! *example2-gng* (find-and-del-neuron-with-min-utility-factor *k-utility* *example2-gng*))
(map print-neuron *example2-gng*)
(format #t "repeat deletion:\n")
(set! *example2-gng* (find-and-del-neuron-with-min-utility-factor *k-utility* *example2-gng*))
(map print-neuron *example2-gng*)
(format #t "repeat deletion:\n")
(set! *example2-gng* (find-and-del-neuron-with-min-utility-factor *k-utility* *example2-gng*))
(map print-neuron *example2-gng*)
(format #t "last repeat deletion (because size of network=2 and nothing will be deleted):\n")
(set! *example2-gng* (find-and-del-neuron-with-min-utility-factor *k-utility* *example2-gng*))
(map print-neuron *example2-gng*)


(format #t "\ncopy artificial neural network (because next two manipulation decrease size of net)\n")
(set! *example2-gng* *example-gng*)
(format #t "set <<random>> utility factor:\n")
(update-neuron-utility-factor 0 + 0.9 *example-gng*)
(update-neuron-utility-factor 1 + 0.4 *example-gng*)
(update-neuron-utility-factor 2 + 0.5 *example-gng*)
(update-neuron-utility-factor 3 + 0.8 *example-gng*)
(update-neuron-utility-factor 4 + 0.7 *example-gng*)
(update-neuron-utility-factor 5 + 0.1 *example-gng*)
(map print-neuron *example2-gng*)

(format #t "\nremove neurons with min utility-factor:\n")
(set! *example2-gng* (find-and-del-neuron-with-min-utility-factor *k-utility* *example2-gng*))
(map print-neuron *example2-gng*)

(format #t "\nrestore big artificial neural network\n")
(map print-neuron *example-gng*)
(define index-max-local-error (find-neuron-index-with-max-local-error *example-gng*))
(format #t "\nindex of neuron with max local error: ~d\n" index-max-local-error)
(format #t "neighbour (for neuron number ~d) index of neuron with max local error: ~a\n"
	index-max-local-error (find-neighbours-index-with-max-local-error index-max-local-error *example-gng*))

(format #t "\nadaptive step: create new neuron:\n")
(set! *example-gng* (adaptive-step-create-new-neuron *example-gng*))
(map print-neuron *example-gng*)

(format #t "\ndecrease local-error and utility-factor:\n")
(set! *example-gng* (decrease-all-neuron-local-errors-and-utility-factor *factor-beta-decrease-local-error* *example-gng*))
(map print-neuron *example-gng*)

(format #t "\ntest list-formatted output: see file knowledge-base.scm\n")
(display-to-file "knowledge-base.scm" (print-gng-as-list *example-gng*))

(format #t "\nfrom conn-age\n")
(map (lambda (x) (format #t "~a\n" x)) (map get-neuron-conn-age *example-gng*))
(format #t "\nextract groups: ~a\n" (extract-groups-from-conn-ages (map get-neuron-conn-age *example-gng*)))
