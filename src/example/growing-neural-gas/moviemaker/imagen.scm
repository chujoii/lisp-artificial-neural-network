#!/usr/bin/guile -s
!#
; coding: utf-8

;;;; imagen.scm ---  script for growing-neural-gas to generate result image



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



;;; Keywords: algorithm neuron network machine learning growing neural gas image



;;; Usage:

;; mkdir tmp
;; ./datagen.scm | ./imagen.scm
;; cd image-cluster
;; ../forall.sh neato -Tpng -O
;; view images (feh, ...)
;; cd ../image-2D
;; ../forall.sh fungnuplot
;; view images (feh, ...)


;;; History:

;; Project started at 2019-11(Nov)-23



;;; Code:

(use-modules (ice-9 format))

(load "../../../growing-neural-gas.scm")
(load "../../../visualization/gng-to-dot.scm")
(load "../../../../../battery-scheme/print-list.scm")
(load "../../../fetch-data.scm")

;;; Use #t for debug print, or #f for silent
(define *debug-print* #f)

;;; adaptation coefficients for weight and local-error
(define *eps-winner*   0.01)
(define *eps-neighbour* 0.0001)
(define *eps-local-error* 0.05)
(define *factor-beta-decrease-local-error* 0.9)

(define *limit-conn-age* 15)

;; adaptation step (add neuron in each *lambda-step* to network)
(define *lambda-step* 20)

;; epoch counter
(define *epoch* 1)

(define *limit-network-size* 100)

(define *dimension-of-sensor* 2)

(define *image-log-file-step* 100)

(define *initial-gng* (add-neuron (make-neuron *dimension-of-sensor* 1)
				  (add-neuron (make-neuron *dimension-of-sensor* 0)
					      '())))
(update-neuron-conn-age 0 1 + 1 *initial-gng*) ;; need create link beetwin first neuron!



(define (main epoch-counter gng)
  (format #t "~d\n" epoch-counter)
  (let ((data (read-check-convert-line *dimension-of-sensor*)))
    (if (null? data)
	gng
	(let ((new-gng (growing-neural-gas
			epoch-counter
			data
			gng)))
	  (if (= 0 (remainder epoch-counter *image-log-file-step*)) ; print only one image of *image-log-file-step*
	      (begin
		(gng-to-dot-file '() *winners*
				 new-gng
				 (format #f "image-cluster/~8,'0d.gv" epoch-counter))
		(display-to-file (format #f "image-2D/~8,'0d.dat" epoch-counter) (weights-to-string (map get-neuron-weight new-gng)))))
	  (main (1+ epoch-counter)
		new-gng)))))

(create-if-not-exist-dir "image-cluster")
(create-if-not-exist-dir "image-2D")

(gng-to-dot-file '() *winners* *initial-gng* (format #f "image-cluster/~8,'0d.gv" *epoch*))

(main (1+ *epoch*) *initial-gng*)
