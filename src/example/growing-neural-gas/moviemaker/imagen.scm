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

;; you can remove (or rename) stored "knowledge-base.scm"
;; for start learning from zero
;; mv knowledge-base.scm old-knowledge-base.scm
;;
;; mkdir image-cluste image-2D
;; ./datagen.scm | ./imagen.scm
;; cd image-cluster
;; ../forall.sh neato -Tpng -O
;; view images (feh, ...)
;; cd ../image-2D
;; ../forall.sh fungnuplot
;; view images (feh, ...)
;;
;; or in one string:
;; rm -rf image-cluster/* image-2D/*; echo '\n\n\nwait for loading and compiling knowledge base ...\n\n\n'; ./datagen.scm | ./imagen.scm; cd image-2D; ../forall.sh fungnuplot; cd ../image-cluster/; geeqie . & ../forall.sh neato -Tpng -O ; cd ..


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

;; Adaptation step (add neuron in each *lambda-step* to network)
;;
;; Inactived nodes may appear if lambda=very_small (high frequency
;; insertion).
;;
;; For lambda=big start to learning goes very smoothly, but rough
;; clusters are created
(define *lambda-step* 20) ;; (try 2, 20, 200)

;; epoch counter
(define *epoch* 1)

(define *limit-network-size* 100)

(define *dimension-of-sensor* 2)

;; from sensor you get: (a b c d);
;; for view in tooltip only "a" and "b" set list to *list-for-print-tooltip*==(0 1)
(define *list-for-print-tooltip* (list 0 1))

;; limit for weights in format:
;; ((lo-lim0 hi-lim0) (lo-lim1 hi-lim1) (lo-lim2 hi-lim2) ... (lo-limN hi-limN))
(define *limit-weight* (list (list 10.0 15.0) (list -2.5 2.5))) ;; select cube


(define *image-log-file-step* (* 10 *lambda-step*))

(define *initial-gng* (add-neuron (make-neuron *dimension-of-sensor* 1)
				  (add-neuron (make-neuron *dimension-of-sensor* 0)
					      '())))
(update-neuron-conn-age 0 1 + 1 *initial-gng*) ;; need create link beetwin first neuron!

(if (file-exists? "knowledge-base.scm")
    (set! *initial-gng* (load "knowledge-base.scm")))


(define *stop* #f)

(define (sig-handler sig)
  (set! *stop* #t))

(map (lambda (x) (sigaction x sig-handler))
     (list SIGINT ;; use Ctrl-c to correct quit
	   SIGTERM ;; termination signal
	   SIGPIPE ;; write on a pipe with no one to read it
	   SIGHUP)) ;; controlling terminal is closed


(define (generate-port-positions limit-weight gng)
  ;; list of node number:
  ;;(define *port-positions* (list -1    -1    -1   -1    -1    -1   -1    -1   -1)) == (make-list 9 -1)
  ;; possible positions:           "n", "ne", "e", "se", "s", "sw", "w", "nw", "c"
  (define *port-positions* (make-list 9 -1)) ;; fixme: very strange: define (clear) and list-set
  (list-set! *port-positions*
	     *compass-point-c* ;; "c" (center)
	     (car (find-index-of-two-minimal (calculate-distance-weight-sensor (map (lambda (x) (/ (apply + x) 2.0)) limit-weight) gng)))) ;; find nearest node for calculated center (calculation based on limit-weight)
  *port-positions*)



(define (main epoch-counter gng)
  (format #t "~d\n" epoch-counter)
  (let ((sensor-data (read-check-convert-line *dimension-of-sensor*)))
    (if (or  *stop* (null? sensor-data))
	gng
	(let ((new-gng (growing-neural-gas
			epoch-counter
			sensor-data
			gng)))
	  (if (= 0 (remainder epoch-counter *image-log-file-step*)) ; print only one image of *image-log-file-step*
	      (begin
		(gng-to-dot-file *list-for-print-tooltip*
				 (generate-port-positions *limit-weight* new-gng)
				 *limit-weight* sensor-data *winners*
				 new-gng
				 (format #f "image-cluster/~8,'0d.gv" epoch-counter))
		(display-to-file (format #f "image-2D/~8,'0d.dat" epoch-counter) (weights-to-string (map get-neuron-weight new-gng)))))
	  (main (1+ epoch-counter)
		new-gng)))))

(create-if-not-exist-dir "image-cluster")
(create-if-not-exist-dir "image-2D")

;;(gng-to-dot-file *list-for-print-tooltip* *limit-weight* *winners* (make-list *dimension-of-sensor* 0.0) *initial-gng* (format #f "image-cluster/~8,'0d.gv" *epoch*))

(display-to-file "knowledge-base.scm" (print-gng-as-list (main (1+ *epoch*) *initial-gng*)))
