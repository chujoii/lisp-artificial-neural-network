#!/usr/bin/guile -s
!#
; coding: utf-8

;;;; example.scm --- generate random data with simple shape



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



;;; Keywords: algorithm neuron network machine learning data generator



;;; Usage:

;; for view (press Ctrl-c for stop generating numbers):
;; guile datagen.scm
;; or
;; ./datagen.scm
;;
;; test sapes:
;; run
;; wait some time for collect data (~10 seconds)
;; press Ctrl-c (for step generating numbers)
;; view doted graph (press Ctrl-c or Enter for exit from gnuplot):
;; ./datagen.scm >! test.dat; gnuplot -e "plot 'test.dat' ; pause -1 'press enter'"



;;; History:

;; Project started at 2019-11(Nov)-22



;;; Code:

(use-modules (ice-9 format))

(load "../../../../../../util/battery-scheme/vector.scm")


(set! *random-state* (random-state-from-platform))
;(set! *random-state* (seed->random-state 0.12345))

(define *stop* #f)


(define (rectangle-shape side-a side-b dx dy)
  (format #t "~,2f ~,2f\n"
	  (+ (* (random:uniform) side-a) dx)
	  (+ (* (random:uniform) side-b) dy)))



(define (rim-shape radius-in radius-out dx dy)
  (let ((x (- (* (random:uniform) radius-out 2.0) radius-out))
	(y (- (* (random:uniform) radius-out 2.0) radius-out)))
    (if (and (< (+ (square x) (square y)) (square radius-out))
	     (> (+ (square x) (square y)) (square radius-in)))
	(format #t "~,2f ~,2f\n"
		(+ x dx)
		(+ y dy))
	(rim-shape radius-in radius-out dx dy))))

;;                     y /\
;;                       |
;;
;;		        _____    
;; 	              ---------
;; 	            /-/       \-\
;; 	 .         //           \\
;; 	       	  ( | .         | )
;; 	           \\           //                .               .
;; 	            \-\       /-/
;; 	              ---------
;; 		.        ---       
;; 
;;		      ..    .  .    .        .
;;	.	   . --.m-+++... .
;;		   --m###+#%+ #---		      +--------------+
;;		 .*+m#.#####*-+%+-	.	      |            . |
;;	       ...+-#########m%#m-------------------+ |      .       |_____________________\
;;		 -.%###########m%#++----------------+ |              |                     / x
;;	.	. -mm--######++%#+		      |         .    |         .
;;		   - -%##%#%m-+-.	 ..	 .    +--------------+
;;		  .  ..+.-   . -  -.
;;
(define (random-shape)
  (let ((shape (random 100)))
    (cond (                   (< shape 66) (rim-shape 2.0 5.0 0.0 20.0))
	  ((and (>= shape 66) (< shape 81)) (rectangle-shape 5.0 5.0 10 -2.5))
	  ((and (>= shape 81) (< shape 82)) (rectangle-shape 10.0 0.1 0.0 -0.5))
	  (     (>= shape 82) (format #t "~,2f ~,2f\n" (random:normal) (random:normal)))))) ; nebula



(define (sig-handler sig)
  (set! *stop* #t))



;; use Ctrl-c to correct quit
(sigaction SIGINT sig-handler)



(while (not *stop*) (random-shape))
