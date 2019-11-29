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
;; echo wait 10sec and press Ctrl-c; ./datagen.scm >! test.dat; gnuplot -e "plot 'test.dat' ; pause -1 'press enter'"



;;; History:

;; Project started at 2019-11(Nov)-22



;;; Code:

(use-modules (ice-9 format))

(load "../../../../../battery-scheme/vector.scm")


(set! *random-state* (random-state-from-platform))
;(set! *random-state* (seed->random-state 0.12345))

(define *stop* #f)

(define (sig-handler sig)
  (set! *stop* #t))

(map (lambda (x) (sigaction x sig-handler))
     (list SIGINT ;; use Ctrl-c to correct quit
	   SIGTERM ;; termination signal
	   ;; very strange, but without SIGPIPE all work correct
	   ;;SIGPIPE ;; write on a pipe with no one to read it
	   SIGHUP)) ;; controlling terminal is closed


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
;; 	            /-/       \-\                               .
;; 	 .         //           \\                        .   ... .
;; 	       	  ( | .  rim    | )             rare nebula-> .*.
;; 	           \\           //                .            .  .
;; 	            \-\       /-/
;; 	              ---------
;; 		.        ---       
;; 
;;		      ..    .  .    .        .
;;	.	   . --.m-+++... .
;;		   --m###+#%+ #---		      #####.#########
;;		 .*+m#.#####*-+%+-	.	       ##.#########.##
;;	       ...+-# nebula m%#m-------------------+ ## rectangle ###_____________________\
;;		 -.%###########m%#++----------------+ ###############                      / x
;;	.	. -mm--######++%#+   long rectangle   ##########.#####         .
;;		   - -%##%#%m-+-.	 ..	 .    ##.###########.#
;;		  .  ..+.-   . -  -.
;;
(define (print-shape old-shape)
  (let ((shape (if (< old-shape 0) (random 1000) old-shape)))
    (cond (                   (< shape 660) (rim-shape 2.0 5.0 0.0 20.0))
	  ((and (>= shape 660) (< shape 810)) (rectangle-shape 5.0 5.0 10 -2.5))
	  ((and (>= shape 810) (< shape 820)) (rectangle-shape 10.0 0.1 0.0 -0.5))
	  ((and (>= shape 820) (< shape 999)) (format #t "~,2f ~,2f\n" (random:normal) (random:normal))) ; nebula
	  (else        (format #t "~,2f ~,2f\n" (+ 12.5 (random:normal)) (+ 20.0 (random:normal))))) ; rare nebula
    shape))



(define (main sequence-counter shape)
  (if (not *stop*)

      ;; random signal not jump from one figure to other,
      ;; but select one and many times set random point in this figure,
      ;; then choise other figure and some time set random points in this new figure,
      ;; ...
      (if (< sequence-counter 0)
	  (main (random 100) (print-shape -1)) ;; counter<0 select new shape; counter = random
	  (main (1- sequence-counter) (print-shape shape))))) ;; counter>=0 repeat old shape; (dec counter)

(main 0 0)
