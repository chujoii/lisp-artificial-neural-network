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

;; guile datagen.scm
;; or
;; ./datagen.scm



;;; History:

;; Project started at 2019-11(Nov)-22



;;; Code:

(use-modules (ice-9 format))

(set! *random-state* (random-state-from-platform))
;(set! *random-state* (seed->random-state 0.12345))

(define *stop* #f)





(define (random-shape)
  (let ((shape (random 2)))
    (cond ((= shape 0) (format #t "circle (center 0, 0) ~,2f ~,2f\n" (random:normal) (random:normal)))
	  ((= shape 1) (format #t "cube (center 10, 10) ~,2f ~,2f\n" (+ (* (random:uniform) 5.0) 7.5) (+ (* (random:uniform) 5.0) 7.5))))))



(define (sig-handler sig)
  (set! *stop* #t))



;; use Ctrl-c to correct quit
(sigaction SIGINT sig-handler)



(while (not *stop*) (random-shape))
