#!/usr/bin/guile -s
!#
; coding: utf-8

;;;; gng-to-dot.scm ---  Convert network generating from growing-neural-gas to dot for graphviz



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



;;; Keywords: algorithm neuron network machine learning growing neural gas dot graphviz



;;; Usage:

;; guile gng-to-dot.scm
;; or
;; ./gng-to-dot.scm



;;; History:

;; Project started at 2019-11(Nov)-21



;;; Code:

(use-modules (ice-9 format))

(load "../growing-neural-gas.scm")


(define (convert-gng-conn-ages-to-simple-list gng)
  (define (iter-x x conn-ages)
    (if (null? conn-ages)
	'()
	(append (iter-y x 0 (car conn-ages)) (iter-x (1+ x) (cdr conn-ages)))))

  (define (iter-y x y line)
    (if (null? line)
	'()
	(if (or (> x y) (< (car line) *initial-connection-age*))
	    (iter-y x (1+ y) (cdr line))
	    (cons (list x y) (iter-y x (1+ y) (cdr line))))))

  (iter-x 0 (map get-neuron-conn-age gng)))
