; coding: utf-8

;;;; gng-to-gnuplot.scm ---  Convert network generating from growing-neural-gas to simple column list for gnuplot



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



;;; Keywords: algorithm neuron network machine learning growing neural gas DOT graphviz



;;; Usage:


;;; History:

;; Project started at 2019-08(Aug)-28



;;; Code:

(use-modules (ice-9 format))

(load "../growing-neural-gas.scm")
(load "../../../battery-scheme/dir-and-file.scm")
(load "gng-to-dot.scm")


(define (replace-connection-to-weight weights conn-list)
  (map (lambda (conn) (list (list-ref weights (car conn)) (list-ref weights (cadr conn))))
       conn-list))


(define (list-to-string-gnuplot weight-conn-weight-list)
  (string-join (map (lambda (x) (format #f "~f ~f\n~f ~f\n" (caar x) (cadar x) (caadr x) (cadadr x)))
		    weight-conn-weight-list) "\n"))



(define (gng-to-gnuplot-file gng filename)
  (display-to-file filename
		   (list-to-string-gnuplot
		    (replace-connection-to-weight
		     (map get-neuron-weight gng)
		     (convert-gng-conn-ages-to-simple-list gng)))))


(define *initial-gng* (load "knowledge-base.scm"))
(gng-to-gnuplot-file *initial-gng* "00000200.dat")
