; coding: utf-8

;;;; growing-neural-gas.scm ---  simple growing neural gas based on algorithm from
;;;; "https://ru.wikipedia.org/wiki/Нейронный_газ"
;;;; "https://en.wikipedia.org/wiki/Neural_gas"
;;;; "http://www.ks.uiuc.edu/Publications/Papers/PDF/MART91B/MART91B.pdf" Thomas Martinetz and Klaus Schulten (1991). "A "neural gas" network learns topologies" (PDF). Artificial Neural Networks. Elsevier. pp. 397–402.



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



;;; Keywords: algorithm neuron network machine learning growing neural gas



;;; Usage:

;; use it



;;; History:

;; Project started at 2019-11(Nov)-14



;;; Code:

;;; neuron (node):
;;; ((weights) local-error (ages))
;;; (weights) - list of weights for input vector from sensors
;;; local-error - number = old-local-error + (d(Weights, Sensors))^2
;;; (ages) - list of age between neurons
;;;
;;; gag = (abc   de   f)
;;;
;;;                  a b c d e f
;;; (    ((1 2 3) 0 (0 1 3 0 0 0))         ; a   age of connections: a~b=1
;;;      ((1 2 3) 0 (1 0 2 0 0 0))         ; b   age of connections: b~c=2
;;;      ((1 2 3) 0 (3 2 0 0 0 0))         ; c   age of connections: c~a=3
;;;      ((1 2 3) 0 (0 0 0 0 4 0))         ; d   age of connections: d~e=4
;;;      ((1 2 3) 0 (0 0 0 4 0 0))         ; e
;;;      ((1 2 3) 0 (0 0 0 0 0 0))    )    ; f   not connected



(load "../../../util/battery-scheme/list.scm")



(define (add-ages old-ages)
  (cons 0 old-ages))



(define (make-neuron dimension-sensor numbers-of-neurons ages)
  (list
   (create-list-of-n-element-filled-by-evaluated-function dimension-sensor random:normal) ; weights
   0.0 ; local-error
   (add-ages ages))) ; ages



(define (add-neuron neuron gng)
  (cons neuron gng))



(define (growing-neural-gas gng)
  gng)
