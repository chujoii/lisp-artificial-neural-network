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



;;; Keywords: algorithm perceptron neuron network machine learning growing neural gas



;;; Usage:

;; use it



;;; History:

;; Project started at 2019-08(Aug)-29



;;; Code:

(use-modules (ice-9 format))

;;; neuron (node):
;;; ((weights) local-error (ages))
;;; (weights) - list of weights
;;; local-error - number = old-local-error + (d(Weights, Sensors))^2
;;; (ages) - list of age between neurons

