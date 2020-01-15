; coding: utf-8

;;;; fetch-data.scm ---  script for fetch, check and convert data from stdin



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



;;; Keywords: get fetch read check convert data stdin



;;; Usage:

;;; History:

;; Project started at 2019-08(Aug)-28



;;; Code:

(use-modules (ice-9 format))
(use-modules (ice-9 rdelim))


;; fixme: need check input data
;; return null if error
;; or list of number if all ok
(define (read-check-convert-line dimension)
  (let ((str-data (read-line)))
    (if (not (and (string? str-data) (not (string-null? str-data)) (>= (string-length str-data) (- (* 2 dimension) 1))))
	'()
	(let ((num-data (map string->number (string-split str-data #\space))))
	  (if (not (= dimension (length num-data)))
	      '()
	      num-data)))))
