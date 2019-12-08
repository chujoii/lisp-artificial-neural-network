#!/usr/bin/guile -s
!#
; coding: utf-8

;;;; heavy-load-datagen.scm --- generate random data with simple shape



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
;; guile heavy-load-datagen.scm
;; or
;; ./heavy-load-datagen.scm
;;
;; run
;; wait some time
;; press Ctrl-c




;;; History:

;; Project started at 2019-08(Aug)-28



;;; Code:

(use-modules (ice-9 format))

(load "../../../../../battery-scheme/list.scm")


(set! *random-state* (random-state-from-platform))
;(set! *random-state* (seed->random-state 0.12345))

(define *dimension-of-sensor* 1000)

(define *stop* #f)

(define (sig-handler sig)
  (set! *stop* #t))

(map (lambda (x) (sigaction x sig-handler))
     (list SIGINT ;; use Ctrl-c to correct quit
	   SIGTERM ;; termination signal
	   ;; very strange, but without SIGPIPE all work correct
	   ;;SIGPIPE ;; write on a pipe with no one to read it
	   SIGHUP)) ;; controlling terminal is closed



(define (main)
  (if (not *stop*)
      (begin
	(format #t "~a" (string-join
			 (map (lambda (x) (format #f "~,2f" x))
			      (create-list-of-n-element-filled-by-evaluated-function *dimension-of-sensor* (lambda () (* 100.0 (random:uniform))))) " "))
	(newline)
	(main))))

(main)
