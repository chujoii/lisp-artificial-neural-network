; coding: utf-8

;;;; growing-neural-gas.scm ---  simple growing neural gas based on algorithm from
;;;; "https://ru.wikipedia.org/wiki/Нейронный_газ"
;;;; "https://en.wikipedia.org/wiki/Neural_gas"
;;;;
;;;; based on main article:
;;;; "http://www.ks.uiuc.edu/Publications/Papers/PDF/MART91B/MART91B.pdf"
;;;; Thomas Martinetz and Klaus Schulten (1991).
;;;; "A "neural gas" network learns topologies".
;;;; Artificial Neural Networks. Elsevier. pp. 397–402.



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
;;; ((weights) (conn-ages) local-error),  where:
;;;    (weights) - list of weights for input vector from sensors
;;;    (conn-ages) - list of connection-age between neurons
;;;    local-error - number = old-local-error + (d(Weights, Sensors))^2
;;;
;;; gag = (abc   de   f)
;;;
;;;  connection-age == -1 not connected ("nc" or "-" in next table)
;;;  connection-age >= 0 connected
;;;
;;;      ((wieght) ( conn-age ) local-error)
;;;                a b c d e f            ; warning: duplication of conn-age data => duplication of program code
;;; (    ((1 2 3) (- 0 2 - - -) 0)         ; a   age of connections: a~b=0
;;;      ((1 2 3) (0 - 1 - - -) 0)         ; b   age of connections: b~c=1
;;;      ((1 2 3) (2 1 - - - -) 0)         ; c   age of connections: c~a=2
;;;      ((1 2 3) (- - - - 3 -) 0)         ; d   age of connections: d~e=3
;;;      ((1 2 3) (- - - 3 - -) 0)         ; e
;;;      ((1 2 3) (- - - - - -) 0)    )    ; f   not connected



(load "../../../util/battery-scheme/list.scm")
(load "../../../util/battery-scheme/vector.scm")


(define *not-connected* -1)
(define *initial-connection-age* 0)



(define (print-neuron neuron)
  (format #t "w: ~a\t" (map (lambda (x) (format #f "~7,1f" x)) (get-neuron-weight neuron)))
  (format #t "a: ~a\t" (map (lambda (x) (if (< x *initial-connection-age*) "-" (format #f "~d" x))) (get-neuron-conn-age neuron)))
  (format #t "e: ~5,1f\n" (get-neuron-local-error neuron)))



(define (add-conn-ages old-conn-ages)
  (format #t "conn-ages:~a\n" old-conn-ages)
  (append old-conn-ages (list *not-connected*)))



(define (make-neuron dimension-sensor)
  (list
   (create-list-of-n-element-filled-by-evaluated-function dimension-sensor random:normal) ; weights
   (list *not-connected*) ; not connected, so conn-ages = -1
   0.0)) ; local-error
(define (get-neuron-weight neuron) (car neuron))
(define *index-neuron-weight* 0)
(define (get-neuron-conn-age neuron) (cadr neuron))
(define *index-neuron-conn-age* 1)
(define (get-neuron-local-error neuron) (caddr neuron))
(define *index-neuron-local-error* 2)



(define (add-neuron neuron gng)
  (define (iter igng size)
    (if (null? igng)
	'()
	(cons (list (get-neuron-weight (car igng)) (make-list size *not-connected*) (get-neuron-local-error neuron))
		    (iter (cdr igng) size))))

  (iter (append gng (list neuron)) (+ (length gng) 1)))



(define (find-and-del-unconnected-neuron gng)
  ;; delete-unconnected return: (growing neural gas with-last-element-deleted-list)
  ;; nonconsistent --- because need clean conn-age list in rest neuron
  (define (delete-unconnected counter deleted-list igng)
    (if (null? igng)
	(list deleted-list) ; significant: list reversed!
	(if (apply = (cons *not-connected* (get-neuron-conn-age (car igng))))
	    (delete-unconnected (1+ counter) (cons counter deleted-list) (cdr igng)) ; delete unconnected
	    (cons (car igng) (delete-unconnected (1+ counter) deleted-list (cdr igng)))))) ; connected

  (define (make-consistent-gng del-list igng)
    (if (null? igng)
	'()
	(cons (list (get-neuron-weight (car igng)) (remove-unexisted-conn-age del-list (get-neuron-conn-age (car igng))) (get-neuron-local-error (car igng)))
	      (make-consistent-gng del-list (cdr igng)))))

  (define (remove-unexisted-conn-age del-list conn-age)
    (if (null? del-list)
	conn-age
	(remove-unexisted-conn-age (cdr del-list) (append (list-head conn-age (car del-list)) (list-tail conn-age (1+ (car del-list)))))))

  (let ((gng-del (delete-unconnected 0 '() gng)))
    (let ((lengng (1- (length gng-del))))
      (make-consistent-gng (car (list-tail gng-del lengng)) (list-head gng-del lengng)))))


;; usage for update neuron weight vector: neuron number a=3 function=(+) step=10
;; (update-neuron-weight-vector 3 (lambda (step weights) (sum-sub-vectors + weights (mul-div-vector-const * (sum-sub-vectors - weights *example-sensor*) step))) 10 *example-gng*)
;; usage for update neuron weight: neuron number a=3 function=add(+) step=10 to each weight
;; simple example (update-neuron-weight 3 (lambda (step weights) (map (lambda (y) (+ y step)) weights)) 10 *example-gng*)
;; more simple example                         (format #t "~a\n" (map (lambda (y) (+ y    1)) (list 1 2 3 4 5)))
(define (update-neuron-weight-vector a function step gng)
  (list-set! (list-ref gng a) *index-neuron-weight*
	     (function (get-neuron-weight (list-ref gng a)) step))
  gng)



(define (update-neighbours-weights function list-neighbour eps-step gng)
  (define (iter neighbours counter igng)
    (if (null? neighbours)
	igng
	(iter (cdr neighbours) (1+ counter)
	      (if (>= (car neighbours) *initial-connection-age*)
		  (update-neuron-weight-vector counter function eps-step igng)
		  igng))))

  (iter list-neighbour 0 gng))



;; increase (+1) link between 2 and 3 elements
;; (update-neuron-conn-age 2 3 + 1 *initial-gng*)
;;
;; remove (*0, -1) link between 2 and 3 elements
;; (update-neuron-conn-age 2 3 * 0 *initial-gng*)
;; (update-neuron-conn-age 2 3 - 1 *initial-gng*)
(define (update-neuron-conn-age a b function step gng)
  (list-set! (get-neuron-conn-age (list-ref gng a)) b
	     (function (list-ref (get-neuron-conn-age (list-ref gng a)) b) step))
  (list-set! (get-neuron-conn-age (list-ref gng b)) a
	     (function (list-ref (get-neuron-conn-age (list-ref gng b)) a) step))
  gng)



;; increase by 1 neighbours connection age
(define (inc-neighbours-conn-age a gng)
  (define (iter neighbours counter igng)
    (if (null? neighbours)
	igng
	(iter (cdr neighbours) (1+ counter)
	      (if (>= (car neighbours) *initial-connection-age*)
		  (update-neuron-conn-age a counter + 1 igng)
		  igng))))

  (iter (get-neuron-conn-age (list-ref gng a)) 0 gng))


(define (remove-old-conn-age limit-conn-age gng)
  (if (null? gng)
      '()
      (cons (list (get-neuron-weight (car gng))
		  (map (lambda (x) (if (> x limit-conn-age) *not-connected* x)) (get-neuron-conn-age (car gng)))
		  (get-neuron-local-error (car gng)))
	    (remove-old-conn-age limit-conn-age (cdr gng)))))
      


;; usage for update neuron local-error: number a=3: function=add(+) step=10 to local-error
;; (update-neuron-local-error 3 + 10 *example-gng*)
(define (update-neuron-local-error a function step gng)
  (list-set! (list-ref gng a) *index-neuron-local-error*
	     (function (get-neuron-local-error (list-ref gng a)) step))
  gng)



(define (calculate-distance-weight-sensor sensor gng)
  (map (lambda (x) (euclidean-distance x sensor)) (map get-neuron-weight gng)))



(define (find-index-of-two-minimal in-list)
  (define (iter counter index-a val-a index-b val-b lst)
    (if (null? lst)
	(list index-a index-b)
	(if (< (car lst) val-a)
	    (iter (1+ counter) counter (car lst) index-a val-a (cdr lst)) ; B = A; set newA to min
	    (if (and (< (car lst) val-b) (not (= index-a counter)))
		(iter (1+ counter) index-a val-a counter (car lst) (cdr lst)) ; set B to min
		(iter (1+ counter) index-a val-a index-b val-b (cdr lst)))))) ; else

    (iter 0 0 (car in-list) 1 (cadr in-list) in-list))


;; fixme: algorithm numbers based on ../../../../doc/Neural_gas(ru).pdf
(define (growing-neural-gas sensor gng)
  (let ((distances-w-s (calculate-distance-weight-sensor sensor gng)))
    (format #t "distances-weight-sensor:\n~a\n\n" distances-w-s)
    (let ((winners (find-index-of-two-minimal distances-w-s))) ; algorithm:04
      (format #t "winners: ~a\n" winners)
      ;; danger! following code with small indent:

      ;; algorithm:10.b
      (find-and-del-unconnected-neuron

      ;; algorithm:10.a
      (remove-old-conn-age *limit-conn-age*

      ;; algorithm:09: set connection to 0 (*initial-connection-age*) between two winners
      (update-neuron-conn-age (car winners) (cadr winners) * *initial-connection-age*

      ;; algorithm:08
      (inc-neighbours-conn-age (car winners)

      ;; algorithm:07 for winner
      (update-neuron-weight-vector (car winners)
				   (lambda (weights step) (sum-sub-vectors + weights (mul-div-vector-const * (sum-sub-vectors - weights sensor) step)))
				   *eps-winner*

       ;; algorithm:07 for neighbours ; wrong formula: W=Wold*eps ; correct formula: W = Wold + eps*(Wols - Eold)
       (update-neighbours-weights (lambda (weights step) (sum-sub-vectors + weights (mul-div-vector-const * (sum-sub-vectors - weights sensor) step)))
				  (get-neuron-conn-age (list-ref gng (car winners)))
				  *eps-neighbour*

	;; algorithm:05
	(update-neuron-local-error (car winners) + (square (list-ref distances-w-s (car winners)))
									     gng))))))))))
