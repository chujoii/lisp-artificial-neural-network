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



;;; Keywords: algorithm neuron network machine learning growing neural gas



;;; Usage:
;;
;; algorithm numbers based on ../doc/growing-neural-gas/ru/algorithm.pdf
;;
;; see unit-testing and examples in
;; example/growing-neural-gas



;;; History:

;; Project started at 2019-08(Aug)-28



;;; Code:

;;; neuron (node):
;;; ((weights) (conn-ages) local-error),  where:
;;;    (weights) - list of weights for input vector from sensors
;;;    (conn-ages) - list of connection-age between neurons
;;;    local-error - number = old-local-error + (d(Weights, Sensors))^2
;;;    utility-factor - number
;;;
;;; gng = (a-b-c   d-e   f)
;;;
;;;  connection-age == -1 not connected ("nc" or "-" in next table)
;;;  connection-age >= 0 connected
;;;
;;;      ((wieght) ( conn-age ) local-error utility-factor)
;;;                a b c d e f  ; warning: duplication of conn-age data => duplication of program code
;;; (    ((1 2 3) (- 0 2 - - -) 0           0)         ; a   age of connections: a~b=0
;;;      ((1 2 3) (0 - 1 - - -) 0           0)         ; b   age of connections: b~c=1
;;;      ((1 2 3) (2 1 - - - -) 0           0)         ; c   age of connections: c~a=2
;;;      ((1 2 3) (- - - - 3 -) 0           0)         ; d   age of connections: d~e=3
;;;      ((1 2 3) (- - - 3 - -) 0           0)         ; e
;;;      ((1 2 3) (- - - - - -) 0           0)    )    ; f   not connected



(load "../../battery-scheme/list.scm")
(load "../../battery-scheme/vector.scm")
(load "../../battery-scheme/minimax.scm")

(define *not-connected* -1)
(define *initial-connection-age* 0)

(define *winners* (list 0 1)) ; fixme: global variable



;; construct neuron from ready data set
(define (construct-neuron weight conn-age local-error utility-factor)
  (list weight conn-age local-error utility-factor))

;; make empty (initial) neuron
;; dimension-gng == current size of growing neural gas
(define (make-neuron dimension-sensor dimension-gng)
  (construct-neuron
   (create-list-of-n-element-filled-by-evaluated-function dimension-sensor random:normal) ; weights
   (make-list dimension-gng *not-connected*) ; not connected, so conn-ages = -1
   0.0   ; local-error
   0.0)) ; utility-factor

(define (get-neuron-weight neuron) (car neuron))
(define *index-neuron-weight* 0)
(define (get-neuron-conn-age neuron) (cadr neuron))
(define *index-neuron-conn-age* 1)
(define (get-neuron-local-error neuron) (caddr neuron))
(define *index-neuron-local-error* 2)
(define (get-neuron-utility-factor neuron) (cadddr neuron))
(define *index-neuron-utility-factor* 3)

(define (print-neuron neuron)
  (format #t "w: ~a\t" (map (lambda (x) (format #f "~7,1f" x)) (get-neuron-weight neuron)))
  (format #t "a: ~a\t" (map (lambda (x) (if (< x *initial-connection-age*) "-" (format #f "~d" x))) (get-neuron-conn-age neuron)))
  (format #t "e: ~5,1f\t" (get-neuron-local-error neuron))
  (format #t "u: ~5,1f\n" (get-neuron-utility-factor neuron)))

;; generate string with start "list"
;; usage: (print-gng-as-list "~d" (list 1 2 (list 3 4 5)))
;; result: "(list 1 2 (list 3 4 5))"
(define (print-gng-as-list gng)
  (define (iter ls)
    (if (null? ls)
	""
	(string-append "(list "
		       (format #f "\t(list ~a)\n" (string-join (map (lambda (x) (format #f "~g" x)) (get-neuron-weight (car ls))) " "))
		       (format #f "\t(list ~a)\n" (string-join (map (lambda (x) (format #f "~d" x)) (get-neuron-conn-age (car ls))) " "))
		       (format #f "\t~g" (get-neuron-local-error (car ls)))
		       (format #f "\t~g" (get-neuron-utility-factor (car ls)))
		       ")\n\n"
		       (iter (cdr ls)))))

  (string-append "(list\n" (iter gng) ")"))


(define (add-neuron neuron gng)
  (define (add-column-with-conn-age igng)
    (if (null? igng)
	'()
	(cons (construct-neuron (get-neuron-weight (car igng)) (append (get-neuron-conn-age (car igng)) (list *not-connected*)) (get-neuron-local-error (car igng)) (get-neuron-utility-factor (car igng)))
	      (add-column-with-conn-age (cdr igng)))))

  (add-column-with-conn-age (append gng (list neuron))))



(define (find-and-del-neuron-with-min-utility-factor k gng)
  ;; delete-neuron-U-min (min-utility) return: (growing neural gas with-list-element-deleted-list)
  ;; nonconsistent --- because need clean conn-age list in rest neuron
  (define (delete-neuron-U-min counter deleted-list E-max minimum-size-of-gng igng)
    (if (null? igng)
	(list deleted-list) ; significant: list reversed!
	(if (and (> minimum-size-of-gng 0)
		 (> (/ E-max (get-neuron-utility-factor (car igng))) k))
	    (begin (format #t "d(~d)" counter)
		   (delete-neuron-U-min (1+ counter) (cons counter deleted-list) E-max (1- minimum-size-of-gng) (cdr igng))) ; delete
	    (cons (car igng) (delete-neuron-U-min (1+ counter) deleted-list E-max minimum-size-of-gng (cdr igng)))))) ; leave

  (define (make-consistent-gng del-list igng)
    (if (null? igng)
	'()
	(cons (construct-neuron (get-neuron-weight (car igng)) (remove-unexisted-conn-age del-list (get-neuron-conn-age (car igng))) (get-neuron-local-error (car igng)) (get-neuron-utility-factor (car igng)))
	      (make-consistent-gng del-list (cdr igng)))))

  (define (remove-unexisted-conn-age del-list conn-age)
    (if (null? del-list)
	conn-age
	(remove-unexisted-conn-age (cdr del-list) (append (list-head conn-age (car del-list)) (list-tail conn-age (1+ (car del-list)))))))

  ;; if set very aggressive coefficients, then network delete all
  ;; neurons and leave two (maybe unconnected) so need reconnect them
  (define (reconnect igng)
    (if (<= (length igng) 2)
	(update-neuron-conn-age 0 1 + 1 igng))
    igng)


  (let ((E-max (cdr (extremum (map get-neuron-local-error gng) >)))
	(minimum-size-of-gng (- (length gng) 2))) ;; leave 2 neuron at least
    ;; in original algorithm remove only one neuron with min utility:
    ;; (U-min (extremum (map get-neuron-local-error gng) <)))
    (let ((gng-del (delete-neuron-U-min 0 '() E-max minimum-size-of-gng gng)))
      (let ((lengng (1- (length gng-del))))
	(reconnect (make-consistent-gng (car (list-tail gng-del lengng)) (list-head gng-del lengng)))))))



;; usage for update neuron weight vector: neuron number a=3 function=(+) step=10
;; (update-neuron-weight-vector 3 (lambda (weights step) (sum-sub-vectors + weights (mul-div-vector-const * (sum-sub-vectors - weights *example-sensor*) step))) 10 *example-gng*)
;; usage for update neuron weight: neuron number a=3 function=add(+) step=10 to each weight
;; simple example (update-neuron-weight 3 (lambda (weights step) (map (lambda (y) (+ y step)) weights)) 10 *example-gng*)
;; more simple example                         (format #t "~a\n" (map (lambda (y) (+ y    1)) (list 1 2 3 4 5)))
;;
;; "function" get two argumets: list-of-weight and step
(define (update-neuron-weight-vector a function step gng)
  (list-set! (list-ref gng a) *index-neuron-weight*
	     (function (get-neuron-weight (list-ref gng a)) step))
  gng)


;; "function" get two argumets: list-of-weight and step
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
;;
;; set link between 2 and 3 elements to number (for example 7)
;; (update-neuron-conn-age 2 3 (lambda (ignored-value val) val) 7 *initial-gng*)
;;
;; "function" get two argumets: number-conn-age and step
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
      (cons (construct-neuron (get-neuron-weight (car gng))
			      (map (lambda (x) (if (> x limit-conn-age) *not-connected* x)) (get-neuron-conn-age (car gng)))
			      (get-neuron-local-error (car gng))
			      (get-neuron-utility-factor (car gng)))
	    (remove-old-conn-age limit-conn-age (cdr gng)))))
      


;; usage for update neuron local-error: number a=3: function=add(+) step=10 to local-error
;; (update-neuron-local-error 3 + 10 *example-gng*)
;;
;; "function" get two argumets: number-locat-error and step
(define (update-neuron-local-error a function step gng)
  (list-set! (list-ref gng a) *index-neuron-local-error*
	     (function (get-neuron-local-error (list-ref gng a)) step))
  gng)



(define (update-neuron-utility-factor a function step gng)
  (list-set! (list-ref gng a) *index-neuron-utility-factor*
	     (function (get-neuron-utility-factor (list-ref gng a)) step))
  gng)



;; Decrease local-errors and utility-factor for all neuron
(define (decrease-all-neuron-local-errors-and-utility-factor factor-beta gng)
  (map (lambda (neuron) (construct-neuron (get-neuron-weight neuron)
					  (get-neuron-conn-age neuron)
					  (* (get-neuron-local-error neuron) factor-beta)
					  (* (get-neuron-utility-factor neuron) factor-beta)))
       gng))



(define (calculate-distance-weight-sensor sensor gng)
  (map (lambda (x) (euclidean-distance-vector x sensor)) (map get-neuron-weight gng)))


(define (calculate-distance-in-mixed-space-weight-sensor functions-mixed-space sensor gng)
  ;; because: euclidean-distance not good for cyclic data (angles, ...)
  ;; functions-mixed-space == (list euclidean-distance euclidean-distance angle-distance angle-distance ...)
  ;; dimension of functions-mixed-space --- equivalent to dimension of sensor
  (map (lambda (x) (sqrt (apply + (map (lambda (a b c) (a b c)) functions-mixed-space x sensor)))) (map get-neuron-weight gng)))


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



;; Find neuron index with max local-error
(define (find-neuron-index-with-max-local-error gng)
  (car (extremum (map get-neuron-local-error gng) >))) ;; get index for extremum (max)



;; Find (for selected neuron) neighbours index with max local-error
(define (find-neighbours-index-with-max-local-error index-max-local-error gng)
  (define (iter counter index-a val-a neighbours)
    (if (null? neighbours)
	index-a
	(if (and (>= (car neighbours) *initial-connection-age*)
		 (or (< index-a 0)           (> (get-neuron-local-error (list-ref gng counter)) val-a)))
	    (iter (1+ counter) counter (get-neuron-local-error (list-ref gng counter)) (cdr neighbours))
	    (iter (1+ counter) index-a val-a (cdr neighbours)))))

  (iter 0 -1 0.0 (get-neuron-conn-age (list-ref gng index-max-local-error))))



(define (adaptive-step-create-new-neuron gng)
  (let ((index-neuron-max-local-error (find-neuron-index-with-max-local-error gng))) ;; algorithm:13

    (let ((tmp-index-neighbour-for-max-local-error (find-neighbours-index-with-max-local-error index-neuron-max-local-error gng)) ;; algorithm:14
	  (index-of-new-neuron (length gng)) ; count from 0
	  (igng (add-neuron (make-neuron *dimension-of-sensor* (length gng)) gng)))  ; algorithm:15.a

      (let ((index-neighbour-for-max-local-error (if (>= tmp-index-neighbour-for-max-local-error 0) ;; fix situation when all index = -1
						     tmp-index-neighbour-for-max-local-error        ;; more correct solution: use not aggressive coefficients (k-utility)
						     (if (= index-neuron-max-local-error 0) 1 0))))

      (let ((conn-age-uv (list-ref (get-neuron-conn-age (list-ref gng index-neuron-max-local-error)) index-neighbour-for-max-local-error))
	    (local-error-u (get-neuron-local-error (list-ref gng index-neuron-max-local-error)))
	    (local-error-v (get-neuron-local-error (list-ref gng index-neighbour-for-max-local-error))))

	;; algorithm:18
	(update-neuron-local-error index-of-new-neuron (lambda (ignored-value val) val) (* (get-neuron-local-error (list-ref gng index-neuron-max-local-error)) *eps-local-error*)
	(update-neuron-local-error index-neighbour-for-max-local-error * *eps-local-error*
	(update-neuron-local-error index-neuron-max-local-error * *eps-local-error*

        ;; algorithm:17
        (update-neuron-utility-factor index-of-new-neuron
				      (lambda (utility step)
					(/ (+ (get-neuron-utility-factor (list-ref igng index-neuron-max-local-error)) (get-neuron-utility-factor (list-ref igng index-neighbour-for-max-local-error)))
					 2))
				      0 ; use "0" and ignored "utility" and "step" --- because previous value are worthless


	;; algorithm:16      instead of "lambda" maybe more correct:      ... + (+ conn-age-uv *initial-connection-age*)
	(update-neuron-conn-age index-neuron-max-local-error index-of-new-neuron                 (lambda (ignored-value val) val) conn-age-uv
	(update-neuron-conn-age index-of-new-neuron          index-neighbour-for-max-local-error (lambda (ignored-value val) val) conn-age-uv
	(update-neuron-conn-age index-neuron-max-local-error index-neighbour-for-max-local-error (lambda (ignored-value val) val) *not-connected*

      ;; algorithm:15.b
      (update-neuron-weight-vector index-of-new-neuron
				   (lambda (weights step)
				     (mul-div-vector-const
				      /
				      (sum-sub-vectors + (get-neuron-weight (list-ref igng index-neuron-max-local-error)) (get-neuron-weight (list-ref igng index-neighbour-for-max-local-error)))
				      2))
				   0 ; use "0" and ignored "weights" and "step" --- because previous value are worthless (random)
				   igng)))))))))))))



(define (growing-neural-gas epoch sensor gng)
  (let ((distances-w-s (if (null? *functions-mixed-space*)
			   (calculate-distance-weight-sensor sensor gng)
			   (calculate-distance-in-mixed-space-weight-sensor *functions-mixed-space* sensor gng))))
	(if *debug-print* (format #t "distances-weight-sensor:\n~a\n\n" distances-w-s))

    (let ((winners (find-index-of-two-minimal distances-w-s))) ; algorithm:04
      (if *debug-print* (format #t "winners: ~a\n" winners))
      (set! *winners* winners)
      ;; danger! following code with small indent:

      ;; algorithm:20,21
      (decrease-all-neuron-local-errors-and-utility-factor *factor-beta-decrease-local-error*

      ;; algorithm:11.b
      (find-and-del-neuron-with-min-utility-factor *k-utility*

      ;; algorithm:11.a
      (remove-old-conn-age *limit-conn-age*

      ;; algorithm:10: set connection to 0 (*initial-connection-age*) between two winners
      (update-neuron-conn-age (car winners) (cadr winners) * *initial-connection-age*

      ;; algorithm:09
      (inc-neighbours-conn-age (car winners)

      ;; algorithm:08
      (update-neuron-utility-factor (car winners) + (-
						     (square (list-ref distances-w-s (cadr winners))) ; second winner
						     (square (list-ref distances-w-s (car winners)))) ; first winner

      ;; algorithm:07 for winner
      (update-neuron-weight-vector (car winners)
				   (lambda (weights step) (sum-sub-vectors + weights (mul-div-vector-const * (sum-sub-vectors - sensor weights) step)))
				   *eps-winner*

       ;; algorithm:07 for neighbours ; wrong formula: W=Wold*eps ; correct formula: W = Wold + eps*(Wols - Eold)
       (update-neighbours-weights (lambda (weights step) (sum-sub-vectors + weights (mul-div-vector-const * (sum-sub-vectors - sensor weights) step)))
				  (get-neuron-conn-age (list-ref gng (car winners)))
				  *eps-neighbour*

	;; algorithm:05
	(update-neuron-local-error (car winners) + (square (list-ref distances-w-s (car winners)))

	 ;; algorithm:12
	 (if (and (= 0 (remainder epoch *lambda-step*)) ; adaptation step: create new neuron each lambda-step
		  (> *limit-network-size* (length gng)))
	     (adaptive-step-create-new-neuron gng)
	     gng)))))))))))))



(define (extract-groups-from-conn-ages conn-ages)
  (define (remove-nc counter lst)
    (if (null? lst)
	'()
	(if (= *not-connected* (car lst))
	    (remove-nc (1+ counter) (cdr lst))
	    (cons counter (remove-nc (1+ counter) (cdr lst))))))

  ;; generate triangle matrix of connections with first element = number in array
  (define (add-number-remove-double counter lst)
    (if (null? lst)
	'()
	(cons (cons counter (remove-nc (1+ counter) (list-tail (car lst) (1+ counter))))
	      (add-number-remove-double (1+ counter) (cdr lst)))))

  ;; return -1 if element not find
  ;; return index if find
  (define (index-of-element-in-list element in-list)
    (define (iterq counter lst)
      (if (null? lst)
	  -1
	  (if (= element (car lst))
	      counter
	      (iterq (+ 1 counter) (cdr lst)))))

    (iterq 0 in-list))

  ;; return #f if (car x) not part of list "y"
  ;; return merged list if is part of list:
  ;; x = (list 3 4 5)
  ;; y = (list 7 6 3 2 1)
  ;; "try-merge" search only "3" and return (list 7 6   3 4 5   2 1)
  (define (try-merge x y)
    (format #t "\ngrouping [~a] [~a]" x y)
    (let ((index (index-of-element-in-list (car x) y))) ;; see memv
      (format #t " ~d" index)
      (if (< index 0)
	  #f
	  (append y (cdr x)))))

  (let ((triangle-array (add-number-remove-double 0 conn-ages)))
    ;(format #t "<~a>" (index-of-element-in-list 1 (car triangle-array)))
    ;(format #t "<~a>" (index-of-element-in-list 7 (car triangle-array)))
    (format #t "{~a}" (try-merge (cadr triangle-array) (car triangle-array)))
    (format #t "{~a}" (try-merge (cadddr triangle-array) (car triangle-array)))
    triangle-array))
