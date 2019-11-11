;;; Set dimensoin of sensor in format: (list mean standard-deviation) only for set dimension
(define dimension-sensor (list (list 0.0 0.0)   ; Ua, V
			       (list 0.0 0.0)   ; Ub, V
			       (list 0.0 0.0)   ; Uc, V
			       (list 0.0 0.0))) ; F, Hz

;;; Set number of output units
(define number-response 3)

;;; Calculate approximate value of number hidden
;;; Nhidden = (2/3)Nin + Nout
(define number-association (ceiling (+ (/ (* 2 (length dimension-sensor)) 3)  number-response)))

;;; Weight Sensor-Association
(define weight-sa (list (list -0.44820175519010724 -0.1644190964567136 -0.6235712905757486 -1.4860171976856185) (list -1.2067315985071732 1.7452035995265809 -1.0329541073047162 -1.1259726525481732) (list 0.31328285994194055 -1.0451852220438795 -1.3232826555123185 -0.8531369423960699) (list -2.4764545935364244 -0.26063631167124046 0.3368043071587377 -0.22806275455453223) (list -0.6704917341125843 0.2439618038701548 -1.1702429718307215 -1.7452757786957653) (list 0.23119501158063693 -0.9114778760412202 -1.8171492442405146 0.4378640551255611)))

;;; Threshold Association
(define threshold-a (list 0.2585004430048117 0.7707661037047685 0.10437850686178983 0.03068224068560792 0.6467715260725889 0.8402565350447063))

;;; Initial Weight Association-Response
;(define initial-weight-ar (list (list 0.5919656585796206 0.7330433683127151 0.9480856923832863 0.9948332033039629 0.6018611121679345 0.3923567680482496) (list -1.0910382928732878 -0.7156780767858174 0.3695369639591079 -1.0055631633560271 1.3204395977276833 -0.8078045255213353) (list 0.3143983562963834 -0.37319499835077846 -2.0802651296258676 -0.05130099748297936 -1.7358081716552298 1.7595068493926114)))

;;; Learned Weight Association-Response
(define learned-weight-ar (list (list 0.5919656585796206 149.7330433683127 0.9480856923832863 0.9948332033039629 0.6018611121679345 0.3923567680482496) (list -1.0910382928732878 187.28432192321418 0.3695369639591079 -1.0055631633560271 1.3204395977276833 -0.8078045255213353) (list 0.3143983562963834 71.62680500164922 -2.0802651296258676 -0.05130099748297936 -1.7358081716552298 1.7595068493926114)))

;;; Threshold Response
(define threshold-r (list 0.24551614536303734 0.10159152075337363 0.506708225757648))
