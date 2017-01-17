(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))
(define (cadar x) (car (cdr (car x))))

; Some utility functions that you may find useful to implement.
(define (map proc items)
  (if (null? items)
	'()
	(cons (proc (car items))
	(map proc (cdr items)))))

(define (cons-all first rests)
  (map (lambda (rest) (cons first rest)) rests))

(define (zip pairs)
	(cond
		((null? pairs) (list nil nil))
		(else
			(cons (map (lambda (x) (car x)) pairs) (cons (map (lambda (x) (car (cdr x))) pairs) nil)))))

;; Problem 18
;; Returns a list of two-element lists
(define (enumerate-helper s x)
	(if (null? s)
	nil
	(cons (list x (car s)) (enumerate-helper (cdr s) (+ 1 x)))))

(define (enumerate s)
  (enumerate-helper s 0))
  ; END Question 18

;; Problem 19
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
	(cond
		((null? denoms) nil)
		((< (- total (car denoms)) 0) (list-change total (cdr denoms)))
		((= (- total (car denoms)) 0) (cons (list (car denoms)) (list-change total (cdr denoms))))
		(else
			(append (cons-all (car denoms) (list-change (- total (car denoms)) denoms)) (list-change total (cdr denoms))))))
)


  ; END Question 19

;; Problem 20
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (analyze expr)
  (cond ((atom? expr)
		expr
         )
        ((quoted? expr)
        expr
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           (cons form (cons params (map analyze body)))
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           (cons (list (quote lambda) (car (zip values)) (analyze (car body))) (analyze (cadr (zip values))))
           ))
        (else
         (map analyze expr)
         ))))

;; Problem 21 (optional)
;; Draw the hax image using turtle graphics.
(define (hax d k)
  ; BEGIN Question 21
  'REPLACE-THIS-LINE
  )
  ; END Question 21
