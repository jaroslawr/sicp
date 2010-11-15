(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "Unknown type of expression - DERIV"))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (sum? x)
  (and
   (list? x)
   (memq '+ x)))

(define (addend s)
  (define (iter exp result)
    (if (eq? (car exp) '+)
        result
        (iter (cdr exp) (append result (list (car exp))))))
  (let ((rest (iter s '())))
    (if (null? (cdr rest))
        (car rest)
        rest)))

(define (augend s)
  (let ((augend (cdr (memq '+ s))))
    (if (null? (cdr augend))
        (car augend)
        augend)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (product? x)
  (and (list? x)
       (not (null? (memq '* x)))
       (not (memq '+ x))))

(define (multiplier p)
  (define (iter exp result)
    (if (eq? (car exp) '*)
        result
        (iter (cdr exp) (append result (list (car exp))))))
  (let ((rest (iter p '())))
    (if (null? (cdr rest))
        (car rest)
        rest)))

(define (multiplicand p)
  (let ((multiplicand (cdr (memq '* p))))
    (if (null? (cdr multiplicand))
        (car multiplicand)
        multiplicand)))
