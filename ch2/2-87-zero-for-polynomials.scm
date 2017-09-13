;;; Basic framework

(define (square x)
  (* x x))

(define (attach-tag type-tag contents)
  (if (and (eq? type-tag 'scheme-number) (number? contents))
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond
   ((number? datum) 'scheme-number)
   ((pair? datum) (car datum))
   (else
    (error "Invalid data with tag -- TYPE-TAG" datum))))

(define (contents datum)
  (cond
   ((number? datum) datum)
   ((pair? datum) (cdr datum))
   (else
    (error "Invalid data with tag -- CONTENTS" datum))))

;;; Type tower operations

(define *type-tower*
  '(scheme-number
    rational
    real
    (rectangular polar)
    complex))

(define (tower-position type-tag)
  (define (iter type-tower n)
    (if (null? type-tower)
        (error "Type not found in the tower")
        (let* ((current-type (car type-tower))
               (match
                (if (list? current-type)
                    (memq type-tag current-type)
                    (eq? type-tag current-type))))
          (if match
              n
              (iter (cdr type-tower) (+ n 1))))))
  (iter *type-tower* 0))

(define (common-supertype type-tags)
  (define (iter type-tags super-type super-type-position)
    (if (null? type-tags)
        super-type
        (let* ((current-type (car type-tags))
               (current-type-position (tower-position current-type)))
          (if (> current-type-position super-type-position)
              (iter (cdr type-tags) current-type current-type-position)
              (iter (cdr type-tags) super-type super-type-position)))))
  (iter type-tags '() -1))

(define (raise x)
  (cond ((number? x)
         (raise-scheme-number x))
         (else
          (let ((raise-proc (get-raise (type-tag x))))
            (if raise-proc
                (raise-proc x)
                x)))))

(define (drop x)
  (if (or (cons? x) (number? x))
      (let* ((project (get-project (type-tag x)))
            (dropped (if project (project x) #f)))
        (if (or (eq? dropped #f) (not (equ? (raise dropped) x)))
            x
            (drop dropped)))
      x))

(define (apply-generic op . args)
  (define (coerce arg type)
    (if (eq? (type-tag arg) type)
        arg
        (let ((raised (raise arg)))
          (if (not (eq? raised arg))
              (coerce (raise arg) type)
              (error "Cannot coerce one of the arguments to its supertype" arg)))))
  (define (prepare-call)
    (let*
        ((type-tags (map type-tag args))
         (proc (get op type-tags)))
      (if proc
          (cons proc (map contents args))
          (let*
              ((supertype (common-supertype type-tags))
               (coerced-args (map (lambda (arg) (coerce arg supertype)) args))
               (coerced-type-tags (map type-tag coerced-args))
               (proc (get op coerced-type-tags)))
            (if proc
                (cons proc (map contents coerced-args))
                  '())))))
  (let ((call (prepare-call)))
    (if (null? call)
        (error "No procedure with such name and signature found")
        (drop (apply (car call) (cdr call))))))

;;; Tables

(define *op-table* (make-weak-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) #f))

(define *coercion-table*
  (make-weak-hash))

(define (put-coercion src dst proc)
  (hash-set! *coercion-table* (cons src dst) proc))

(define (get-coercion src dst)
  (hash-ref *coercion-table* (cons src dst) #f))

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(define (complex->scheme-number z)
  (real-part z))

(define (complex->complex z) z)

(define (scheme-number->scheme-number n) n)

(put-coercion 'scheme-number 'complex scheme-number->complex)
(put-coercion 'complex 'scheme-number complex->scheme-number)
(put-coercion 'complex 'complex complex->complex)
(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)

(define *raise-table*
  (make-weak-hash))

(define (put-raise what proc)
  (hash-set! *raise-table* what proc))

(define (get-raise what)
  (hash-ref *raise-table* what #f))

(define (raise-scheme-number x)
  (make-rational x 1))

(define (raise-rational x)
  (make-real (/ (/ (numer x) (denom x)) 1.0)))

(define (raise-real x)
  (make-complex-from-real-imag x 0.0))

(put-raise 'rational raise-rational)
(put-raise 'real raise-real)

(define *project-table*
  (make-weak-hash))

(define (put-project what proc)
  (hash-set! *project-table* what proc))

(define (get-project what)
  (hash-ref *project-table* what #f))

(define (project-complex x)
  (real-part x))

(define (project-rational x)
  (numer x))

(define (project-real x)
  (round (contents x)))

(put-project 'complex project-complex)
(put-project 'rational project-rational)
(put-project 'real project-real)

;;; Packages

(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  (define (=zero? x)
    (= (numer x) 0))
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational) equ?)
  (put '=zero? '(rational) =zero?)
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  (define (equ? z1 z2)
    (and (apply-generic 'equ? (real-part z1) (real-part z2))
         (apply-generic 'equ? (imag-part z1) (imag-part z2))))
  (define (=zero? z)
    (and (equ? (real-part z) 0)
         (equ? (imag-part z) 0)))
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z2 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z2 z2))))
  (put 'equ? '(complex complex) equ?)
  (put '=zero? '(complex) =zero?)
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

(define (install-real-package)
  (define (tag x) (attach-tag 'real x))
  (define (add r1 r2)
    (+ r1 r2))
  (define (sub r1 r2)
    (- r1 r2))
  (define (mul r1 r2)
    (* r1 r2))
  (define (div r1 r2)
    (/ r1 r2))
  (define (equ? r1 r2)
    (= r1 r2))
  (put 'equ? '(real real) equ?)
  (put 'add '(real real) (lambda (r1 r2) (tag (add r1 r2))))
  (put 'sub '(real real) (lambda (r1 r2) (tag (sub r1 r2))))
  (put 'mul '(real real) (lambda (r1 r2) (tag (mul r1 r2))))
  (put 'div '(real real) (lambda (r1 r2) (tag (div r1 r2))))
  (put 'make 'real (lambda (x) (attach-tag 'real x))))

(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p)
    (car p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (term-list p)
    (cdr p))
  (define (adjoin-term term term-list)
    (if (apply-generic '=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (add-terms terms1 terms2)
    (cond ((empty-termlist? terms1) terms2)
          ((empty-termlist? terms2) terms1)
          (else
           (let ((t1 (first-term terms1)) (t2 (first-term terms2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms terms1) terms2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms terms1 (rest-terms terms2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms terms1)
                                (rest-terms terms2)))))))))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polynomials contain different variables -- ADD-POLY" (list p1 p2))))
  (define (mul-terms terms1 terms2)
    (define (mul-term-by-all-terms term terms)
      (if (empty-termlist? terms)
          (the-empty-termlist)
          (let ((term2 (first-term terms)))
            (adjoin-term
             (make-term (+ (order term) (order term2))
                        (mul (coeff term) (coeff term2)))
             (mul-term-by-all-terms term (rest-terms terms))))))
    (if (empty-termlist? terms1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term terms1) terms2)
                   (mul-terms (rest-terms terms1) terms2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polynomials contain different variables -- MUL-POLY" (list p1 p2))))
  (define (=zero? poly)
    (define (iter terms)
      (if (null? terms)
          #t
          (if (apply-generic '=zero? (coeff (car terms)))
              (iter (rest-terms terms))
              #f)))
    (iter (term-list poly)))
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put '=zero? '(polynomial) (lambda (p) (=zero? p)))
  (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms))))
  'done)

;;; Public interfaces

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (numer x) (car (contents x)))

(define (denom x) (cdr (contents x)))

(define (make-real x)
  ((get 'make 'real) x))

(define (real-part z)
  (apply-generic 'real-part z))

(define (imag-part z)
  (apply-generic 'imag-part z))

(define (magnitude z)
  (apply-generic 'magnitude z))

(define (angle z)
  (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (add x y)
  (apply-generic 'add x y))

(define (sub x y)
  (apply-generic 'sub x y))

(define (mul x y)
  (apply-generic 'mul x y))

(define (div x y)
  (apply-generic 'div x y))

(define (exp x y)
  (apply-generic 'exp x y))

(define (equ? x y)
  (apply-generic 'equ? x y))

(define (=zero? x)
  (apply-generic '=zero? x))

;;; Setup the environment

(install-polar-package)
(install-rectangular-package)
(install-rational-package)
(install-real-package)
(install-scheme-number-package)
(install-complex-package)
(install-polynomial-package)
