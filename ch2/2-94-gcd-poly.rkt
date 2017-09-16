;;; Basic framework

(define (square x)
  (* x x))

(define (attach-tag type-tag contents)
  (if (and (eq? type-tag 'scheme-number) (number? contents))
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond
   ((symbol? datum) 'symbol)
   ((number? datum) 'scheme-number)
   ((pair? datum) (car datum))
   (else
    (error "Invalid data with tag -- TYPE-TAG" datum))))

(define (contents datum)
  (cond
   ((symbol? datum) datum)
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
        (error "Type not found in the tower" type-tag)
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
  (if (or (polynomial? x) (and (rat? x) (polynomial? (numer x))))
      x
      (if (or (cons? x) (number? x))
          (let* ((project (get-project (type-tag x)))
                 (dropped (if project (project x) #f)))
            (if (or (eq? dropped #f) (not (equ? (raise dropped) x)))
                x
                (drop dropped)))
          x)))

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
  (make-real (div (div (numer x) (denom x)) 1.0)))

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
  (define (negate z)
    (make-from-real-imag
     (apply-generic 'negate (real-part z))
     (apply-generic 'negate (imag-part z))))
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'negate '(rectangular) (lambda (z) (tag (negate z))))
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
  (define (negate z)
    (make-from-mag-ang (magnitude z) (remainder (- (angle z) pi) (* 2 pi))))
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'negate '(polar) (lambda (z) (negate z)))
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
  (put 'negate '(scheme-number)
       (lambda (x) (- x)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (cons n d))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  (define (negate r)
    (make-rat (negate (numer r)) (denom r)))
  (define (equ? x y)
    (and (equ? (numer x) (numer y))
         (equ? (denom x) (denom y))))
  (define (=zero? x)
    (=zero? (numer x)))
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'negate '(rational) negate)
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
  (put 'negate '(complex)
       (lambda (z) (tag (negate z))))
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
  (define (negate r)
    (- r))
  (define (=zero? r)
    (= r 0))
  (put 'add '(real real) (lambda (r1 r2) (tag (add r1 r2))))
  (put 'sub '(real real) (lambda (r1 r2) (tag (sub r1 r2))))
  (put 'mul '(real real) (lambda (r1 r2) (tag (mul r1 r2))))
  (put 'div '(real real) (lambda (r1 r2) (tag (div r1 r2))))
  (put 'negate '(real) negate)
  (put 'equ? '(real real) equ?)
  (put 'zero? '(real) zero?)
  (put 'make 'real (lambda (x) (attach-tag 'real x))))

(define (coeff term)
  (car term))

(define (order term)
  (cadr term))

(define (make-term coeff order)
  (list coeff order))

(define (install-sparse-terms-package)
  (define (empty-termlist? term-list)
    (null? term-list))
  (define (first-term term-list)
    (car term-list))
  (define (rest-terms term-list)
    (cdr term-list))
  (define (adjoin-term term-list term)
    (if (apply-generic '=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (tag p)
    (attach-tag 'sparse-terms p))
  (put 'empty-termlist? '(sparse-terms) (lambda (terms) (empty-termlist? terms)))
  (put 'first-term '(sparse-terms) (lambda (terms) (first-term terms)))
  (put 'rest-terms '(sparse-terms) (lambda (terms) (tag (rest-terms terms))))
  (put 'adjoin-term '(sparse-terms)
       (lambda (terms term)
         (tag (adjoin-term terms term))))
  (put 'make 'sparse-terms (lambda (terms) (tag terms))))

(define (install-dense-terms-package)
  (define (term-order term)
    (- (length term) 1))
  (define (term-coeff term)
    (car term))
  (define (empty-termlist? term-list)
    (null? term-list))
  (define (first-term term-list)
    (make-term (car term-list) (term-order term-list)))
  (define (rest-terms term-list)
    (cdr term-list))
  (define (adjoin-term term-list term)
    (define (n-zeros n)
      (if (<= n 0)
          '()
          (cons 0 (n-zeros (- n 1)))))
    (let ((next-term-order (+ (term-order term-list) 1)))
      (cond ((= (order term) next-term-order)
             (cons (coeff term) term-list))
            ((< (order term) next-term-order)
             (cons (car term-list) (adjoin-term (cdr term-list) term)))
            ((> (order term) next-term-order)
             (append (list (coeff term))
                     (n-zeros (- (order term) (term-order term-list) 1))
                     term-list)))))
  (define (tag p)
    (attach-tag 'dense-terms p))
  (put 'empty-termlist? '(dense-terms) (lambda (terms) (empty-termlist? terms)))
  (put 'first-term '(dense-terms) (lambda (terms) (first-term terms)))
  (put 'rest-terms '(dense-terms) (lambda (terms) (tag (rest-terms terms))))
  (put 'adjoin-term '(dense-terms)
       (lambda (terms term)
         (tag (adjoin-term terms term))))
  (put 'make 'dense-terms (lambda (terms) (tag terms))))

(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p)
    (car p))
  (define (term-list p)
    (cdr p))
  (define (variable? x)
    (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (empty-termlist? terms)
    (apply-generic 'empty-termlist? terms))
  (define (the-empty-termlist)
    (make-sparse-terms (list)))
  (define (first-term terms)
    (apply-generic 'first-term terms))
  (define (rest-terms terms)
    (apply-generic 'rest-terms terms))
  (define (adjoin-term terms term)
    ((get 'adjoin-term (list (type-tag terms))) (contents terms) term))
  (define (terms-each terms fn)
    (let ((term (first-term terms)))
      (fn term)
      (terms-each (rest-terms terms) fn)))
  (define (terms-map terms fn)
    (define (iter terms result)
      (if (empty-termlist? terms)
          result
          (iter (rest-terms terms) (adjoin-term result (fn (first-term terms))))))
    (iter terms (make-sparse-terms (list))))
  (define (terms-accumulate terms mapfn combinator)
    (define (iter terms result)
      (if (empty-termlist? terms)
          result
          (iter (rest-terms terms) (combinator result (mapfn (first-term terms))))))
    (iter (rest-terms terms) (mapfn (first-term terms))))
  (define (reorder-poly poly old-var new-var)
    (define (adjust-coeffs poly multiplier)
      (define (adjust-coeff term)
        (let ((poly-term
               (if (polynomial? term)
                   term
                   (make-polynomial old-var (make-sparse-terms (list (list (coeff term) 0)))))))
          (make-term (mul poly-term multiplier) (order term))))
      (make-polynomial new-var (terms-map (term-list poly) adjust-coeff)))
    (define (convert-term term)
      (let ((term-coeff (coeff term))
            (term-order (order term)))
        (if (polynomial? term-coeff)
            (let*
                ((coeff-poly (contents term-coeff))
                 (reordered-poly
                  (if (eq? (variable coeff-poly) new-var)
                      coeff-poly
                      (reorder coeff-poly new-var))))
              (adjust-coeffs
               reordered-poly
               (make-polynomial old-var
                                (make-sparse-terms (list (list 1 term-order))))))
            (let ((free-expr-coeff
                   (make-polynomial old-var (make-sparse-terms (list (list term-coeff term-order))))))
              (make-polynomial new-var (make-sparse-terms (list (list free-expr-coeff 0))))))))
      (contents
       (terms-accumulate
        (term-list poly)
        convert-term
        (lambda (a b)
          (tag (add-poly (contents a) (contents b)))))))
  (define (add-terms terms1 terms2)
    (cond ((empty-termlist? terms1) terms2)
          ((empty-termlist? terms2) terms1)
          (else
           (let ((term1 (first-term terms1))
                 (term2 (first-term terms2)))
             (cond ((> (order term1) (order term2))
                    (adjoin-term
                     (add-terms (rest-terms terms1) terms2)
                     term1))
                   ((< (order term1) (order term2))
                    (adjoin-term
                     (add-terms terms1 (rest-terms terms2))
                     term2))
                   (else
                    (adjoin-term
                     (add-terms (rest-terms terms1)
                                (rest-terms terms2))
                     (make-term (add (coeff term1) (coeff term2)) (order term1)))))))))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (add-poly p1
                  (contents (reorder (tag p2) (variable p1))))))
  (define (negate-terms terms)
    (if (empty-termlist? terms)
        terms
        (let* ((term (first-term terms))
               (negated (apply-generic 'negate (coeff term))))
          (adjoin-term (negate-terms (rest-terms terms)) (make-term negated (order term))))))
  (define (negate poly)
    (make-poly (variable poly) (negate-terms (term-list poly))))
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (add-poly p1 (negate p2))
        (error "Polynomials contain different variables -- SUB-POLY" (list p1 p2))))
  (define (mul-terms terms1 terms2)
    (define (mul-term-by-all-terms term terms)
      (if (empty-termlist? terms)
          terms
          (let ((term2 (first-term terms)))
            (adjoin-term
             (mul-term-by-all-terms term (rest-terms terms))
             (make-term (mul (coeff term) (coeff term2)) (+ (order term) (order term2)))))))
    (if (empty-termlist? terms1)
        terms1
        (add-terms (mul-term-by-all-terms (first-term terms1) terms2)
                   (mul-terms (rest-terms terms1) terms2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polynomials contain different variables -- MUL-POLY" (list p1 p2))))
  (define (div-terms terms1 terms2)
    (if (empty-termlist? terms1)
        (cons (the-empty-termlist) (the-empty-termlist))
        (let ((term1 (first-term terms1))
              (term2 (first-term terms2)))
          (if (> (order term2) (order term1))
              (cons (the-empty-termlist) terms1)
              (let ((new-coeff (div (coeff term1) (coeff term2)))
                    (new-order (- (order term1) (order term2))))
                (let*
                    ((rest-of-result
                      (div-terms (rest-terms terms1) terms2))
                     (division-result
                      (adjoin-term (car rest-of-result) (make-term new-coeff new-order))))
                  (cons
                   division-result
                   (add-terms terms1 (negate-terms (mul-terms division-result terms2))))))))))
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((division-result (div-terms (term-list p1) (term-list p2))))
          (list
           (make-poly (variable p1) (car division-result))
           (make-poly (variable p1) (cdr division-result))))
        (error "Polynomials contain different variables -- DIV-POLY" (list p1 p2))))
  (define (remainder-terms terms1 terms2)
    (cdr (div-terms terms1 terms2)))
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        a
        (gcd-terms b (remainder-terms a b))))
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1) (gcd-terms (term-list p1) (term-list p2)))
        (error "Polynomials contain different variables -- GCD-POLY" (list p1 p2))))
  (define (tag p)
    (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'negate '(polynomial) (lambda (p) (tag (negate p))))
  (put 'sub '(polynomial polynomial) (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial) (lambda (p1 p2) (map tag (div-poly p1 p2))))
  (put 'greatest-common-divisor '(polynomial polynomial) (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  (put 'reorder '(polynomial symbol) (lambda (poly new-var) (tag (reorder-poly poly (variable poly) new-var))))
  (put '=zero? '(polynomial) (lambda (poly) (empty-termlist? (term-list poly))))
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

(define (make-sparse-terms terms)
  ((get 'make 'sparse-terms) terms))

(define (make-dense-terms terms)
  ((get 'make 'dense-terms) terms))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (reorder poly var)
  (apply-generic 'reorder poly var))

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

(define (greatest-common-divisor x y)
  (apply-generic 'greatest-common-divisor x y))

(define (negate x)
  (apply-generic 'negate x))

(define (rat? x)
  (and (list? x) (eq? (type-tag x) 'rational)))

(define (polynomial? x)
  (and (list? x) (eq? (type-tag x) 'polynomial)))

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
(install-sparse-terms-package)
(install-dense-terms-package)
(install-polynomial-package)

(define p1 (make-polynomial 'x (make-sparse-terms '((1 4) (-1 3) (-2 2) (2 1)))))
(define p2 (make-polynomial 'x (make-sparse-terms '((1 3) (-1 1)))))
