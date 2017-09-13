(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
   (pairs (stream-cdr s) (stream-cdr t))))

(define integer-pairs (pairs integers integers))

;; Such a definition of pairs leads to an infinite recursion - when
;; defining the integer-pairs, the pairs procedure attempts to
;; evaluate the arguments to interleave, one of which is itself a call
;; to pairs. In this way, each row is a correct, delayed infinite
;; stream, but the procedure is attempting to compute the whole
;; inifite stream of rows at once. In the previous, correct
;; definition, the interleave call is also delayed, making the stream
;; correct.
