(library (utils)
  (export vector-splice
          vector->list-of-vectors
          rand-range
          shuffle-vector!
          shuffle-vector
          second-to-last
          list-of-vectors-shape
          reverse-accumulate-2
          reverse-fold-2
          vector-fold-right-2
          vector-fold-right
          save
          restore)

  (import
   (chezscheme))

  (define (save something filename)
    (call-with-output-file filename
      (lambda (port)
        (display something port))
      'replace))

  (define (restore filename)
    (call/cc
     (lambda (k)
       (with-exception-handler
        (lambda (exception)
          (if (error? exception)
              (k #f)
              (raise exception)))
        (lambda ()
          (call-with-input-file filename
            (lambda (port)
              (read port))))))))


  (define (vector-fold-right function initial vector)
    (let ((vector-length (vector-length vector)))
      (let iterate ((i 0) (accumulator initial))
        (if (= i vector-length)
            accumulator
            (iterate
             (+ i 1)
             (function accumulator (vector-ref vector i)))))))

  (define (vector-fold-right-2 function initial1 initial2 vector)
    (let ((vector-length (vector-length vector)))
      (let iterate ((i 0) (accumulator1 initial1) (accumulator2 initial2))
        (if (= i vector-length)
            (values accumulator1 accumulator2)
            (let-values (((x1 x2) (function accumulator1 accumulator2 (vector-ref vector i))))
              (iterate (+ i 1) x1 x2))))))

  (define (reverse-accumulate-2 accumulator1 accumulator2 function . list)
    (let it ((function function) (accumulator1 accumulator1) (accumulator2 accumulator2) (list list))
      (cond
       ((null? (car list)) (values accumulator1 accumulator2))
       (else (let-values (((x1 x2) (apply function (map car list))))
               (it function (cons x1 accumulator1) (cons x2 accumulator2) (map cdr list)))))))

  (define (reverse-fold-2 function accumulator1 accumulator2 . list)
    (let it ((function function) (accumulator1 accumulator1) (accumulator2 accumulator2) (list list))
      (cond
       ((null? (car list)) (values accumulator1 accumulator2))
       (else (let-values (((x1 x2) (apply function (cons* accumulator1 accumulator2 (map car list)))))
               (it function x1 x2 (map cdr list)))))))

  (define (vector-splice old-vector n m)
    (let ((new-vector (make-vector (- m n))))
      (let iter ((c 0))
        (if (< c (- m n))
            (let ()
              (vector-set! new-vector c
                           (vector-ref old-vector (+ n c)))
              (iter (add1 c)))
            new-vector))))

  (define (vector->list-of-vectors vector size) ; of size
    (do ((n 0 (+ n size))
         (l '() (cons (vector-splice vector n (+ n size)) l)))
        ((> (+ n size) (sub1 (vector-length vector)))
         (reverse (cons (vector-splice vector n (vector-length vector))
                        l)))))

  (define (rand-range min max)
    (+ min (random (add1 (- max min)))))

  (define shuffle-vector!
    (lambda (v)
      (let ((n (- (vector-length v) 1)))
        (do ((i 0 (+ i 1))) ((< n i) v)
          (let* ((r (rand-range i n))
                 (t (vector-ref v r)))
            (vector-set! v r (vector-ref v i))
            (vector-set! v i t))))))

   (define (shuffle-vector vector)
     (let ((v (vector-copy vector)))
       (let ((n (- (vector-length v) 1)))
         (do ((i 0 (+ i 1))) ((< n i) v)
           (let* ((r (rand-range i n))
                  (t (vector-ref v r)))
             (vector-set! v r (vector-ref v i))
             (vector-set! v i t))))
       v))

  (define (last list) ; last element in the list
    (cond
     ((null? list) '())
     ((null? (cdr list)) (car list))
     (else
      (last (cdr list)))))

  (define (second-to-last list) ; second to last
    (cond
     ((null? list) '())
     ((null? (cdr list)) (car list))
     ((null? (cddr list)) (car list))
     (else
      (second-to-last (cdr list)))))

  (define (list-of-vectors-shape lov s)
    (cond
     ((and (null? lov) (null? s)) #t)
     ((= (car s) (vector-length (car lov))))
     (else
      (list-of-vectors-shape (cdr lov) (cdr s))))))
