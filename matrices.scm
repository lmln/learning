(library (matrices)

  (export
   make-matrix
   matrix?
   matrix-rows
   matrix-columns
   matrix-ref
   matrix-set!
   matrix-map
   matrix-hadamard
   matrix-dif
   matrix-sum
   matrix-mul
   transpose
   matrix-transpose
   matrix-scale
   T
   make-one-hot-vector
   make-random-matrix
   matrix-sigmoid
   matrix-sigmoid-derivative
   matrix-max-row
   matrix-zeros-like)

  (import (chezscheme))

  (define make-matrix
    (case-lambda
     ((rows columns)
      (do ((m (make-vector rows))
           (i 0 (fx+ i 1)))
          ((fx= i rows) m)
        (vector-set! m i (make-vector columns 0.0))))
     ((rows columns value)
      (do ((m (make-vector rows))
           (i 0 (fx+ i 1)))
          ((fx= i rows) m)
        (vector-set! m i (make-vector columns value))))))

  (define matrix?
    (lambda (x)
      (and (vector? x)
           (fx> (vector-length x) 0)
           (vector? (vector-ref x 0)))))

  (define matrix-rows
    (lambda (x)
      (vector-length x)))

  (define matrix-columns
    (lambda (x)
      (vector-length (vector-ref x 0))))

  ;; matrix-ref returns the jth element of the ith row.
  (define matrix-ref
    (lambda (m i j)
      (vector-ref (vector-ref m i) j)))

  ;; matrix-set! changes the jth element of the ith row.
  (define matrix-set!
    (lambda (m i j x)
      (vector-set! (vector-ref m i) j x)))

  (define (matrix-map function . matrices)
    (apply vector-map (lambda m (apply vector-map function m)) matrices))

  (define (matrix-hadamard . matrices)
    (apply matrix-map fl* matrices))
  (define (matrix-dif . matrices)
    (apply matrix-map fl- matrices))

  (define (matrix-sum . matrices)
    (apply matrix-map fl+ matrices))

  (define (matrix-scale scalar m)
    (matrix-map (lambda (x) (fl* x scalar)) m))

  (define (matrix-max-row m)
    (let ((x 0) (y 0) (max-x 0) (max-y 0)
          (max (matrix-ref m 0 0)))
      (vector-for-each
       (lambda (row)
         (vector-for-each
          (lambda (n)
            (if (> n max)
                (begin (set! max-x x) (set! max-y y) (set! max n)))
            (set! x (+ x 1))) row)
         (set! y (+ y 1))) m)
      max-y))

  (define sigmoid
    (lambda (z) (/ 1 (+ 1 (exp (- 0 z))))))

  (define matrix-sigmoid
    (lambda (z)
      (if (matrix? z)
          (matrix-map (lambda (z) (fl/ 1.0 (+ 1.0 (exp (- 0.0 z))))) z)
          (fl/ 1.0 (fl+ 1.0 (exp (fl- 0.0 z)))))))

  (define matrix-sigmoid-derivative
    (lambda (x)
      (if (matrix? x)
          (matrix-map (lambda (x) (let ((sx (sigmoid x))) (fl* sx (- 1.0 sx)))) x)
          (fl* (sigmoid x) (fl- 1.0 (sigmoid x))))))

  (define transpose
    (lambda (m)
      (let* ([nr (matrix-rows m)]
             [nc (matrix-columns m)]
             [r (make-matrix nc nr)])
        (do ([i 0 (+ i 1)])
            ((= i nr) r)
          (do ([j 0 (+ j 1)])
              ((= j nc))
            (matrix-set! r j i (matrix-ref m i j)))))))

  (define matrix-transpose transpose)

  (define T matrix-transpose)

  (define matrix-mul
    (lambda (m1 m2)
      (let* ((nr1 (matrix-rows m1))
             (nr2 (matrix-rows m2))
             (nc2 (matrix-columns m2))
             (r   (make-matrix nr1 nc2)))
        (if (not (fx= (matrix-columns m1) nr2))
            (error m1 m2))
        (do ((i 0 (fx+ i 1)))
            ((fx= i nr1) r)

          (do ((k 0 (fx+ k 1)))
              ((fx= k nr2))

            (let ((ith-input-row (vector-ref m1 i))
                  (kth-input-row (vector-ref m2 k))
                  (ith-output-row (vector-ref r i)))

              (do ((j 0 (fx+ j 1)))
                  ((fx= j nc2))
                (vector-set! ith-output-row j
                             (fl+ (vector-ref ith-output-row j)
                                  (fl* (vector-ref ith-input-row k)
                                       (vector-ref kth-input-row j)))))))))))

  ;; mapping from uniform to normal distribution
  (define box-muller
    (lambda ()
      (let* ((u1 (random 1.0))
             (u2 (random 1.0))
             (pi 3.14159265358979323846)
             (R (sqrt (* -2 (log u1))))
             (theta (* 2 pi u2)))
        (* R (cos theta)))))

  ;; with normal distribution
  ;; (define make-random-matrix
  ;;   (lambda (j k)
  ;;     (matrix-map (lambda (x) (box-muller)) (make-matrix j k))))


  (define make-random-matrix
    (lambda (j k)
      (matrix-map (lambda (x) (- (random 4.0) 2.0)))))  


  (define (matrix-zeros-like x)
    (make-matrix (matrix-rows x) (matrix-columns x) 0.0))

  (define make-one-hot-vector
    (lambda (n max)
      (let ((m (make-matrix max 1 0.0)))
        (matrix-set! m n 0 1.0)
        m))))
