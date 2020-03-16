(library (cmatrices)  

  (export  
   initialize-cmatrices
   collect-cmatrices
   display-cmatrix
   
   matrix-set!
   matrix-ref

   matrix-rows
   matrix-columns
   matrix-max-row

   make-matrix
   make-random-matrix
   make-one-hot-vector

   matrix-hadamard
   ;;matrix-map
   matrix-sum
   matrix-dif
   matrix-mul
   matrix-scale
   matrix-sigmoid
   matrix-sigmoid-derivative
   matrix-transpose
   v-of-v->matrix
   free-cmatrix
   T
   matrix-zeros-like
   get-matrix-count
   box-muller)

  (import           
   (chezscheme))

  (define shared-library
    (load-shared-object "libcmatrices.so"))
  
  (define-ftype
    (cmatrix (struct 
              (rows int)
              (columns int)
              (nums (* double)))))
  
  (define make-cmatrix
    (foreign-procedure "make_matrix" 
                       (int int double)
                       (* cmatrix)))

  (define make-random-cmatrix
    (foreign-procedure "make_random_matrix" 
                       (int int)
                       (* cmatrix)))  
  
  (define free-cmatrix
    (foreign-procedure "free_matrix" 
                       ((* cmatrix))
                       void))
  
  (define cmatrix*cmatrix->cmatrix
    (lambda (str)
      (foreign-procedure str ((* cmatrix) (* cmatrix)) (* cmatrix))))

  (define chadamard
    (cmatrix*cmatrix->cmatrix "hadamard"))

  (define chadamard!
    (cmatrix*cmatrix->cmatrix "destructive_hadamard"))

  (define csum
    (cmatrix*cmatrix->cmatrix "sum"))

  (define csum!
    (cmatrix*cmatrix->cmatrix "destructive_sum"))

  (define cdif
    (cmatrix*cmatrix->cmatrix "dif"))

  (define cdif!
    (cmatrix*cmatrix->cmatrix "destructive_dif"))

  (define cmul
    (cmatrix*cmatrix->cmatrix "mul"))

  (define cscale
    (foreign-procedure "scale" 
                       (double (* cmatrix))
                       (* cmatrix)))

  (define cscale!
    (foreign-procedure "destructive_scale" 
                       (double (* cmatrix))
                       (* cmatrix)))

  (define csigmoid
    (foreign-procedure "sigmoid" 
                       ((* cmatrix))
                       (* cmatrix)))

  (define csigmoid!
    (foreign-procedure "destructive_sigmoid" 
                       ((* cmatrix))
                       (* cmatrix)))

  (define csigmoid-derivative
    (foreign-procedure "sigmoid_derivative" 
                       ((* cmatrix))
                       (* cmatrix)))

  (define csigmoid-derivative!
    (foreign-procedure "destructive_sigmoid_derivative" 
                       ((* cmatrix))
                       (* cmatrix)))

  (define ctranspose
    (foreign-procedure "transpose" 
                       ((* cmatrix))
                       (* cmatrix)))

  (define display-cmatrix
    (foreign-procedure "display_matrix" 
                       ((* cmatrix))
                       void))

  (define cmatrix-set!
    (foreign-procedure "matrix_set" 
                       ((* cmatrix) int int double)
                       void))

  (define matrix-set! cmatrix-set!)

  (define cmatrix-ref
    (foreign-procedure "matrix_ref" 
                       ((* cmatrix) int int)
                       double))

  (define matrix-ref cmatrix-ref)

  (define cmatrix-columns
    (foreign-procedure "matrix_columns" 
                       ((* cmatrix))
                       int))

  (define matrix-columns cmatrix-columns)

  (define cmatrix-rows
    (foreign-procedure "matrix_rows" 
                       ((* cmatrix))
                       int))

  (define matrix-rows cmatrix-rows)
  
  (define cmatrix-max-column
    (foreign-procedure "matrix_max_column" 
                       ((* cmatrix))
                       int))

  (define cmatrix-max-row
    (foreign-procedure "matrix_max_row" 
                       ((* cmatrix))
                       int))

  (define matrix-max-row cmatrix-max-row)

  (define cmatrix-max-value
    (foreign-procedure "matrix_max_value" 
                       ((* cmatrix))
                       int))

  (define matrix-max-value cmatrix-max-value)

  (define v-of-v->matrix
    (lambda (m)
      (let* ([nr (vector-length m)]
             [nc (vector-length (vector-ref m 0))]
             [r (make-matrix nr nc 0.0)])
        (do ([i 0 (+ i 1)])
            ((= i nr) r)
          (do ([j 0 (+ j 1)])
              ((= j nc))
            (cmatrix-set! r i j (vector-ref (vector-ref  m i) j))))
        r)))

  ;; (define cmatrix->matrix
  ;;   (lambda (fm)
  ;;     (let* ([nr (cmatrix-rows fm)]
  ;;            [nc (cmatrix-columns fm)]
  ;;            [r (make-matrix nr nc)])
  ;;       (do ([i 0 (+ i 1)])
  ;;           ((= i nr) r)
  ;;         (do ([j 0 (+ j 1)])
  ;;             ((= j nc))
  ;;           (matrix-set! r i j (cmatrix-ref fm i j))))
  ;;       r)))
  
  
  (define cmatrix-guardian (make-guardian))

  (define matrix-count 0)

  (define make-matrix
    (lambda (rows columns value)
      (let ([x (make-cmatrix rows columns value)])
        (cmatrix-guardian x)
        ;;(set! matrix-count (+ matrix-count 1))                  
        x)))

  ;; (define make-matrix-with-value
  ;;   (lambda (rows columns value)
  ;;     (let ([x (make-cmatrix-with-value rows columns value)])
  ;;       (cmatrix-guardian x)
  ;;       x)))

  (define make-random-matrix
    (lambda (rows columns)
      (let ([x (make-random-cmatrix rows columns)])
        ;;(set! matrix-count (+ matrix-count 1))                  
        (cmatrix-guardian x)
        x)))  
  
  (define matrix-hadamard
    (lambda (m n)
      (let ((x (chadamard m n)))
        (cmatrix-guardian x)
        x)))

  (define matrix-sum
    (lambda (m n)
      (let ((x (csum m n)))
        (cmatrix-guardian x)
        x)))
  
  (define matrix-dif
    (lambda (m n)
      (let ((x (cdif m n)))
        (cmatrix-guardian x)
        x)))
  
  (define matrix-mul
    (lambda (m n)
      (let ((x (cmul m n)))
        (cmatrix-guardian x)
        ;;(set! matrix-count (+ matrix-count 1))                  
        x)))

  (define matrix-scale
    (lambda (m n)
      (let ((x (cscale m n)))
        (cmatrix-guardian x)
        x)))

  (define matrix-sigmoid
    (lambda (m)
      (let ((x (csigmoid m)))
        (cmatrix-guardian x)
        x)))

  (define matrix-sigmoid-derivative
    (lambda (m)
      (let ((x (csigmoid-derivative m)))
        (cmatrix-guardian x)
        x)))

  (define matrix-transpose
    (lambda (m)
      (let ((x (ctranspose m)))
        (cmatrix-guardian x)
        x)))
  
  (define T matrix-transpose)

  (define make-one-hot-vector
    (lambda (n max)
      (let ((m (make-matrix max 1 0.0))) 
        (cmatrix-set! m n 0 1.0)
        m)))

  (define (collect-cmatrices)
    (let f ()
      (let ([x (cmatrix-guardian)])
        (when x
              (free-cmatrix x)
              (f)))))

  (define initialize-cmatrices
    (lambda ()        
      (collect-request-handler
       (lambda ()
         (collect)
         (collect-cmatrices)))))
  
  (define (get-matrix-count)
    matrix-count)

  (define (matrix-zeros-like x)
    (make-matrix (matrix-rows x) (matrix-columns x) 0.0))

  (define box-muller
    (foreign-procedure "box_muller" 
                       ()
                       double))
   
  ;; to initialize the gc
  (define initialization
    (initialize-cmatrices)))
