
(library (mnist-loading)

  (export *size-of-the-training-data*
          *size-of-the-test-data*
          print-letter
          create-data-vector
          ;;check-training-data
          load-test-data
          load-training-data)

  (import (chezscheme)
          (matrices)
          (learning))

  (define *size-of-the-training-data* 59999)

  (define *size-of-the-test-data* 9992)

  (define print-letter ; works only with matrices.scm
    (lambda (v c n)
      (define helper
        (lambda (character)
          (if (= 0 (modulo c 28))
              (let ()
                (display character)
                (newline)
                (print-letter v (add1 c) n))
              (let ()
                (display character)
                (print-letter v (add1 c) n)))))
      (cond
       ((= c n) (let () (newline)))
       ((< 225 (vector-ref v c)) (helper "#"))
       ((< 50 (vector-ref v c)) (helper "+"))
       ((< 0 (vector-ref v c)) (helper "."))
       ((>= 0 (vector-ref v c)) (helper " "))
       (else
        (error 'print-letter "can't print")))))

  ;; mnist loading

  (define get-u32
    (lambda (p)
      (logior (get-u8 p)
              (ash (get-u8 p) 8)
              (ash (get-u8 p) 16)
              (ash (get-u8 p) 24))))

  ;; (define get-pair
  ;;   (lambda (t y dim)
  ;;     (let ((size (* dim dim)))
  ;;       (letrec ((i (lambda (times)
  ;;                     (if (zero? times)
  ;;                         (quote ())
  ;;                         ; (cons (get-u8 p) (i (sub1 times)))))))
  ;;                         (cons (/ (get-u8 t) 255.0) (i (sub1 times))))))) ; scaling by 255
  ;;         (vector (get-u8 y) (list->vector (i size)))))))

  (define get-pair
    (lambda (t y dim)
      (let* ((size (* dim dim))
             (new-matrix (make-matrix size 1 0.0)))

        (let iter ((n 0))
          (unless (= n size)
                  (matrix-set! new-matrix n 0 (/ (get-u8 t) 255.0)) ;; 1 -> 0
                  (iter (add1 n))))
        (let ((v (vector (make-one-hot-vector (get-u8 y) 10) new-matrix)))
          v))))

  ;; create vector with training data-set

  (define create-data-vector
    (lambda (p l v dimension)
      (let ((vs (vector-length v)))
        (letrec
            ((f (lambda (c)
                  (cond
                   ((= vs c)
                    (let ()
                      (display "Data set loaded")
                      (newline)
                      v))
                   (else
                    (let ()
                      (vector-set! v c (get-pair p l dimension))
                      (f (add1 c))))))))
          (f 0)))))


  ;; loading test data

  (define o
    (parameterize ([file-buffer-size 256])
                  (open-file-input-port
                   "data/t10k-images.idx3-ubyte")))

  (define k
    (parameterize ([file-buffer-size 256])
                  (open-file-input-port
                   "data/t10k-labels.idx1-ubyte")))



  (define skip-test-headers
    (lambda ()
      (begin
        (get-u32 k)
        (get-u32 k)
        (get-u32 o)
        (get-u32 o)
        (get-u32 o)
        (get-u32 o)
        (display "Test headers skipped.")
        (newline))))

  (define (load-test-data)
    (define test-data
      (make-vector *size-of-the-test-data*
                   (vector
                    (make-vector 1)
                    (make-vector 784))))
    (skip-test-headers)

    (set! test-data (create-data-vector o k test-data 28)) ;; lodading dataset 2

    (close-input-port o)

    (close-input-port k)
    test-data)

  ;;
  ;; loading training data
  ;;

  (define p
    (parameterize ([file-buffer-size 3])
                  (open-file-input-port
                   "data/train-images.idx3-ubyte")))

  (define l
    (parameterize ([file-buffer-size 3])
                  (open-file-input-port
                   "data/train-labels.idx1-ubyte")))

  (define skip-training-headers
    (lambda ()
      (begin
        (get-u32 l)
        (get-u32 l)
        (get-u32 p)
        (get-u32 p)
        (get-u32 p)
        (get-u32 p)
        (display "Headers skipped.")
        (newline))))

  ;; (define (load-training-data)
  ;;   (define training-data
  ;;     (make-vector
  ;;      *size-of-the-training-data*))
  ;;   (skip-training-headers) ;; skiping file headers
  ;;   (set! training-data (create-data-vector p l training-data 28)) ;; loading dataset
  ;;   (close-input-port p)
  ;;   (close-input-port l)
  ;;   training-data)

  (define (load-training-data)
    (let ((training-data (make-vector *size-of-the-training-data*)))
      (skip-training-headers)
      (let ((training-data (create-data-vector p l training-data 28)))
        (close-input-port p)
        (close-input-port l)
        training-data))))
