(library (mnist-loading)

  (export *size-of-the-training-data*
          *size-of-the-testing-data*
          create-data-vector
          load-testing-data
          load-training-data)

  (import (chezscheme)
          (cmatrices)
          (learning))

  (define *size-of-the-training-data* 59999)

  (define *size-of-the-testing-data* 9992)

  (define-structure (sample input output))

  (define (get-u32 p)
    (logior (get-u8 p)
            (ash (get-u8 p) 8)
            (ash (get-u8 p) 16)
            (ash (get-u8 p) 24)))

  (define (get-sample images labels dimension)
    (let* ((size (* dimension dimension))
           (new-matrix (make-matrix size 1 0.0)))
      (let iter ((n 0))
        (unless (= n size)
          (matrix-set! new-matrix n 0 (/ (get-u8 images) 255.0))
          (iter (add1 n))))
      (make-sample new-matrix (make-one-hot-vector (get-u8 labels) 10))))

  (define (create-data-vector images labels vector dimension)
    (let ((length (vector-length vector)))
      (let iterate ((count 0))
        (unless (= length count)
          (vector-set! vector count (get-sample images labels dimension))
          (iterate (+ 1 count))))
      (display (format "Data set loaded.~n"))
      vector))

  (define testing-images
    (parameterize ([file-buffer-size 256])
                  (open-file-input-port
                   "data/t10k-images.idx3-ubyte")))

  (define testing-labels
    (parameterize ([file-buffer-size 256])
                  (open-file-input-port
                   "data/t10k-labels.idx1-ubyte")))

  (define (skip-testing-headers)
    (begin
      (get-u32 testing-labels)
      (get-u32 testing-labels)
      (get-u32 testing-images)
      (get-u32 testing-images)
      (get-u32 testing-images)
      (get-u32 testing-images)
      (display (format "Test headers skipped.~n"))))

  (define (load-testing-data)
    (let ((testing-data (make-vector *size-of-the-testing-data*)))
      (skip-testing-headers)
      (let ((training-data
             (create-data-vector
              testing-images
              testing-labels
              testing-data 28)))
        (close-input-port testing-images)
        (close-input-port testing-labels)
        testing-data)))

  (define training-images
    (parameterize ([file-buffer-size 256])
                  (open-file-input-port
                   "data/train-images.idx3-ubyte")))

  (define training-labels
    (parameterize ([file-buffer-size 256])
                  (open-file-input-port
                   "data/train-labels.idx1-ubyte")))

  (define (skip-training-headers)
    (begin
      (get-u32 training-labels)
      (get-u32 training-labels)
      (get-u32 training-images)
      (get-u32 training-images)
      (get-u32 training-images)
      (get-u32 training-images)
      (display (format "Headers skipped.~n"))))

  (define (load-training-data)
    (let ((training-data (make-vector *size-of-the-training-data*)))
      (skip-training-headers)
      (let ((training-data
             (create-data-vector
              training-images
              training-labels
              training-data 28)))
        (close-input-port training-images)
        (close-input-port training-labels)
        training-data))))
