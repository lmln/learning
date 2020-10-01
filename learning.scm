(library (learning)

  (export train
          backpropagate
          cost-derivative
          stochastic-gradient-descent

          initialize-network
          feedforward
          evaluate)

  (import (chezscheme)
          (cmatrices)
          (utils))

  (define-structure (network layers sizes biases weights))

  (define-structure (sample input output))

  (define (initialize-network sizes . opt)
    (let ((initialize (if (null? opt) make-random-matrix (car opt))))
      (let ((layers (length sizes)))
        (make-network
         layers
         sizes
         (map (lambda (size) (initialize size 1))
              (cdr sizes))
         (map initialize
              (cdr sizes) (list-head sizes (- layers 1)))))))

  (define (map-zs&activations network input)
    (let traverse-network ((biases (network-biases network))
                           (weights (network-weights network))
                           (activation input) (activations (list input))
                           (zs '()))
      (cond ((or (null? biases) (null? weights))
             (values zs activations))
            (else
             (let* ((z (matrix-sum (matrix-mul (car weights) 
                                               activation) 
                                   (car biases)))
                    (new-activation (matrix-sigmoid z)))
               (traverse-network (cdr biases)
                                 (cdr weights)
                                 new-activation 
                                 (cons new-activation activations)
                                 (cons z zs)))))))

  (define (cost-derivative output-activations y)
    (matrix-dif output-activations y))

  (define (backpropagate network input expected)
    (let-values
        (((zs activations)
          (map-zs&activations network input)))
      (let* ((delta
              (matrix-hadamard
               (cost-derivative (car activations) expected)
               (matrix-sigmoid-derivative (car zs))))
             (delta-nabla-biases (list delta))
             (delta-nabla-weights
              (list (matrix-mul delta 
                                (matrix-transpose (cadr activations)))))
             (weights (reverse (cdr (network-weights network))))
             (zs (cdr zs))
             (activations (cddr activations)))
        (reverse-accumulate-2
         delta-nabla-biases delta-nabla-weights
         (lambda (weight z activation)
           (set! delta (matrix-hadamard
                        (matrix-mul (matrix-transpose weight) 
                                    delta)
                        (matrix-sigmoid-derivative z)))
           (values delta (matrix-mul delta 
                                     (matrix-transpose activation))))
         weights zs activations))))

  (define (train network mini-batch eta)
    (let ((batch-length (vector-length mini-batch)))
      (let-values (((nabla-biases nabla-weights)
                    (vector-fold-right-2
                     (lambda (nabla-biases nabla-weights sample)
                       (let-values
                           (((delta-nabla-biases delta-nabla-weights)
                             (backpropagate network
                                            (sample-input sample)
                                            (sample-output sample))))
                         (values
                          (map matrix-sum nabla-biases delta-nabla-biases)
                          (map matrix-sum nabla-weights delta-nabla-weights))))
                     (map matrix-zeros-like (network-biases network))
                     (map matrix-zeros-like (network-weights network))
                     mini-batch)))
        (make-network
         (network-sizes network)
         (network-layers network)
         (map (lambda (bias nabla-bias)
                (matrix-dif bias (matrix-scale (/ eta batch-length) nabla-bias)))
              (network-biases network) nabla-biases)
         (map (lambda (weight nabla-weight)
                (matrix-dif weight (matrix-scale (/ eta batch-length) nabla-weight)))
              (network-weights network) nabla-weights)))))

  (define stochastic-gradient-descent
    (lambda (network training-data testing-data minibatch-size epochs eta)
      (let ((n (vector-length training-data)))
        (let iter ((e 1) (network network))
          ;;(display (format "~s " e))
          (let* ((training-data (shuffle-vector training-data))
                 (mini-batches (vector->list-of-vectors training-data 
                                                        minibatch-size))
                 (network
                  (fold-left (lambda (n mini-batch) (train n mini-batch eta))
                             network mini-batches)))
            (if (not (null? testing-data))
                (let ()
                  ;; (plot-layers network (format "Epoch ~s " e))
                  (display (format "~s " (evaluate network testing-data)))))
            (if (= e epochs) network (iter (add1 e) network)))))))

  (define (feedforward network a)
    (fold-left (lambda (a bias weight)
                 (matrix-sigmoid (matrix-sum (matrix-mul weight a) bias)))
               a
               (network-biases network) (network-weights network)))

  (define (evaluate network testing-data)
    (/ (vector-fold-right
        (lambda (previous sample)
          (+ previous
             (if (= (matrix-max-row (feedforward network 
                                                 (sample-input sample)))
                    (matrix-max-row (sample-output sample)))
                 1 0)))
        0 testing-data)
       (vector-length testing-data) 1.0)))
