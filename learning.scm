(library (learning)

  (export train
          backpropagate
          cost-derivative
          stochastic-gradient-descent

          make-network
          network?
          network-layers
          network-sizes
          network-biases
          network-weights

          initialize-network
          feedforward
          evaluate)

  (import (chezscheme)
          (plot)
          (matrices)
          (utils))

  (define-structure (network layers sizes biases weights))

  (define (plot-layers network name)
    (display (network-layers network))
    (let ((n 0))
      (for-each
       (lambda (layer)
         (vector-for-each
          (lambda (node)
            (let ((len (sqrt (vector-length node)))
                  (filename (string-append name (number->string n))))
              (vector->gnuplot-matrix 
               node
               filename
               len)
              (gnuplot-plot-matrix-to-png 
               filename
               (string-append filename ".png")
               filename
               len))
            (set! n (+ n 1)))
          layer))        
       (network-weights network))))

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
             (let* ((z (matrix-sum (matrix-mul (car weights) activation) (car biases)))
                    (new-activation (matrix-sigmoid z)))
               (traverse-network (cdr biases)
                                 (cdr weights)
                                 new-activation (cons new-activation activations)
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
              (list (matrix-mul delta (matrix-transpose (cadr activations)))))
             (weights (reverse (cdr (network-weights network))))
             (zs (cdr zs))
             (activations (cddr activations)))
        (reverse-accumulate-2
         delta-nabla-biases delta-nabla-weights
         (lambda (weight z activation)
           (set! delta (matrix-hadamard
                        (matrix-mul (matrix-transpose weight) delta)
                        (matrix-sigmoid-derivative z)))
           (values delta (matrix-mul delta (matrix-transpose activation))))
         weights zs activations))))

  (define (train network mini-batch eta)
    (let ((batch-length (vector-length mini-batch)))
      ;;(spin)
      (let-values (((nabla-b nabla-w)
                    (vector-fold-right-2
                     (lambda (nabla-b nabla-w pair)
                       (let-values
                           (((delta-nabla-b delta-nabla-w)
                             (backpropagate network (vector-ref pair 1)
                                            (vector-ref pair 0))))
                         (values
                          (map matrix-sum nabla-b delta-nabla-b)
                          (map matrix-sum nabla-w delta-nabla-w))))
                     (map matrix-zeros-like (network-biases network))
                     (map matrix-zeros-like (network-weights network))
                     mini-batch)))
        (make-network
         (network-sizes network)
         (network-layers network)
         (map (lambda (bias nb)
                (matrix-dif bias (matrix-scale (/ eta batch-length) nb)))
              (network-biases network) nabla-b)
         (map (lambda (weight nw)
                (matrix-dif weight (matrix-scale (/ eta batch-length) nw)))
              (network-weights network) nabla-w)))))

  (define stochastic-gradient-descent
    (lambda (network training-data testing-data minibatch-size epochs eta . current-epoch)
      (let ((n (vector-length training-data)))
        (let iter ((e (if (null? current-epoch) 1 (car current-epoch))) (network network))
          (display (format "epoch ~s\n" e))
          (let* ((training-data (shuffle-vector training-data))
                 (mini-batches (vector->list-of-vectors training-data minibatch-size))
                 (network
                  (fold-left (lambda (n mini-batch) (train n mini-batch eta))
                             network mini-batches)))           
            (save e "epoch")
            (save network "network")
            (if (not (null? testing-data))
                (let ()
                  (plot-layers network (format "Epoch ~s " e))
                  (display (format "accuracy: ~s\n" (evaluate network testing-data)))))
            (collect)
            (if (= e epochs) network (iter (add1 e) network)))))))

  (define (feedforward network a)
    (fold-left (lambda (a bias weight)
                 (matrix-sigmoid (matrix-sum (matrix-mul weight a) bias)))
               a
               (network-biases network) (network-weights network)))

  (define (evaluate network test-data)
    (/ (vector-fold-right
        (lambda (previous pair)
          (+ previous
             (if (= (matrix-max-row (feedforward
                                     network (vector-ref pair 1)))
                    (matrix-max-row (vector-ref pair 0))) 1 0))) 0 test-data)
       (vector-length test-data)
       1.0)))
