
(let ()
  (import (mnist-loading)
          (learning)
          (cmatrices)
          (utils)
          (chezscheme))

  (if (not (= (length (command-line)) 4)) (error 'mnist "Too few arguments"))

  ;; enlarge the allocate block each time and avoid collecting frequently
  ;; -- it speeds things up when we are using native matrices

  ;; (collect-trip-bytes (* 10 (collect-trip-bytes)))
  ;; (collect-generation-radix 1000000)

  ;; try to continue from saved file
  (let* ((network (initialize-network '(784 30 30 10)))
         (training-data (load-training-data))
         (testing-data (load-testing-data))
         (minibatch-size (string->number (cadr (command-line))))  ;; 10
         (epochs (string->number (caddr (command-line)))) ;; 2
         (learning-rate (string->number (cadddr (command-line)))) ;; 3.0
         (trained-network
          (time (stochastic-gradient-descent
                 network
                 training-data
                 testing-data
                 minibatch-size
                 epochs
                 learning-rate
                 ))))

    trained-network))
