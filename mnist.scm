
(let ()
  (import (mnist-loading)
          (learning)
          (cmatrices)
          (utils)
          (chezscheme))

  ;; enlarge the allocate block each time and avoid collecting frequently
  ;; -- it speeds things up when we are using native matrices

  ;; (collect-trip-bytes (* 10 (collect-trip-bytes)))
  ;; (collect-generation-radix 1000000)

  ;; try to continue from saved file
  (let* ((network (initialize-network '(784 30 10)))
         (training-data (load-training-data))
         (testing-data (load-testing-data))
         (trained-network
          (time (stochastic-gradient-descent
                 network
                 training-data
                 testing-data
                 10  ;; minibatch-size
                 5   ;; epochs
                 3.0 ;; learning  rate
                 ))))

    (display (format "test data accuracy: ~s\n"
                     (evaluate trained-network test-data)))

    trained-network))
