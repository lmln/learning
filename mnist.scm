
(let ()
  (import (mnist-loading)
          (learning)
          (matrices)
          (chezscheme))
  ;; enlarge the allocate block each time and avoid collecting frequently
  (collect-trip-bytes (* 10 (collect-trip-bytes)))
  (collect-generation-radix 1000000)
  (time
   (let* ((network (initialize-network '(784 30 10)))
          (training-data (load-training-data))
          (test-data (load-test-data))
          (trained-network
           (stochastic-gradient-descent
            network       ;; network
            training-data ;; training data
            test-data     ;; testing data
            10            ;; minibatch-size
            1             ;; epochs
            3.0)))        ;; eta

     (display (format "training data accuracy: ~s\n"
                      (evaluate trained-network training-data)))

     (display (format "test data accuracy: ~s\n"
                      (evaluate trained-network test-data))))))






