
(let ()
  (import (mnist-loading)
          (learning)
          (matrices)
          (utils)
          (chezscheme))

  ;; enlarge the allocate block each time and avoid collecting frequently
  (collect-trip-bytes (* 10 (collect-trip-bytes)))
  (collect-generation-radix 1000000)

  ;; try to continue from saved file
  (let ((restored-network (restore "network"))
        (restored-epoch (restore "epoch")))

    (let* ((network (if restored-network restored-network 
                        (initialize-network '(784 30 10))))
           (training-data (load-training-data))
           (test-data (load-test-data))
           (trained-network
            (stochastic-gradient-descent
             network       
             training-data 
             test-data
             ;; minibatch-size
             10
             ;; epochs
             100
             ;; learning rate
             3.0
             ;; passing an optional argumnt with current epoch 
             (if restored-epoch (+ restored-epoch 1) 1))))

      (display (format "training data accuracy: ~s\n"
                       (evaluate trained-network training-data)))

      (display (format "test data accuracy: ~s\n"
                       (evaluate trained-network test-data)))
      
      (pretty-print trained-network))))




