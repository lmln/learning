
(let ()
  (import
   (chezscheme)
   (learning)
   (matrices))

  (define xor-dataset
    `#(#(,(make-one-hot-vector 0 2)  #(#(0.0) #(0.0)))
       #(,(make-one-hot-vector 1 2)  #(#(1.0) #(0.0)))
       #(,(make-one-hot-vector 1 2)  #(#(0.0) #(1.0)))
       #(,(make-one-hot-vector 0 2)  #(#(1.0) #(1.0)))))

  (define xor-network (initialize-network '(2 8 2)))

  (time (stochastic-gradient-descent
         xor-network xor-dataset xor-dataset 2 1 5.0))

  (evaluate xor-network xor-dataset))



