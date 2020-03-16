(library (plot)
  (export vector->gnuplot-matrix
          matrix->gnuplot-matrix
          gnuplot-plot-matrix-to-png)
  (import (matrices)
          (chezscheme))

  (define (gnuplot-set-output filename)
    (string-append "set output '" filename "';"))

  (define (gnuplot-create-pipe . script)
    (system (string-append "echo " (apply string-append script) " | gnuplot")))

  (define (gnuplot-set-terminal-png)
    "set terminal png; ")

  (define (gnuplot-set-xrange min max)
    (string-append "set xrange [" (number->string min) ":" (number->string max) "];"))

  (define (gnuplot-set-yrange min max)
    (string-append "set yrange [" (number->string min) ":" (number->string max) "];"))

  (define (gnuplot-set-title title)
    (string-append "set title \"" title "\";"))

  (define (gnuplot-plot-matrix-with-image filename)
    (string-append
     "plot '"
     filename
     "' matrix with image"))

  (define (gnuplot-plot-matrix-to-png
           input-file output-file title stride)
    (gnuplot-create-pipe
     (gnuplot-set-output output-file)
     (gnuplot-set-xrange 0 (- stride 1))
     (gnuplot-set-yrange 0 (- stride 1))
     (gnuplot-set-title title)
     (gnuplot-set-terminal-png)
     (gnuplot-plot-matrix-with-image input-file)))

  (define (vector->gnuplot-matrix vector outputname . opt)
    (call-with-output-file outputname
      (lambda (output)
        (let ((x 0) (y 0)
              (stride (if (null? opt)
                          (sqrt (vector-length vector))
                          (car opt))))
          (vector-for-each
           (lambda (num)
             (if (= x stride)
                 (begin (display (format "\n") output)
                        (set! x 0) (set! y (+ 1 y))))
             (display num output)
             (display " " output)
             (set! x (+ x 1)))
           vector)))))

  (define (matrix->gnuplot-matrix matrix outputname)
    (call-with-output-file outputname
      (lambda (output)
        (vector-for-each
         (lambda (row)
           (vector-for-each
            (lambda (num)
              (display num output)
              (display " " output))
            row)
           (display (format "\n") output))
         matrix))))

  (define (plot-matrices matrices name)
    (let ((n 0))
      (for-each
       (lambda (matrix)
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
          matrix))
       matrices))))
