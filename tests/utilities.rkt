(module utilities racket
  (require racket/system)
  (provide make)

  (define (make)
    (define status-make-clean 
      (system "make clean > make-clean.log 2>&1"))
    (unless status-make-clean
      (error 'make-clean "make clean was not successful."))
    (define status-make (system "make > make.log 2>&1"))
    (unless status-make
      (error 'make "make was not successful."))))
