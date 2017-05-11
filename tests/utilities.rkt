(module utilities racket
  (require racket/system)
  (provide make)

  (define (make (dir "./"))
    (define prefix (string-append "cd " dir "; "))
    (define status-make-clean
      (system
       (string-append prefix "make clean > make-clean.log 2>&1")))
    (unless status-make-clean
      (error 'make-clean "make clean was not successful."))
    (define status-make (system
                         (string-append prefix
                                        "make > make.log 2>&1")))
    (unless status-make
      (error 'make "make was not successful.")))

  

  )
