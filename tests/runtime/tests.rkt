;; Time benchmark
(require racket racket/main rackunit racket/system)
(require (file "../utilities.rkt")) ;; code compilation using "make"
(require (file "../test-generator/test-generator.rkt"))

;; TODO: use load for now, change later to "require"
(load "../../atmega163-simulator.rkt")
(load "../../instruction-table.rkt")
(load "../../procedures.rkt")

(define test-dir "001-loading/")

;; generate N random instructions with random arguments
(define N 30)
(time 
 (for ([i 1])
   (define code (generate-random-code 3000))
   (display-lines-to-file code (~a test-dir "test.S")
                          #:exists 'truncate/replace)
   (make test-dir)))

;; reset the machine, load everything, 
;; and let the init code execute
(define (load-hex test-dir)
  (define symbol-file (~a test-dir "main.sim"))
  (load-symbol-table symbol-file)
  (define hex-file (~a test-dir "main.hex"))
  (reset-machine)
  (hex->flash! hex-file)
  (flash->procedures!))

(time (for ([_ 100])(load-hex test-dir)))


(* 2 -2047)
(* 2 2048)
(* 4194303 2)
(for ([i (range -2048 -2046 2)])
  (display-to-file (~a "RJMP " i)
                   (~a test-dir "test.S")
                   #:exists 'truncate/replace)
  (make test-dir)
  (load-hex test-dir)
  (print-flash-procedures (vector-take PROCEDURES 1)))

