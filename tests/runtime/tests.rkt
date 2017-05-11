;; Time benchmark
(require racket racket/main rackunit racket/system)
(require (file "../utilities.rkt")) ;; code compilation using "make"
(require (file "../test-generator/test-generator.rkt"))

;; TODO: use load for now, change later to "require"
(load "../../atmega163-simulator.rkt")
(load "../../instruction-table.rkt")
(load "../../procedures.rkt")

(define test-dir "001-loading/")

;; reset the machine, load everything, 
;; and let the init code execute
(define (load-hex test-dir)
  (define symbol-file (~a test-dir "main.sim"))
  (load-symbol-table symbol-file)
  (define hex-file (~a test-dir "main.hex"))
  (reset-machine)
  (hex->flash! hex-file)
  (flash->procedures!))

(define (measure-time n)
  (for ([i n])
    (define code (generate-random-code 3000))
    (display-lines-to-file code (~a test-dir "test.S")
                           #:exists 'truncate/replace)
    (make test-dir)
    (load-hex test-dir)))

(measure-and-save-time measure-time (list 100) "./runtimes.org")
