;; Time benchmark
(require racket racket/main rackunit racket/system)
(require (file "../utilities.rkt")) ;; code compilation using "make"
(require (file "./runtime-utilities.rkt"))
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

(define (measure-and-save-time n file-name)
  (define cpu 0)
  (define real 0)
  (define gc 0)
  
  (for ([i n])
    (define code (generate-random-code 3000))
    (display-lines-to-file code (~a test-dir "test.S")
                           #:exists 'truncate/replace)
    (make test-dir)
    (define-values (_ cpu-i real-i gc-i)
      (time-apply load-hex (list test-dir)))
    (set! cpu  (+ cpu  cpu-i))
    (set! real (+ real real-i))
    (set! gc   (+ gc   gc-i)))
  ;; append the time to the file
  (append-data-to-file n cpu real gc file-name))

(measure-and-save-time 100 "./runtimes.org")
