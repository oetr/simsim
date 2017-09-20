(require racket racket/main rackunit racket/system
         (planet vyzo/crypto))

(require (file "../../tests/utilities.rkt"))

;; compile the code before simulating it
(make)

;; TODO: use load for now, change later to "require"
(load "../../atmega163-simulator.rkt")
(load "../../instruction-table.rkt")
(load "../../procedures.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load compiled binary into the simulator
;; set key and plaintext in the flash
;; run, while recording the executed instructions (only
;; on the first run), and the side channel leakage
;; save both in two files
;; see Octave code for a side-channel attack
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (run-example n key-proc pt-proc
                     #:exec-trace-name (exec-trace-name #f)
                     #:leakage-file-name (leakage-file-name #f))
  (define start-ms (current-milliseconds))

  (define leakage-file #f)    


  (reset-machine execution-trace-name)
  (load-symbol-table "main.sim")
  (hex->flash! "main.hex")
  (flash->procedures!)
  
  ;; save the number of leakage traces
  (when leakage-file-name
    (set! leakage-file 
          (open-output-file leakage-trace-name #:exists 'replace))
    (write-header leakage-file n))

  (define execution-recorded? #f)

  (for ([iteration n])
    (define key (key-proc))
    (define pt (pt-proc))
    (define iv #"00000000000000000000000000000000")
    (define correct-ct
      (subbytes (encrypt cipher:aes-128-ecb key iv pt) 0 16))
    ;; set key and plaintexts in the flash
    ;; addresses in the text area all get divided by 2
    (define pt-address (* 2 (lookup-symbol "plaintext")))
    (define key-address (* 2 (lookup-symbol "key")))
    (for ([i 16]
          [k key]
          [p pt])
      (flash-set-byte (+ pt-address i) p)
      (flash-set-byte (+ key-address i) k))
    
    ;; encrypt and compare to the correct result
    (unless execution-recorded?
      (set! debug? #t) ;; record execution trace
      (set! execution-recorded? #t))
    (set! save-intermediate-values? #t) ;; record leakage
    (set! save-hamming-distance? #t) ;; only Hamming weight otherwise
    (go-address (lookup-symbol "encrypted"))
    (set! save-intermediate-values? #f) ;; stop recording leakage
    (set! debug? #f) ;; stop recording trace
    ;; TODO: hide more from the user to reduce this 
    ;; flush output buffer if it's a file, so that the whole
    ;; execution trace is saved
    (close-if-file)

    ;; check whether the simulator computes correct ciphertext
    ;; ciphertext is in the registers r0-r15
    (for ([i 16]
          [correct-ct-byte correct-ct])
      (define computed-ct-byte (sram-get-byte i))
      (define msg (~a "ciphertext byte " i " is not correct. "
                      "Expected: " (num->hexb correct-ct-byte)
                      ", got: " (num->hexb computed-ct-byte)))
      (check-equal? computed-ct-byte correct-ct-byte msg))
    ;; save I/O and the number of leakage data points
    (when leakage-file
      (write-header leakage-file key pt correct-ct
                    INTERMEDIATE-VALUES-INDEX)
      ;; save the leakage
      (write-intermediate-values-bytes leakage-file))
    ;; reset everything, but keep the flah
    (reset-machine #:keep-flash? #t))
  (close-output-port leakage-file)
  (define stop-ms (current-milliseconds))
  (printf "finished in ~a ms~n" (- stop-ms start-ms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make a fixed key in a closure
(define key-proc
  (let ([key (list->bytes (build-list 16 (lambda _ (random 256))))])
    (lambda ()
      key)))

;; make random plaintext
(define (pt-proc)
  (list->bytes (build-list 16 (lambda _ (random 256)))))

(define execution-trace-name "execution-trace.txt")
(define leakage-trace-name "leakage-trace.txt")
(define N 1000)

;; takes about 18 seconds on my machine
(run-example N key-proc pt-proc
             #:exec-trace-name execution-trace-name
             #:leakage-file-name leakage-trace-name)
