(require racket racket/main rackunit racket/system
         (planet vyzo/crypto))

(require (file "../../tests/utilities.rkt"))

;; compile the code before simulating it
(make)

;; TODO: use load for now, change later to "require"
(load "../../atmega163-simulator.rkt")
(load "../../instruction-table.rkt")
(load "../../procedures.rkt")

(reset-machine)
(load-symbol-table "main.sim")
(hex->flash! "main.hex")
(flash->procedures!)

;; make random key, plaintext, compute the result using the 
;; crypto library
(define key (list->bytes (build-list 16 (lambda _ (random 256)))))
(define pt  (list->bytes (build-list 16 (lambda _ (random 256)))))
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
(set! debug? #f) ;; turn it on to see the trace
(go-address (lookup-symbol "encrypted"))

;; check whether the simulator computes correct ciphertext
;; ciphertext is in the registers r0-r15
(for ([i 16]
      [correct-ct-byte correct-ct])
  (define computed-ct-byte (sram-get-byte i))
  (define msg (~a "ciphertext byte " i " is not correct. "
                  "Expected: " (num->hexb correct-ct-byte)
                  ", got: " (num->hexb computed-ct-byte)))
  (check-equal? computed-ct-byte correct-ct-byte msg))

