(require racket racket/main rackunit)
(require "data.rkt")

;; TODO: use load for now, change later to "require"
(load "../atmega163-simulator.rkt")
(load "../instruction-table.rkt")
(load "../procedures.rkt")

;; Helper procedures
(define (step-and-compare-reg-content test-label reg val (n-instr 1))
  (run n-instr)
  (check-equal? (sram-get-byte reg) val test-label))

;; reset the machine, load everything, 
;; and let the init code execute
(define (prepare-machine label)
  ;; Get the symbol table
  (define symbol-file "main.sim")
  (load-symbol-table symbol-file)
  ;; Load the hex file
  (define hex-file "main.hex")
  (set! debug? #t)
  (reset-machine)
  (hex->flash! hex-file)
  (flash->procedures!)
  (set! PC (lookup-symbol "main"))
  (go-address (lookup-symbol "init"))
  (set! PC (lookup-symbol label)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LDI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prepare-machine "startTestLDI")
(for ([i 16])
  (step-and-compare-reg-content
   (string-append "LDI-r16-1-" 
                  (number->string i)) 16 i))
(for ([i 16])
  (step-and-compare-reg-content
   (string-append "LDI-r16-2-" 
                  (number->string i)) 16 (ior #xf0 i)))
(for ([i 16])
  (step-and-compare-reg-content
   (string-append "LDI-r17-1-" 
                  (number->string i)) 17 (ior #xc0 i)))
(for ([reg (range 18 32)])
  (step-and-compare-reg-content
   (string-append "LDI-r" (number->string reg) "-ff-") reg #xff))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LPM RXX,Z for registers R0-R29
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(for ([reg 30])
  (prepare-machine (string-append "startTestLPMR" 
                                  (number->string reg)))
  (run 2)
  (for ([i 256])
    (step-and-compare-reg-content
     (string-append "LPM-" (number->string reg) 
                    "-sbox-" (number->string i))
     reg
     (vector-ref sbox i))
    (run 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LPM RXX,Z+ for registers R0-R29
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(for ([reg 30])
  (prepare-machine (string-append "startTestLPMZplusR" 
                                  (number->string reg)))
  (run 2)
  (for ([i 256])
    (step-and-compare-reg-content
     (string-append "LPM-Z+-" (number->string reg) 
                    "-sbox-" (number->string i))
     reg
     (vector-ref sbox i))
    (run)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ST X,Rr
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prepare-machine "startTestSTXRr")
(set! debug? #f)
(run 2)
(for ([i 256])
  (step-and-compare-reg-content "startTestSTXRr" (+ #x60 i) 
                                (vector-ref sbox i) 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ST X+,Rr
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prepare-machine "startTestSTXRr")


(prepare-machine "cipher_e_xfrm")
(set! debug? #t)
(set-register 28 #xff)
(go-address (lookup-symbol "cipher_e_xfrm_end"))
