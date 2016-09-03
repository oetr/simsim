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
(define (prepare-machine label (set-debug? #f))
  ;; Get the symbol table
  (define symbol-file "main.sim")
  (load-symbol-table symbol-file)
  ;; Load the hex file
  (define hex-file "main.hex")
  (set! debug? set-debug?)
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
(run 2)
(for ([i 256])
  (step-and-compare-reg-content "startTestSTXRr" (+ #x60 i) 
                                (vector-ref sbox i) 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MOV
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prepare-machine "testMOV0")
(for ([i 16])
  (set-register (+ i 16) i))
(for ([i 16])
  (step-and-compare-reg-content
   (string-append "MOV r"
                  (number->string i)
                  ", r"
                  (number->string (+ i 16)))
   i i))

(prepare-machine "testMOV1")
(for ([i 16])
  (set-register (+ i 16) i))
(for ([i 16])
  (step-and-compare-reg-content
   (string-append "MOV r"
                  (number->string (modulo (+ i 1) 16))
                  ", r"
                  (number->string (+ i 16)))
   (modulo (+ i 1) 16) i))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test jumps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prepare-machine "testJUMPS")
(run 1)
(check-equal? PC (lookup-symbol "main") "testJUMPS main")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test branches and 32-bit instructions afterwards
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prepare-machine "test32BitInstructions" #t)
(set-register 30 0)
(set-register 31 0)
(define saved-pc PC)
(run 1)
(check-equal? PC (+ saved-pc 3) "CPSE followed by 32-bit insruction")


