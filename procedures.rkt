;; 2 bit register id (R24 R26 R28 R30)
(define mask-Rd-2 #x0030)
;; 3 bit register id (R16 - R23)
(define mask-Rd-3 #x0070)
;; 4 bit register id (R16 - R31)
(define mask-Rd-4 #x00f0)
;; 5 bit register id (R00 - R31)
(define mask-Rd-5 #x01f0)
;; 3 bit register id (R16 - R23)
(define mask-Rr-3 #x0007)
;; 4 bit register id (R16 - R31)
(define mask-Rr-4 #x000f)
;; 5 bit register id (R00 - R31)
(define mask-Rr-5 #x020f)
;; for 8 bit constant
(define mask-K-8 #x0F0F)
;; for 6 bit constant
(define mask-K-6 #x00CF)
;; for 7 bit relative address
(define mask-k-7 #x03F8)
;; for 12 bit relative address
(define mask-k-12 #x0FFF)
;; for 22 bit absolute address
(define mask-k-22 #x01F1)
;; register bit select
(define mask-reg-bit #x0007)
;; status register bit select
(define mask-sreg-bit #x0070)
;; address displacement (q)
(define mask-q-displ #x2C07)
;; 5 bit register id (R00 - R31)
(define mask-A-5 #x00F8)
;; 6 bit IO port id
(define mask-A-6 #x060F)

(define (make-opcode-info opcode name proc cycles 32-bit? args)
  (list opcode name proc cycles 32-bit? args))

(define (opcode-info-opcode opcode-info)
  (list-ref opcode-info 0))

(define (opcode-info-name opcode-info)
  (list-ref opcode-info 1))

(define (opcode-info-proc opcode-info)
  (list-ref opcode-info 2))

(define (opcode-info-cycles opcode-info)
  (list-ref opcode-info 3))

(define (opcode-info-32-bit? opcode-info)
  (list-ref opcode-info 4))

(define (opcode-info-args opcode-info)
  (list-ref opcode-info 5))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 16-bit instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (avr-NOP . args) avr-NOP)
(define (avr-ADC Rd Rr)
  (define Rd-val (sram-get-byte Rd))
  (define Rr-val (sram-get-byte Rr))
  (define C (sr-get-C))
  (define R (& (+ Rd-val Rr-val C) #xff))
  (sram-set-byte Rd-val R)
  (compute-C Rd-val Rr-val R 7)
  (compute-Z R)
  (compute-H R 3)
  (compute-N R 7)
  (compute-V Rd-val Rr-val R 7)
  (compute-S)
  (copmute-H Rd-val Rr-val R 3)
  (when debug? 
    (print-instruction-uniquely OUT 'ADC)
    (fprintf OUT "ADC R~a[~a],R~a[~a],C[~a] ; ~a" 
             Rd (num->hexb Rd-val) 
             Rr (num->hexb Rr-val)
             C (num->hexb R)))
  (set! clock-cycles 1))



  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 32-bit instructions
;; either get the second half, when saving
;; or execute, when second arg is empty
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (avr-CALL-get-args kh kl)
  (printf "CALL (~a), (~a)~n" kh kl)
  (list avr-CALL (list (ior (<< kh 16) kl))))
(define (avr-CALL k)
  (cons avr-CALL k))

(define (avr-JMP-get-args kh kl)
  (list avr-JMP (list (+ (<< kh 16) kl))))
(define (avr-JMP k)
  (cons avr-JMP k))

(define (avr-LDS-get-args Rd k)
  (list avr-LDS (list Rd k)))
(define (avr-LDS Rd k)
  (list avr-LDS Rd k))

(define (avr-STS-get-args Rr k)
  (list avr-STS (list Rr k)))
(define (avr-STS Rr k)
  (list avr-STS Rr k))

;; opcode, name, proc, nof-cycles, 32-bit?
(define opcodes-no-operands
  (make-hash
   (list
    ;; opcodes with no operands
    (list #x9598 'BREAK  'avr-BREAK  2 #f)
    (list #x9519 'EICALL 'avr-EICALL 2 #f)
    (list #x9419 'EIJMP  'avr-EIJMP  2 #f)
    (list #x95D8 'ELPM   'avr-ELPM   2 #f)
    (list #x95F8 'ESPM   'avr-ESPM   2 #f)
    (list #x9509 'ICALL  'avr-ICALL  2 #f)
    (list #x9409 'IJMP   'avr-IJMP   2 #f)
    (list #x95C8 'LPM    'avr-LPM    2 #f)
    (list #x0000 'NOP    avr-NOP     2 #f)
    (list #x9508 'RET    'avr-RET    2 #f)
    (list #x9518 'RETI   'avr-RETI   2 #f)
    (list #x9588 'SLEEP  'avr-SLEEP  2 #f)
    (list #x95E8 'SPM    'avr-SPM    2 #f)
    (list #x95A8 'WDR    'avr-WDR    2 #f))))
;; opcodes with two 5-bit registers Rd and Rr
(define opcodes-5-bit-Rd-Rr
  (make-hash
   (list
    (list #x1C00 'ADC avr-ADC 2 #f)
    (list #x0C00 'ADD 'avr-ADD 2 #f)
    (list #x2000 'AND 'avr-AND 2 #f)
    (list #x1400 'CP 'avr-CP 2 #f)
    (list #x0400 'CPC 'avr-CPC 2 #f)
    (list #x1000 'CPSE 'avr-CPSE 2 #f)
    (list #x2400 'EOR 'avr-EOR 2 #f)
    (list #x2C00 'MOV 'avr-MOV 2 #f)
    (list #x9C00 'MUL 'avr-MUL 2 #f)
    (list #x2800 'OR 'avr-OR 2 #f)
    (list #x0800 'SBC 'avr-SBC 2 #f)
    (list #x1800 'SUB 'avr-SUB 2 #f))))
;; opcode with a single register Rd as operand
(define opcodes-Rd
  (make-hash
   (list
    (list #x9405 'ASR 'avr-ASR 2 #f)
    (list #x9400 'COM 'avr-COM 2 #f)
    (list #x940A 'DEC 'avr-DEC 2 #f)
    (list #x9006 'ELPM-Z 'avr-ELPM-Z 2 #f)
    (list #x9007 'ELPM-Z-incr 'avr-ELPM-Z-incr 2 #f)
    (list #x9403 'INC 'avr-INC 2 #f)
    (list #x9000 'LDS avr-LDS-get-args 2 #t)
    (list #x900C 'LD-X 'avr-LD-X 2 #f)
    (list #x900E 'LD-X-decr 'avr-LD-X-decr 2 #f)
    (list #x900D 'LD-X-incr 'avr-LD-X-incr 2 #f)
    (list #x900A 'LD-Y-decr 'avr-LD-Y-decr 2 #f)
    (list #x9009 'LD-Y-incr 'avr-LD-Y-incr 2 #f)
    (list #x9002 'LD-Z-decr 'avr-LD-Z-decr 2 #f)
    (list #x9001 'LD-Z-incr 'avr-LD-Z-incr 2 #f)
    (list #x9004 'LPM-Z 'avr-LPM-Z 2 #f)
    (list #x9005 'LPM-Z-incr 'avr-LPM-Z-incr 2 #f)
    (list #x9406 'LSR 'avr-LSR 2 #f)
    (list #x9401 'NEG 'avr-NEG 2 #f)
    (list #x900F 'POP 'avr-POP 2 #f)
    (list #x920F 'PUSH 'avr-PUSH 2 #f)
    (list #x9407 'ROR 'avr-ROR 2 #f)
    (list #x9200 'STS avr-STS-get-args 2 #t)
    (list #x920C 'ST-X 'avr-ST-X 2 #f)
    (list #x920E 'ST-X-decr 'avr-ST-X-decr 2 #f)
    (list #x920D 'ST-X-incr 'avr-ST-X-incr 2 #f)
    (list #x920A 'ST-Y-decr 'avr-ST-Y-decr 2 #f)
    (list #x9209 'ST-Y-incr 'avr-ST-Y-incr 2 #f)
    (list #x9202 'ST-Z-decr 'avr-ST-Z-decr 2 #f)
    (list #x9201 'ST-Z-incr 'avr-ST-Z-incr 2 #f)
    (list #x9402 'SWAP 'avr-SWAP 2 #f))))
;; opcodes with a register Rd and a constant data K
(define opcodes-Rd-K
  (make-hash
   (list
    (list #x7000 'ANDI 'avr-ANDI 2 #f)
    (list #x3000 'CPI 'avr-CPI 2 #f)
    (list #xE000 'LDI 'avr-LDI 2 #f)
    (list #x6000 'ORI 'avr-ORI 2 #f)
    (list #x4000 'SBCI 'avr-SBCI 2 #f)
    (list #x5000 'SUBI 'avr-SUBI 2 #f)
    (list #xF800 'BLD 'avr-BLD 2 #f))))
;; opcodes with a register Rd and a register bit number b
(define opcodes-Rd-b
  (make-hash
   (list
    (list #xFA00 'BST 'avr-BST 2 #f)
    (list #xFC00 'SBRC 'avr-SBRC 2 #f)
    (list #xFE00 'SBRS 'avr-SBRS 2 #f))))
;; opcodes with a relative 7-bit address k and a register bit number b
(define opcodes-7-bit-k-b
  (make-hash
   (list
    (list #xF400 'BRBC 'avr-BRBC 2 #f)
    (list #xF000 'BRBS 'avr-BRBS 2 #f))))
;; opcodes with a 6-bit address displacement q and a register Rd
(define opcodes-6-bit-q-Rd
  (make-hash
   (list
    (list #x8008 'LDD-Y 'avr-LDD-Y 2 #f)
    (list #x8000 'LDD-Z 'avr-LDD-Z 2 #f)
    (list #x8208 'STD-Y 'avr-STD-Y 2 #f)
    (list #x8200 'STD-Z 'avr-STD-Z 2 #f))))
;; opcodes with a absolute 22-bit address k
(define opcodes-22-bit-k
  (make-hash
   (list
    (list #x940E 'CALL avr-CALL-get-args 2 #t)
    (list #x940C 'JMP avr-JMP-get-args 3 #t))))
;; opcode with a sreg bit select s operand
(define opcodes-s
  (make-hash
   (list
    (list #x9488 'BCLR 'avr-BCLR 2 #f)
    (list #x9408 'BSET 'avr-BSET 2 #f))))
;; opcodes with a 6-bit constant K and a register Rd
(define opcodes-6-bit-K-Rd
  (make-hash
   (list
    (list #x9600 'ADIW 'avr-ADIW 2 #f)
    (list #x9700 'SBIW 'avr-SBIW 2 #f))))
;; opcodes with a 5-bit IO Addr A and register bit number b
(define opcodes-5-bit-A-b
  (make-hash
   (list
    (list #x9800 'CBI 'avr-CBI 2 #f)
    (list #x9A00 'SBI 'avr-SBI 2 #f)
    (list #x9900 'SBIC 'avr-SBIC 2 #f)
    (list #x9B00 'SBIS 'avr-SBIS 2 #f))))
;; opcodes with a 6-bit IO Addr A and register Rd
(define opcodes-6-bit-A-Rd
  (make-hash
   (list
    (list #xB000 'IN 'avr-IN 2 #f)
    (list #xB800 'OUT 'avr-OUT 2 #f))))
;; opcodes with a relative 12-bit address k
(define opcodes-12-bit-k
  (make-hash
   (list
    (list #xD000 'RCALL 'avr-RCALL 2 #f)
    (list #xC000 'RJMP 'avr-RJMP 2 #f))))
;; opcodes with two 4-bit register Rd and Rr
(define opcodes-4-bit-Rd-Rr
  (make-hash
   (list
    (list #x0100 'MOVW 'avr-MOVW 2 #f)
    (list #x0200 'MULS 'avr-MULS 2 #f))))
;; opcodes with two 3-bit register Rd and Rr
(define opcodes-3-bit-Rd-Rr
  (make-hash
   (list
    (list #x0300 'MULSU 'avr-MULSU 2 #f)
    (list #x0308 'FMUL 'avr-FMUL 2 #f)
    (list #x0380 'FMULS 'avr-FMULS 2 #f)
    (list #x0388 'FMULSU 'avr-FMULSU 2 #f))))

(define (lookup-opcode a-hash opcode)
  (hash-ref a-hash opcode #f))

(define (get-arg opcode mask (bit-length 16))
  (define mask-opcode (& opcode mask))
  (define target-i 0)
  (define result 0)
  (for ([i (min (integer-length mask-opcode))])
    (when (bitwise-bit-set? mask i)
      (when (bitwise-bit-set? opcode i)
        (set! result (ior result (<< 1 target-i))))
      (set! target-i (+ target-i 1))))
  result)


(define tables-and-masks
  (list
   (list opcodes-no-operands 0)
   (list opcodes-5-bit-Rd-Rr mask-Rd-5 mask-Rr-5)
   (list opcodes-Rd mask-Rd-5)
   (list opcodes-Rd-K mask-Rd-4 mask-K-8)
   (list opcodes-Rd-b mask-Rd-5 mask-reg-bit)
   (list opcodes-7-bit-k-b mask-k-7 mask-reg-bit)
   (list opcodes-6-bit-q-Rd mask-Rd-5 mask-q-displ)
   (list opcodes-22-bit-k mask-k-22)
   (list opcodes-s mask-sreg-bit)
   (list opcodes-6-bit-K-Rd mask-K-6 mask-Rd-2)
   (list opcodes-5-bit-A-b mask-A-5 mask-reg-bit)
   (list opcodes-6-bit-A-Rd mask-A-6 mask-Rd-5)
   (list opcodes-12-bit-k mask-k-12)
   (list opcodes-4-bit-Rd-Rr mask-Rd-4 mask-Rr-4)
   (list opcodes-3-bit-Rd-Rr mask-Rd-3 mask-Rr-3)))

(define (get-masks tables-and-masks-entry)
  (cdr tables-and-masks-entry))

(define (get-table tables-and-masks-entry)
  (car tables-and-masks-entry))

(define (get-args opcode masks)
  (map (lambda (mask) (get-arg opcode mask)) masks))

;; map opcode to instruction procedure
(define (decode opcode)
  (let loop ([tables-and-masks tables-and-masks])
    (if (empty? tables-and-masks)
        #f ;; opcode unknown (probably data, or 32-bit instr)
        (let ([mapping (car tables-and-masks)])
          (define masks (get-masks mapping))
          (define table (get-table mapping))
          (define mask (bitwise-not (apply ior masks)))
          (define opcode-info (lookup-opcode (get-table mapping)
                                             (& opcode mask)))
          (define args (get-args opcode masks))
          (if opcode-info
              (append (cons opcode opcode-info) (list args))
              (loop (cdr tables-and-masks)))))))

;; convert the flash into prepared procedures
;; make sure to get args of 32-bit opcodes
(define (flash->procedures flash)
  ;; vector of procedures with same size
  (define procedures (make-vector FLASHEND #f))
  (define was-32-bit? #f) ;; will ignore the word when "yes"
  (let loop ([addr 0])
    (unless (>= addr FLASHEND)
      (define opcode-info (decode (flash-get-word addr)))
      (if (and (not was-32-bit?) opcode-info)
          (let ([opcode  (opcode-info-opcode opcode-info)]
                [name    (opcode-info-name opcode-info)]
                [proc    (opcode-info-proc opcode-info)]
                [cycles  (opcode-info-cycles opcode-info)]
                [32-bit? (opcode-info-32-bit? opcode-info)]
                [args    (opcode-info-args opcode-info)])
            ;; 32-bit opcodes need to lookup the next word
            (when (opcode-info-32-bit? opcode-info)
              (let ([next-word (flash-get-word (+ addr 1))])
                (set! was-32-bit? #t)
                (set! opcode (+ (<< opcode 16) next-word))
                (define proc-args 
                  (apply proc (append args (list next-word))))
                (set! proc (car proc-args))
                (set! args (cadr proc-args))))
            ;; copy 
            (define result
              (make-opcode-info opcode name 
                                proc cycles 32-bit? args))
            (vector-set! procedures addr result))
          (set! was-32-bit? #f))
      (loop (+ addr 1))))
  procedures)

(define (print-flash-procedures procs)
  (for ([opcode-info procs]
        [i (vector-length procs)])
    (when opcode-info
      (define opcode  (opcode-info-opcode  opcode-info))
      (define name    (opcode-info-name    opcode-info))
      (define proc    (opcode-info-proc    opcode-info))
      (define cycles  (opcode-info-cycles  opcode-info))
      (define 32-bit? (opcode-info-32-bit? opcode-info))
      (define args    (opcode-info-args    opcode-info))
      (printf "~a: ~a ~a ~a ~a ~a ~a~n" 
              (num->hex i) 
              (num->hex opcode)
              name proc cycles 32-bit?
              (map num->hex args)))
    (unless opcode-info
      (printf "~a: #f~n" i))))


;; small test
(define hex-file "./tests/main.hex")
(reset-machine)
(hex->flash! hex-file)
(print-flash-procedures all-procs)
(define all-procs (flash->procedures FLASH))

;; tests
(check eq? 'BREAK (cadr (decode #x9598)))
(check eq? 'EICALL (cadr (decode #x9519)))
(check eq? 'EIJMP (cadr (decode #x9419)))
(check eq? 'ELPM (cadr (decode #x95D8)))
(check eq? 'ESPM (cadr (decode #x95F8)))
(check eq? 'ICALL (cadr (decode #x9509)))
(check eq? 'IJMP (cadr (decode #x9409)))
(check eq? 'LPM (cadr (decode #x95C8)))
(check eq? 'NOP (cadr (decode #x0000)))
(check eq? 'RET (cadr (decode #x9508)))
(check eq? 'RETI (cadr (decode #x9518)))
(check eq? 'SLEEP (cadr (decode #x9588)))
(check eq? 'SPM (cadr (decode #x95E8)))
(check eq? 'WDR (cadr (decode #x95A8)))

(check eq? 'ADC (cadr (decode #x1C15)))
(check eq? 'ADD (cadr (decode #x0C12)))
(check eq? 'AND (cadr (decode #x2055)))
(check eq? 'CP (cadr (decode #x1455)))
(check eq? 'CPSE (cadr (decode #x1055)))
(check eq? 'EOR (cadr (decode #x24ff)))

(check eq? 'ASR (cadr (decode #x9495)))
(check eq? 'COM (cadr (decode #x94f0)))
(check eq? 'ST-X (cadr (decode #x92fC)))
(check eq? 'ROR (cadr (decode #x94f7)))

(check eq? 'ANDI (cadr (decode #x7fff)))
(check eq? 'CPI (cadr (decode #x3fff)))
(check eq? 'CALL (cadr (decode #x95ff)))

