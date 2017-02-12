(define PROCEDURES #f)
(define clock-cycles 0)
(define symbol-need-to-print? #t)

(define save-intermediate-values? #f)
(define save-hamming-distance? #f)
(define average-values? #f)
;; Intermediate values
(define NOF-INTERMEDIATE-VALUES 3)
(define INTERMEDIATE-VALUES-N 800000)
;; Data types
(define DOUBLE-BYTES 8)
(define INT-BYTES 2)
(define INTERMEDIATE-VALUES (make-vector INTERMEDIATE-VALUES-N))
(define INTERMEDIATE-VALUES-INDEX 0)
(define SAVED-PC 0)

(define *saved-value-data* (make-bytes
                            (* INTERMEDIATE-VALUES-N DOUBLE-BYTES)))
(define *saved-value-pc* (make-bytes
                          (* INTERMEDIATE-VALUES-N INT-BYTES)))
(define *saved-value-cc* (make-bytes
                          (* INTERMEDIATE-VALUES-N INT-BYTES)))

(struct saved-value (data pc cc))

(define (subvector v from to)
  (vector-drop (vector-take v to) from))

(define (uint->bytes uint n-bytes)
  ;; check: make sure n-bytes is big enough
  (when (> (integer-length uint) (* n-bytes 8))
    (error
     'uint->bytes
     "Provided integer ~a is bigger than requested byte width ~a~n"
     uint n-bytes))
  (define num uint)
  (define result (make-bytes n-bytes))
  ;; write MSB first, process LSB first
  (for ([i n-bytes])
    (bytes-set! result (- n-bytes i 1) (bitwise-and num #xFF))
    (set! num (arithmetic-shift num -8)))
  result)

(define *accum-count* 0.0)
(define *accum-data* 0.0)
(define *prev-accum-data* 0.0)

(define (divide-intervals start stop n)
  (if (= start stop)
      (make-list n stop)
      (let ([step (/ (- stop start) n 1.0)])
        (range (+ start step) (+ stop step (- (/ step 10.0))) step))))

(define (save-values! pc cc-prev cc)
  (when (zero? *accum-count*)
    (set! *accum-count* 1.0))
  (define interpolated-data (divide-intervals
                             *prev-accum-data*
                             (/ *accum-data* *accum-count*)
                             (- cc cc-prev)))
  (define interpolated-cc (range (+ cc-prev 1) (+ cc 1)))
  (define interpolated-pc (make-list (- cc cc-prev) pc))
  (when (or (not (= (length interpolated-data)
                    (length interpolated-cc)))
            (not (= (length interpolated-data)
                    (length interpolated-pc)))
            (not (= (length interpolated-cc)
                    (length interpolated-pc))))
    (printf "data: ~a~npc: ~a~ncc: ~a~n"
            interpolated-data
            interpolated-pc
            interpolated-cc))
  (for ([data-to-save interpolated-data]
        [pc-to-save interpolated-pc]
        [cc-to-save interpolated-cc])
    (define data-bytes (real->floating-point-bytes data-to-save
                                                   DOUBLE-BYTES))
    (define cc-bytes (uint->bytes cc-to-save INT-BYTES))
    (define pc-bytes (uint->bytes pc-to-save INT-BYTES))
    (bytes-copy! *saved-value-data*
                 (* INTERMEDIATE-VALUES-INDEX DOUBLE-BYTES)
                 data-bytes)
    (bytes-copy! *saved-value-pc*
                 (* INTERMEDIATE-VALUES-INDEX INT-BYTES) pc-bytes)
    (bytes-copy! *saved-value-cc*
                 (* INTERMEDIATE-VALUES-INDEX INT-BYTES) cc-bytes)
    (set! INTERMEDIATE-VALUES-INDEX (+ INTERMEDIATE-VALUES-INDEX 1)))
  ;; update accumulated value
  (set! *prev-accum-data* (/ *accum-data* *accum-count*))
  (set! *accum-data* 0.0)
  (set! *accum-count* 0.0))

(define (accumulate-values! data)
  (set! *accum-data* (+ *accum-data* data))
  (set! *accum-count* (+ *accum-count* 1.0)))

(define (save-intermediate-values data)
  (when save-intermediate-values?
    (accumulate-values! (hamming-weight data))
    (unless (eq? CURRENT-CLOCK-CYCLE PREVIOUS-CLOCK-CYCLE)
      (define instr (vector-ref PROCEDURES PC))
      (when instr
        (set! SAVED-PC (- PC 1))))
    (set! PREVIOUS-CLOCK-CYCLE CURRENT-CLOCK-CYCLE)))

(define (print-with-separator a-file fn (sep ","))
  (for ([i (range 0 INTERMEDIATE-VALUES-INDEX)])
    (fprintf a-file "~a"
             (fn (INTERMEDIATE-VALUES-INDEX
                  (vector-ref INTERMEDIATE-VALUES i))))
    (when (< i (- INTERMEDIATE-VALUES-INDEX 1))
      (fprintf a-file "~a" sep)))
  (fprintf a-file "~n"))

(define (write-intermediate-values a-file #:exists (exists 'append))
  (print-with-separator a-file saved-value-data)
  (print-with-separator a-file saved-value-pc)
  (print-with-separator a-file saved-value-cc))

(define (intermediate-values->file file-name
                                   #:exists (exists 'append))
  (define a-file (open-output-file (expand-user-path file-name)
                                   #:exists exists
                                   #:mode 'binary))
  (write-intermediate-values a-file #:exists exists #:mode 'binary)
  (close-output-port a-file))

(define (write-header a-file . args)
  (for ([num args]) (write-bytes (uint->bytes num 4) a-file)))

(define (write-intermediate-values-bytes a-file)
  (write-bytes *saved-value-data* a-file 0
               (* INTERMEDIATE-VALUES-INDEX DOUBLE-BYTES))
  (write-bytes *saved-value-pc* a-file 0
               (* INTERMEDIATE-VALUES-INDEX INT-BYTES))
  (write-bytes *saved-value-cc* a-file 0
               (* INTERMEDIATE-VALUES-INDEX INT-BYTES)))

(define (intermediate-value-bytes->file a-file
                                        #:exists (exists 'append))
  (define a-file (open-output-file (expand-user-path file-name)
                                   #:exists exists
                                   #:mode 'binary))
  (print-bytes a-file)
  (close-output-port a-file))

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

(struct opcode-info (opcode name proc cycles 32-bit? args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 16-bit instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (avr-IJMP . _)
  (define z (get-z))
  (set-pc! z)
  (when debug?
    (print-instruction-uniquely OUT 'IJMP)
    (fprintf OUT "IJMP Z[~a]" z))
  (set! clock-cycles 2))

(define (avr-NOP . _)
  (when debug?
    (print-instruction-uniquely OUT 'NOP)
    (fprintf OUT "NOP"))
  (set! clock-cycles 1))

(define (avr-ADC Rd Rr)
  (define Rd-val (get-register Rd))
  (define Rr-val (get-register Rr))
  (define C (sr-get-C))
  (define R (& (+ Rd-val Rr-val C) #xff))
  (set-register Rd R)
  (compute-C-add Rd-val Rr-val R 7)
  (compute-Z R)
  (compute-N R 7)
  (compute-V Rd-val Rr-val R 7)
  (compute-S)
  (compute-H Rd-val Rr-val R 3)
  (when debug?
    (print-instruction-uniquely OUT 'ADC)
    (fprintf OUT "ADC R~a[~a],R~a[~a],C[~a] ; ~a"
             Rd (num->hexb Rd-val)
             Rr (num->hexb Rr-val)
             C (num->hexb R)))
  (set! clock-cycles 1))

(define (avr-ADD Rd Rr)
  (define Rd-val (get-register Rd))
  (define Rr-val (get-register Rr))
  (define R (& (+ Rd-val Rr-val) #xff))
  (set-register Rd R)
  (compute-Z R)
  (compute-C-add Rd-val Rr-val R 7)
  (compute-N R 7)
  (compute-V Rd-val Rr-val R 7)
  (compute-S)
  (compute-H Rd-val Rr-val R 3)
  (when debug?
    (print-instruction-uniquely OUT 'ADD)
    (fprintf OUT "ADD R~a[~a],R~a[~a] ; ~a"
             Rd (num->hexb Rd-val)
             Rr (num->hexb Rr-val)
             (num->hexb R)))
  (set! clock-cycles 1))

(define (avr-AND Rd Rr)
  (define Rd-val (get-register Rd))
  (define Rr-val (get-register Rr))
  (define R (& Rd-val Rr-val))
  (set-register Rd R)
  (sr-clear-V)
  (compute-N R 7)
  (compute-S)
  (compute-Z R)
  (when debug?
    (print-instruction-uniquely OUT 'AND)
    (fprintf OUT "AND R~a[~a],R~a[~a]"
             Rd (num->hex Rd-val) Rr (num->hex Rr-val)))
  (set! clock-cycles 1))


(define (avr-ADIW K Rd-2-bits)
  (define Rd (+ 24 (* Rd-2-bits 2)))
  (define Rd+ (+ Rd 1))
  (define Rdh (get-register Rd+))
  (define Rdl (get-register Rd))
  (define Rd-val (ior (<< Rdh 8) Rdl))
  (define R (& (+ Rd-val K) #xffff))
  (set-register Rd+ (<< R -8))
  (set-register Rd (& R #xff))
  (if (zero? R) (sr-set-Z) (sr-clear-Z))
  (if (one? (& (n-bit-ref R 15)
               (bit-ref Rdh 7)))
      (sr-set-C) (sr-clear-C))
  (if (! R 15) (sr-set-N) (sr-clear-N))
  (if (one? (& (bit-ref R 15)
               (n-bit-ref Rdh 7)))
      (sr-set-V) (sr-clear-V))
  (if (one? (bitwise-xor (sr-get-N) (sr-get-V)))
      (sr-set-S) (sr-clear-S))
  (when debug?
    (print-instruction-uniquely OUT 'ADIW)
    (fprintf OUT "ADIW R~a:R~a[~a],K[~a] ; ~a"
             Rd Rd+ (num->hex Rd-val) (num->hex K) (num->hex R)))
  (set! clock-cycles 2))

(define (avr-OR Rd Rr)
  (define Rd-val (get-register Rd))
  (define Rr-val (get-register Rr))
  (define R (ior Rd-val Rr-val))
  (set-register Rd R)
  (sr-clear-V)
  (compute-N R)
  (compute-S)
  (compute-Z R)
  (when debug?
    (print-instruction-uniquely OUT 'OR)
    (fprintf OUT "OR R~a[~a],R~a[~a] ; ~a"
             Rd (num->hex Rd-val)
             Rr (num->hex Rr-val)
             (num->hex R)))
  (set! clock-cycles 1))


(define (avr-SBC Rd Rr)
  (define Rd-val (get-register Rd))
  (define Rr-val (get-register Rr))
  (define C (sr-get-C))
  (define R (& (- Rd-val Rr-val C) #xff))
  (set-register Rd R)
  (compute-H Rd-val Rr R 3)
  (compute-V Rd-val Rr R 7)
  (compute-N R 7)
  (compute-S)
  (unless (zero? R) (sr-clear-Z))
  (compute-C Rd-val Rr R 7)
  (when debug?
    (print-instruction-uniquely OUT 'SBC)
    (fprintf OUT "SBC R~a[~a],R~a[~a],C[~a] ; ~a"
             Rd (num->hex Rd-val)
             Rr (num->hex Rr-val) C
             (num->hex R)))
  (set! clock-cycles 1))

(define (avr-SUB Rd Rr)
  (define Rd-val (get-register Rd))
  (define Rr-val (get-register Rr))
  (define R (& (- Rd-val Rr-val) #xff))
  (sram-set-byte Rd R)
  (compute-H Rd-val Rr-val R)
  (compute-V Rd-val Rr-val R)
  (compute-N R)
  (compute-S)
  (compute-Z R)
  (compute-C Rd-val Rr-val R)
  (when debug?
    (print-instruction-uniquely OUT 'SUB)
    (fprintf OUT "SUB R~a[~a],R~a[~a] ; ~a"
             Rd (num->hex Rd-val)
             Rr (num->hex Rr-val)
             (num->hex R)))
  (set! clock-cycles 1))

(define (avr-ANDI Rd-16 K)
  (define Rd (+ 16 Rd-16))
  (define Rd-val (get-register Rd))
  (define R (& Rd-val K))
  (set-register Rd R)
  (sr-clear-V)
  (compute-N R)
  (compute-S)
  (compute-Z R)
  (when debug?
    (print-instruction-uniquely OUT 'ANDI)
    (fprintf OUT "ANDI R~a[~a],K[~a] ; ~a"
             Rd (num->hex Rd-val)
             (num->hex K)
             (num->hex R)))
  (set! clock-cycles 1))

(define (avr-BCLR s)
  (sr-clear-bit s)
  (when debug?
    (print-instruction-uniquely OUT 'BCLR)
    (fprintf OUT "BCLR ~a" s))
  (set! clock-cycles 1))

(define (avr-BRBC k-num b)
  (define k (2-complement->num 7 k-num))
  (define b-val (sr-get-bit b))
  (when (zero? b-val)
    (set-pc! (+ PC k))
    (set! clock-cycles (+ clock-cycles 1)))
  (set! clock-cycles (+ clock-cycles 1))
  (when debug?
    (print-instruction-uniquely OUT 'BRBC clock-cycles)
    (fprintf OUT "BRBC ~a[~a], ~a ; ~a" b b-val k (zero? b-val))))

(define (avr-BRBS k-num b)
  (define k (2-complement->num 7 k-num))
  (define b-val (sr-get-bit b))
  (when (one? b-val)
    (set-pc! (+ PC k))
    (set! clock-cycles (+ clock-cycles 1)))
  (set! clock-cycles (+ clock-cycles 1))
  (when debug?
    (print-instruction-uniquely OUT 'BRBS clock-cycles)
    (fprintf OUT "BRBS s[~a],k[~a] ; ~a" b-val k (one? b-val))))

(define (avr-BSET s)
  (sr-set-bit s)
  (when debug?
    (print-instruction-uniquely OUT 'BSET)
    (fprintf OUT "BSET"))
  (set! clock-cycles 1))

(define (avr-BST Rd b)
  (define Rd-val (get-register Rd))
  (if (! Rd-val b) (sr-set-T) (sr-clear-T))
  (when debug?
    (print-instruction-uniquely OUT 'BST)
    (fprintf OUT "BST R~a[~a],b[~a] ; ~a"
             Rd (num->hex Rd-val)
             b (sr-get-T)))
  (set! clock-cycles 1))

;; Skip if bit in register is cleared
(define (avr-SBRC Rr b)
  (define Rr-val (get-register Rr))
  (define b-val (bit-ref Rr-val b))
  (when (zero? b-val)
    (define next-instr (vector-ref PROCEDURES PC))
    (when (opcode-info-32-bit? next-instr)
      (inc-pc)
      (set! clock-cycles (+ clock-cycles 1)))
    (inc-pc)
    (set! clock-cycles (+ clock-cycles 1)))
  (set! clock-cycles (+ clock-cycles 1))
  (when debug?
    (print-instruction-uniquely OUT 'SBRC clock-cycles)
    (fprintf OUT "SBRC R~a[~a],b[~a] ; ~a"
             Rr Rr-val b (zero? b-val))))

(define (avr-SBRS Rr b)
  (define Rr-val (get-register Rr))
  (define b-val (bit-ref Rr-val b))
  (when (one? b-val)
    (define next-instr (vector-ref PROCEDURES PC))
    (when (opcode-info-32-bit? next-instr)
      (inc-pc)
      (set! clock-cycles (+ clock-cycles 1)))
    (inc-pc)
    (set! clock-cycles (+ clock-cycles 1)))
  (set! clock-cycles (+ clock-cycles 1))
  (when debug?
    (print-instruction-uniquely OUT 'SBRS clock-cycles)
    (fprintf OUT "SBRS R~a[~a],b[~a] ; ~a"
             Rr Rr-val b (one? b-val))))

(define (avr-CBI A b)
  (io-clear-bit A b)
  (when debug?
    (print-instruction-uniquely OUT 'CBI)
    (fprintf OUT "CBI A[~a],b[~a]" (num->hex A) b))
  (set! clock-cycles 2))

(define (avr-CP Rd Rr)
  (define Rd-val (get-register Rd))
  (define Rr-val (get-register Rr))
  (define R (& (- Rd-val Rr-val) #xff)) ;; result
  (compute-H Rd-val Rr-val R)
  (compute-V Rd-val Rr-val R)
  (compute-N R)
  (compute-S)
  (compute-Z R)
  (compute-C Rd-val Rr-val R)
  (when debug?
    (print-instruction-uniquely OUT 'CP)
    (fprintf OUT "CP R~a[~a],R~a[~a] ; ~a"
             Rd (num->hex Rd-val)
             Rr (num->hex Rr-val)
             (num->hex R)))
  (set! clock-cycles 1))

(define (avr-CPC Rd Rr)
  (define Rd-val (get-register Rd))
  (define Rr-val (get-register Rr))
  (define C (sr-get-C))
  (define R (& (- Rd-val Rr-val C) #xff)) ;; result
  (if (one? (ior (& (n-bit-ref Rd-val 3) (bit-ref Rr-val 3))
                 (& (bit-ref Rr-val 3) (bit-ref R 3))
                 (& (bit-ref R 3) (n-bit-ref Rd-val 3))))
      (sr-set-H) (sr-clear-H))
  (if (one? (ior (& (bit-ref Rd-val 7)
                    (n-bit-ref Rr-val 7)
                    (n-bit-ref R 7))
                 (& (n-bit-ref Rd-val 7)
                    (bit-ref Rr-val 7)
                    (bit-ref R 7))))
      (sr-set-V) (sr-clear-V))
  (if (! R 7) (sr-set-N) (sr-clear-N))
  (if (= (bitwise-xor (sr-get-N) (sr-get-V)) 1)
      (sr-set-S) (sr-clear-S))
  (unless (zero? R) (sr-clear-Z))
  (if (one? (ior (& (n-bit-ref Rd-val 7)
                    (bit-ref Rr-val 7))
                 (& (bit-ref Rr-val 7)
                    (bit-ref R 7))
                 (& (bit-ref R 7)
                    (n-bit-ref Rd-val 7))))
      (sr-set-C) (sr-clear-C))
  (when debug?
    (print-instruction-uniquely OUT 'CPC)
    (fprintf OUT "CPC R~a[~a],R~a[~a],C[~a] ; ~a"
             Rd (num->hex Rd-val)
             Rr (num->hex Rr-val)
             C (num->hex R)))
  (set! clock-cycles 1))

(define (avr-CPI Rd-16 K)
  (define Rd (+ Rd-16 16))
  (define Rd-val (get-register Rd))
  (define result (& (- Rd-val K) #xff))
  (if (one? (ior (& (n-bit-ref Rd-val 3) (bit-ref K 3))
                 (& (bit-ref result 3) (bit-ref K 3))
                 (& (bit-ref result 3) (n-bit-ref Rd-val 3))))
      (sr-set-H) (sr-clear-H))
  (if (one? (ior (& (bit-ref Rd-val 7)
                    (n-bit-ref K 7)
                    (n-bit-ref result 7))
                 (& (n-bit-ref Rd-val 7)
                    (bit-ref K 7)
                    (bit-ref result 7))))
      (sr-set-V) (sr-clear-V))
  (if (! result 7) (sr-set-N) (sr-clear-N))
  (if (= (bitwise-xor (sr-get-N) (sr-get-V)) 1)
      (sr-set-S) (sr-clear-S))
  (if (zero? result) (sr-set-Z) (sr-clear-Z))
  (if (> K Rd-val) (sr-set-C) (sr-clear-C))
  (when debug?
    (print-instruction-uniquely OUT 'CPI)
    (fprintf OUT "CPI R~a[~a],~a ; ~a"
             Rd (num->hex Rd-val) (num->hex K) (num->hex result)))
  (set! clock-cycles 1))


(define (avr-CPSE Rd Rr)
  (define Rd-val (get-register Rd))
  (define Rr-val (get-register Rr))
  (when (= Rd-val Rr-val)
    (define next-instr (vector-ref PROCEDURES PC))
    (when (opcode-info-32-bit? next-instr)
      (inc-pc)
      (set! clock-cycles (+ clock-cycles 1)))
    (inc-pc)
    (set! clock-cycles (+ clock-cycles 1)))
  (set! clock-cycles (+ clock-cycles 1))
  (when debug?
    (print-instruction-uniquely OUT 'CPSE clock-cycles)
    (fprintf OUT "CPSE R~a[~a],R~a[~a] ; ~a"
             Rd (num->hex Rd-val)
             Rr (num->hex Rr-val)
             (= Rd-val Rr-val))))

(define (avr-DEC Rd)
  (define Rd-val (get-register Rd))
  (define R (& (- Rd-val 1) #xff))
  (set-register Rd R)
  (if (= Rd-val #x80) (sr-set-V) (sr-clear-V))
  (compute-N R 7)
  (compute-S)
  (compute-Z R)
  (when debug?
    (print-instruction-uniquely OUT 'DEC)
    (fprintf OUT "DEC R~a[~a] ; ~a"
             Rd (num->hex Rd-val) (num->hex R)))
  (set! clock-cycles 1))

(define (avr-EOR Rd Rr)
  (define Rd-val (get-register Rd))
  (define Rr-val (get-register Rr))
  (define R (bitwise-xor Rd-val Rr-val))
  (set-register Rd R)
  (sr-clear-V)
  (if (! R 7) (sr-set-N) (sr-clear-N))
  (if (one? (bitwise-xor (sr-get-N) (sr-get-V)))
      (sr-set-S) (sr-clear-S))
  (if (zero? R) (sr-set-Z) (sr-clear-Z))
  (when debug?
    (print-instruction-uniquely OUT 'EOR)
    (fprintf OUT "EOR R~a[~a],R~a[~a] ; ~a"
             Rd (num->hex Rd-val)
             Rr (num->hex Rr-val)
             (num->hex R)))
  (set! clock-cycles 1))

(define (avr-INC Rd)
  (define Rd-val (get-register Rd))
  (define R (& (+ Rd-val 1) #xff))
  (set-register Rd R)
  (if (= Rd-val #x7f) (sr-set-V) (sr-clear-V))
  (if (! R 7) (sr-set-N) (sr-clear-N))
  (if (one? (bitwise-xor (sr-get-N) (sr-get-V)))
      (sr-set-S) (sr-clear-S))
  (if (zero? R) (sr-set-Z) (sr-clear-Z))
  (when debug?
    (print-instruction-uniquely OUT 'INC)
    (fprintf OUT "INC R~a ; ~a" Rd (num->hex R)))
  (set! clock-cycles 1))

(define (avr-LD-X Rd)
  (define Rd-val (get-register Rd))
  (define x (get-x))
  (define x-val (sram-get-byte x))
  (set-register Rd x-val)
  (when debug?
    (print-instruction-uniquely OUT 'LDRdX)
    (fprintf OUT "LD R~a[~a],X[~a]"
             Rd (num->hex Rd-val) (num->hex x-val)))
  (set! clock-cycles 2))

(define (avr-LD-X-decr Rd)
  (define Rd-val (get-register Rd))
  (define x (get-x))
  (define x-val (sram-get-byte x))
  (set-register Rd x-val)
  (dec-x)
  (when debug?
    (print-instruction-uniquely OUT 'LDRdX-)
    (fprintf OUT "LD R~a[~a],X-[~a]"
             Rd (num->hex Rd-val)(num->hex x-val)))
  (set! clock-cycles 2))

(define (avr-LD-X-incr Rd)
  (define Rd-val (get-register Rd))
  (define x (get-x))
  (define x-val (sram-get-byte x))
  (set-register Rd x-val)
  (inc-x)
  (when debug?
    (print-instruction-uniquely OUT 'LDRdX+)
    (fprintf OUT "LD R~a[~a],X+[~a]"
             Rd (num->hex Rd-val)(num->hex x-val)))
  (set! clock-cycles 2))

(define (avr-LDD-Y Rd q)
  (define Rd-val (get-register Rd))
  (define y (get-y))
  (define val (sram-get-byte (+ y q)))
  (set-register Rd val)
  (when debug?
    (print-instruction-uniquely OUT 'LDRdY+q)
    (fprintf OUT "LD R~a[~a],Y+~a[~a]"
             Rd (num->hex Rd-val) q (num->hex val)))
  (set! clock-cycles 2))

(define (avr-LD-Y-incr Rd)
  (define Rd-val (get-register Rd))
  (define y (get-y))
  (define y-val (sram-get-byte y))
  (set-register Rd y-val)
  (inc-y)
  (when debug?
    (print-instruction-uniquely OUT 'LDRdY+)
    (fprintf OUT "LD R~a[~a],Y+[~a]"
             Rd (num->hex Rd-val)(num->hex y-val)))
  (set! clock-cycles 2))

(define (avr-LD-Y-decr Rd)
  (define Rd-val (get-register Rd))
  (define y (get-y))
  (define y-val (sram-get-byte y))
  (set-register Rd y-val)
  (dec-y)
  (when debug?
    (print-instruction-uniquely OUT 'LDRdY-)
    (fprintf OUT "LD R~a[~a],Y-[~a]"
             Rd (num->hex Rd-val)(num->hex y-val)))
  (set! clock-cycles 2))

(define (avr-LDD-Z Rd q)
  (define Rd-val (get-register Rd))
  (define z (get-z))
  (define z-val (sram-get-byte (+ z q)))
  (set-register Rd z-val)
  (when debug?
    (print-instruction-uniquely OUT 'LDRdZ+q)
    (fprintf OUT "LD R~a[~a],Z+~a[~a]"
             Rd (num->hex Rd-val) q (num->hex z-val)))
  (set! clock-cycles 2))

(define (avr-LD-Z-decr Rd)
  (define Rd-val (get-register Rd))
  (define z (get-z))
  (define z-val (sram-get-byte z))
  (set-register Rd z-val)
  (dec-z)
  (when debug?
    (print-instruction-uniquely OUT 'LDRdZ-)
    (fprintf OUT "LD R~a[~a],Z-[~a]"
             Rd (num->hex Rd-val)(num->hex z-val)))
  (set! clock-cycles 2))

(define (avr-LD-Z-incr Rd)
  (define Rd-val (get-register Rd))
  (define z (get-z))
  (define z-val (sram-get-byte z))
  (set-register Rd z-val)
  (inc-z)
  (when debug?
    (print-instruction-uniquely OUT 'LDRdZ+)
    (fprintf OUT "LD R~a[~a],Z+[~a]"
             Rd (num->hex Rd-val)(num->hex z-val)))
  (set! clock-cycles 2))

(define (avr-LDI Rd-16 K)
  (define Rd (+ Rd-16 16))
  (define Rd-val (get-register Rd))
  (set-register Rd K)
  (when debug?
    (print-instruction-uniquely OUT 'LDI)
    (fprintf OUT "LDI R~a[~a],K[~a]"
             Rd (num->hex Rd-val) (num->hex K)))
  (set! clock-cycles 1))

(define (avr-LDS-get-args Rd k)
  (list avr-LDS (list Rd k)))
(define (avr-LDS Rr k)
  (define k-val (sram-get-byte k))
  (set-register Rr k-val)
  (when debug?
    (print-instruction-uniquely OUT 'LDS)
    (fprintf OUT "LDS R~a,(~a)[~a]"
             Rr (num->hex k) (num->hex k-val)))
  (inc-pc)
  (set! clock-cycles 2))

(define (avr-LPM _)
  (define Rd-val (get-register 0))
  (define z (get-z))
  (define z-val (flash-get-byte z))
  (set-register 0 z-val)
  (when debug?
    (print-instruction-uniquely OUT 'LPM)
    (fprintf OUT "LPM R0[~a],Z[~a]"
             (num->hex Rd-val) (num->hex z-val)))
  (set! clock-cycles 3))

(define (avr-LPM-Z Rd)
  (define Rd-val (get-register Rd))
  (define z (get-z))
  (define z-val (flash-get-byte z))
  (set-register Rd z-val)
  (when debug?
    (print-instruction-uniquely OUT 'LPM)
    (fprintf OUT "LPM R~a[~a],Z[~a]"
             Rd (num->hex Rd-val) (num->hex z-val)))
  (set! clock-cycles 3))

(define (avr-LPM-Z-incr Rd)
  (define z (get-z))
  (define z-val (flash-get-byte z))
  (set-register Rd z-val)
  (inc-z)
  (when debug?
    (print-instruction-uniquely OUT 'LPMRdZ+)
    (fprintf OUT "LPM R~a,Z+ [~a]" Rd (num->hex z-val)))
  (set! clock-cycles 3))


(define (avr-NEG Rd)
  (define Rd-val (get-register Rd))
  (define R (& (- Rd-val) #xff))
  (set-register Rd R)
  (if (one? (ior (bit-ref R 3)
                 (n-bit-ref Rd-val 3)))
      (sr-set-H) (sr-clear-H))
  (if (= R #x80) (sr-set-V)(sr-clear-V))
  (compute-S)
  (compute-N R)
  (compute-Z R)
  (if (zero? R) (sr-clear-C)(sr-set-C))
  (when debug?
    (print-instruction-uniquely OUT 'NEG)
    (fprintf OUT "NEG R~a[~a] ; ~a"
             Rd
             (num->hex Rd-val)
             (num->hex R)))
  (set! clock-cycles 1))

(define (avr-LSR Rd)
  (define Rd-val (get-register Rd))
  (define R (<< Rd-val -1))
  (set-register Rd R)
  (if (! Rd-val 0) (sr-set-C) (sr-clear-C))
  (compute-Z R)
  (sr-clear-N)
  (if (one? (bitwise-xor (sr-get-N) (sr-get-C)))
      (sr-set-V) (sr-clear-V))
  (if (one? (bitwise-xor (sr-get-N) (sr-get-V)))
      (sr-set-S) (sr-clear-S))
  (when debug?
    (print-instruction-uniquely OUT 'LSR)
    (fprintf OUT "LSR R~a[~a] ; ~a"
             Rd
             (num->hex Rd-val)
             (num->hex R)))
  (set! clock-cycles 1))

(define (avr-MOV Rd Rr)
  (define Rd-val (get-register Rd))
  (define Rr-val (get-register Rr))
  (set-register Rd Rr-val)
  (when debug?
    (print-instruction-uniquely OUT 'MOV)
    (fprintf OUT "MOV R~a[~a],R~a[~a]"
             Rd (num->hex Rd-val) Rr (num->hex Rr-val)))
  (set! clock-cycles 1))

(define (avr-MUL Rd Rr)
  (define Rd-val (get-register Rd))
  (define Rr-val (get-register Rr))
  (define R (* Rd-val Rr-val))
  ;; save the result in R1:R0
  (set-register 0 (& R #xff))
  (set-register 1 (<<& R -8 #xff))
  (compute-Z R)
  (if (! R 15) (sr-set-C) (sr-clear-C))
  (when debug?
    (print-instruction-uniquely OUT 'MUL)
    (fprintf OUT "MUL R~a[~a],R~a[~a] ; ~a"
             Rd (num->hex Rd-val)
             Rr (num->hex Rr-val)
             (num->hex R)))
  (set! clock-cycles 2))

(define (avr-MOVW Rd/2 Rr/2)
  (define Rd (* Rd/2 2))
  (define Rr (* Rr/2 2))
  (define Rd+ (+ Rd 1))
  (define Rr+ (+ Rr 1))
  (define Rr-val (get-register Rr))
  (set-register Rd Rr-val)
  (define Rr+-val (get-register Rr+))
  (set-register Rd+ Rr+-val)
  (when debug?
    (print-instruction-uniquely OUT 'MOVW)
    (fprintf OUT "MOVW R~a:R~a,R~a[~a]:R~a[~a]"
             Rd Rd+ Rr
             (num->hexb Rr-val)
             Rr+
             (num->hexb Rr+-val)))
  (set! clock-cycles 1))

;; signed 8bit x 8bit multiplication
(define (avr-MULS Rd-16 Rr-16)
  (define Rd (+ Rd-16 16))
  (define Rr (+ Rr-16 16))
  (when (or (< Rd 16) (> Rd 31))
    (error 'avr-MULS "Rd should be >= 16 and <= 31"))
  (when (or (< Rr 16) (> Rr 31))
    (error 'avr-MULS "Rr should be >= 16 and <= 31"))
  (define Rd-val (2-complement->num 8 (get-register Rd)))
  (define Rr-val (2-complement->num 8 (get-register Rr)))
  (define R (& (* Rd-val Rr-val) #xffff))
  (set-register 1 (&<< R #xff00 -8))
  (set-register 0 (& R #xff))
  (compute-Z R)
  (if (bitwise-bit-set? R 14) (sr-set-C) (sr-clear-C))
  (when debug?
    (print-instruction-uniquely OUT 'MULS)
    (fprintf OUT "MULS R~a[~a], R~a[~a] ; ~a"
             Rd Rd-val Rr Rr-val
             (num->hexb R)))
  (set! clock-cycles 2))

(define (avr-ORI Rd K)
  (define Rd-val (get-register Rd))
  (define R (ior Rd-val K))
  (set-register Rd R)
  (sr-clear-V)
  (compute-N R)
  (compute-S)
  (compute-Z R)
  (when debug?
    (print-instruction-uniquely OUT 'ORI)
    (fprintf OUT "ORI R~a[~a],K[~a] ; ~a"
             Rd (num->hex Rd-val)
             (num->hex K)
             (num->hex R)))
  (set! clock-cycles 1))

(define (avr-IN A Rd)
  (define A-val (io-get A))
  (set-register Rd A-val)
  (when debug?
    (print-instruction-uniquely OUT 'IN)
    (fprintf OUT "IN R~a,A[~a] ; [~a]"
             Rd (num->hex A) (num->hex A-val)))
  (set! clock-cycles 1))

(define (avr-OUT A Rr)
  (define Rr-val (get-register Rr))
  (io-set A Rr-val)
  (when debug?
    (print-instruction-uniquely OUT 'OUT)
    (fprintf OUT "OUT A[~a],R~a[~a]"
             (num->hex A) Rr (num->hex Rr-val)))
  (set! clock-cycles 1))

(define (avr-POP Rd)
  (define Rd-old-val (get-register Rd))
  (define Rd-new-val (stack-pop))
  (set-register Rd Rd-new-val)
  (when debug?
    (print-instruction-uniquely OUT 'POP)
    (fprintf OUT "POP R~a[~a(~a)]"
             Rd Rd-new-val (num->hex Rd-new-val)))
  (set! clock-cycles 2))

(define (avr-PUSH Rd)
  (define Rd-val (get-register Rd))
  (stack-push Rd-val)
  (when debug?
    (print-instruction-uniquely OUT 'PUSH)
    (fprintf OUT "PUSH R~a[~a(~a)]" Rd Rd-val (num->hex Rd-val)))
  (set! clock-cycles 2))

(define (avr-RCALL k-unsigned)
  (define k (2-complement->num 12 k-unsigned))
  (stack-push-word PC)
  (set-pc! (+ PC k))
  (when debug?
    (print-instruction-uniquely OUT 'RCALL)
    (fprintf OUT "RCALL ~a" (lookup-address PC)))
  (set! clock-cycles 3))

(define (avr-RET _)
  (set-pc! (stack-pop-word))
  (when debug?
    (print-instruction-uniquely OUT 'RET)
    (fprintf OUT "RET"))
  (set! clock-cycles 4))

(define (avr-RJMP k-unsigned)
  (define k (2-complement->num 12 k-unsigned))
  (define PC-now PC)
  (set-pc! (& (+ PC k) FLASHEND))
  (when debug?
    (print-instruction-uniquely OUT 'RJMP)
    (fprintf OUT "RJMP ~a"
             (num->hex (- (& (+ PC k) FLASHEND) PC -1))))
  (set! clock-cycles 2))

(define (avr-ROR Rd)
  (define C (sr-get-C))
  (define Rd-val (get-register Rd))
  (define low-bit (bit-ref Rd-val 0))
  (define R (ior (<< C 7) (<< Rd-val -1)))
  (set-register Rd R)
  (if (one? low-bit) (sr-set-C) (sr-clear-C))
  (if (one? C) (sr-set-N) (sr-clear-N))
  (if (one? (bitwise-xor (sr-get-N) low-bit))
      (sr-set-V) (sr-clear-V))
  (compute-S)
  (compute-Z R)
  (when debug?
    (print-instruction-uniquely OUT 'ROR)
    (fprintf OUT "ROR R~a[~a] ; ~a"
             Rd (num->hex Rd-val) (num->hex R)))
  (set! clock-cycles 1))

(define (avr-SBCI Rd-4-bytes K)
  (define Rd (+ Rd-4-bytes 16))
  (define Rd-val (get-register Rd))
  (define C (sr-get-C))
  (define R (& (- Rd-val K C) #xff))
  (set-register Rd R)
  (compute-H Rd-val K R 3)
  (compute-V Rd-val K R 7)
  (compute-N R 7)
  (compute-S)
  (unless (zero? R) (sr-clear-Z))
  (compute-C Rd-val K R 7)
  (when debug?
    (print-instruction-uniquely OUT 'SBCI)
    (fprintf OUT "SBCI R~a[~a],K[~a] ; ~a"
             Rd (num->hex Rd-val) (num->hex K) (num->hex R)))
  (set! clock-cycles 1)
  )

(define (avr-SBI A b)
  (io-set-bit A b)
  (when debug?
    (print-instruction-uniquely OUT 'SBI)
    (fprintf OUT "SBI A[~a],b[~a]"
             (num->hex A) b))
  (set! clock-cycles 2))

(define (avr-SBIC A b)
  (define b-val (io-get-bit A b))
  (when (zero? b-val)
    (define next-instr (vector-ref PROCEDURES PC))
    (when (opcode-info-32-bit? next-instr)
      (inc-pc)
      (set! clock-cycles (+ clock-cycles 1)))
    (inc-pc)
    (set! clock-cycles (+ clock-cycles 1)))
  (set! clock-cycles (+ clock-cycles 1))
  (when debug?
    (print-instruction-uniquely OUT 'SBIC)
    (fprintf OUT "SBIC A[~a],b[~a] ; ~a"
             (num->hex A) b (zero? b-val))))

(define (avr-SBIS A b)
  (define b-val (io-get-bit A b))
  (when (one? b-val)
    (define next-instr (vector-ref PROCEDURES PC))
    (when (opcode-info-32-bit? next-instr)
      (inc-pc)
      (set! clock-cycles (+ clock-cycles 1)))
    (inc-pc)
    (set! clock-cycles (+ clock-cycles 1)))
  (set! clock-cycles (+ clock-cycles 1))
  (when debug?
    (print-instruction-uniquely OUT 'SBIS)
    (fprintf OUT "SBIS A[~a],b[~a] ; ~a"
             (num->hex A) b (zero? b-val))))

(define (avr-SBIW K Rd-2-bits)
  (define Rd (+ 24 (* Rd-2-bits 2)))
  (define Rd+ (+ Rd 1))
  (define Rdh (get-register Rd+))
  (define Rdl (get-register Rd))
  (define Rd-val (ior (<< Rdh 8) Rdl))
  (define R (& (- Rd-val K) #xffff))
  (set-register Rd+ (<< R -8))
  (set-register Rd (& R #xff))
  (compute-Z R)
  (if (one? (& (bit-ref R 15)
               (n-bit-ref Rdh 7)))
      (sr-set-C) (sr-clear-C))
  (compute-N R 15)
  (if (one? (& (n-bit-ref R 15)
               (bit-ref Rdh 7)))
      (sr-set-V) (sr-clear-V))
  (compute-S)
  (when debug?
    (print-instruction-uniquely OUT 'SBIW)
    (fprintf OUT "SBIW R~a:R~a[~a],K[~a] ; ~a"
             Rd Rd+
             (num->hex Rd-val)
             (num->hex K)
             (num->hex R)))
  (set! clock-cycles 2))

(define (avr-ST-X Rr)
  (define x (get-x))
  (define Rr-val (get-register Rr))
  (sram-set-byte x Rr-val)
  (when debug?
    (print-instruction-uniquely OUT 'STXRr)
    (fprintf OUT "ST X,R~a[~a]" Rr (num->hex Rr-val)))
  (set! clock-cycles 2))

(define (avr-ST-X-incr Rr)
  (define x (get-x))
  (define Rr-val (get-register Rr))
  (sram-set-byte x Rr-val)
  (inc-x)
  (when debug?
    (print-instruction-uniquely OUT 'STX+Rr)
    (fprintf OUT "ST X+,R~a[~a]" Rr (num->hex Rr-val)))
  (set! clock-cycles 2))

(define (avr-ST-X-decr Rr)
  (define x (get-x))
  (define Rr-val (get-register Rr))
  (sram-set-byte x Rr-val)
  (dec-x)
  (when debug?
    (print-instruction-uniquely OUT 'STX-Rr)
    (fprintf OUT "ST X-,R~a[~a]" Rr (num->hex Rr-val)))
  (set! clock-cycles 2))

(define (avr-STD-Y Rr q)
  (define y (get-y))
  (define Rr-val (get-register Rr))
  (sram-set-byte (+ y q) Rr-val)
  (when debug?
    (print-instruction-uniquely OUT 'STY+qRr)
    (fprintf OUT "ST Y+~a[~a],R~a ; ~a"
             q (num->hex y) Rr
             (num->hex Rr-val)))
  (set! clock-cycles 2))

(define (avr-ST-Y-incr Rr)
  (define y (get-y))
  (define Rr-val (get-register Rr))
  (sram-set-byte y Rr-val)
  (inc-y)
  (when debug?
    (print-instruction-uniquely OUT 'STY+Rr)
    (fprintf OUT "ST Y+,R~a[~a]" Rr (num->hex Rr-val)))
  (set! clock-cycles 2))

(define (avr-ST-Y-decr Rr)
  (define y (get-y))
  (define Rr-val (get-register Rr))
  (sram-set-byte y Rr-val)
  (dec-y)
  (when debug?
    (print-instruction-uniquely OUT 'STY-Rr)
    (fprintf OUT "ST Y-,R~a[~a]" Rr (num->hex Rr-val)))
  (set! clock-cycles 2))

(define (avr-SWAP Rd)
  (define Rd-val (get-register Rd))
  (define R (ior (&<< Rd-val #x0f 4)
                 (&<< Rd-val #xf0 -4)))
  (set-register Rd R)
  (when debug?
    (print-instruction-uniquely OUT 'SWAP)
    (fprintf OUT "SWAP R~a[~a] ; ~a"
             Rd (num->hex Rd-val) (num->hex R)))
  (set! clock-cycles 1))

(define (avr-STD-Z Rr q)
  (define z (get-z))
  (define Rr-val (get-register Rr))
  (sram-set-byte (+ z q) Rr-val)
  (when debug?
    (print-instruction-uniquely OUT 'STZ+qRr)
    (fprintf OUT "ST Z+~a,R~a[~a]"
             q Rr (num->hex Rr-val)))
  (set! clock-cycles 2))

(define (avr-ST-Z-decr Rr)
  (define z (get-z))       
  (define Rr-val (get-register Rr))
  (sram-set-byte z Rr-val)
  (dec-z)
  (when debug?
    (print-instruction-uniquely OUT 'STZ-Rr)
    (fprintf OUT "ST Z-,R~a[~a]" Rr (num->hex Rr-val)))
  (set! clock-cycles 2))

(define (avr-ST-Z-incr Rr)
  (define z (get-z))       
  (define Rr-val (get-register Rr))
  (sram-set-byte z Rr-val)
  (inc-z)
  (when debug?
    (print-instruction-uniquely OUT 'STZ+Rr)
    (fprintf OUT "ST Z+,R~a[~a]" Rr (num->hex Rr-val)))
  (set! clock-cycles 2))


(define (avr-STS-get-args Rr k)
  (list avr-STS (list Rr k)))
(define (avr-STS Rr k)
  (define Rr-val (get-register Rr))
  (when debug?
    (print-instruction-uniquely OUT 'STS)
    (fprintf OUT "STS (~a),R~a[~a]"
             (num->hex k) Rr (num->hex Rr-val)))
  (sram-set-byte k Rr-val)
  (inc-pc)
  (set! clock-cycles 2))

(define (avr-SUBI Rd-4-bytes K)
  (define Rd (+ 16 Rd-4-bytes))
  (define Rd-val (get-register Rd))
  (define R (& (- Rd-val K) #xff))
  (set-register Rd R)
  (compute-H Rd-val K R 3)
  (compute-V Rd-val K R 7)
  (compute-N R 7)
  (compute-S)
  (compute-Z R)
  (compute-C Rd-val K R 7)
  (when debug?
    (print-instruction-uniquely OUT 'SUBI)
    (fprintf OUT "SUBI R~a[~a],K[~a] ; ~a"
             Rd (num->hex Rd-val) (num->hex K) (num->hex R)))
  (set! clock-cycles 1))

(define (avr-BLD Rd b)
  (define Rd-val (get-register Rd))
  (define T (sr-get-T))
  (define R (ior Rd-val (<< T b)))
  (set-register Rd R)
  (when debug?
    (print-instruction-uniquely OUT 'BLD)
    (fprintf OUT "BLD R~a[~a],b[~a] ; ~a"
             Rd (num->hex Rd-val) (num->hex b) (num->hex R)))
  (set! clock-cycles 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 32-bit instructions
;; either get the second half, when saving
;; or execute, when second arg is empty
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (avr-CALL-get-args kh kl)
  (list avr-CALL (list (ior (<< kh 16) kl))))
(define (avr-CALL k)
  (stack-push-word (+ PC 1))
  (set-pc! k)
  (when debug?
    (print-instruction-uniquely OUT 'CALL)
    (fprintf OUT "CALL ~a" (lookup-address PC)))
  (set! clock-cycles 4))

(define (avr-JMP-get-args kh kl)
  (list avr-JMP (list (+ (<< kh 16) kl))))
(define (avr-JMP k)
  (set-pc! k)
  (when debug?
    (print-instruction-uniquely OUT 'JMP)
    (fprintf OUT "JMP ~a" k))
  (set! clock-cycles 3))

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
    (list #x9409 'IJMP   avr-IJMP   2 #f)
    (list #x95C8 'LPM    avr-LPM    2 #f)
    (list #x0000 'NOP    avr-NOP     2 #f)
    (list #x9508 'RET    avr-RET    2 #f)
    (list #x9518 'RETI   'avr-RETI   2 #f)
    (list #x9588 'SLEEP  'avr-SLEEP  2 #f)
    (list #x95E8 'SPM    'avr-SPM    2 #f)
    (list #x95A8 'WDR    'avr-WDR    2 #f))))
;; opcodes with two 5-bit registers Rd and Rr
(define opcodes-5-bit-Rd-Rr
  (make-hash
   (list
    (list #x1C00 'ADC avr-ADC 2 #f)
    (list #x0C00 'ADD avr-ADD 2 #f)
    (list #x2000 'AND avr-AND 2 #f)
    (list #x1400 'CP avr-CP 2 #f)
    (list #x0400 'CPC avr-CPC 2 #f)
    (list #x1000 'CPSE avr-CPSE 2 #f)
    (list #x2400 'EOR avr-EOR 2 #f)
    (list #x2C00 'MOV avr-MOV 2 #f)
    (list #x9C00 'MUL avr-MUL 2 #f)
    (list #x2800 'OR avr-OR 2 #f)
    (list #x0800 'SBC avr-SBC 2 #f)
    (list #x1800 'SUB avr-SUB 2 #f))))
;; opcode with a single register Rd as operand
(define opcodes-Rd
  (make-hash
   (list
    (list #x9405 'ASR 'avr-ASR 2 #f)
    (list #x9400 'COM 'avr-COM 2 #f)
    (list #x940A 'DEC avr-DEC 2 #f)
    (list #x9006 'ELPM-Z 'avr-ELPM-Z 2 #f)
    (list #x9007 'ELPM-Z-incr 'avr-ELPM-Z-incr 2 #f)
    (list #x9403 'INC avr-INC 2 #f)
    (list #x9000 'LDS avr-LDS-get-args 2 #t)
    (list #x900C 'LD-X avr-LD-X 2 #f)
    (list #x900E 'LD-X-decr avr-LD-X-decr 2 #f)
    (list #x900D 'LD-X-incr avr-LD-X-incr 2 #f)
    (list #x900A 'LD-Y-decr avr-LD-Y-decr 2 #f)
    (list #x9009 'LD-Y-incr avr-LD-Y-incr 2 #f)
    (list #x9002 'LD-Z-decr avr-LD-Z-decr 2 #f)
    (list #x9001 'LD-Z-incr avr-LD-Z-incr 2 #f)
    (list #x9004 'LPM-Z avr-LPM-Z 2 #f)
    (list #x9005 'LPM-Z-incr avr-LPM-Z-incr 2 #f)
    (list #x9406 'LSR avr-LSR 2 #f)
    (list #x9401 'NEG avr-NEG 2 #f)
    (list #x900F 'POP avr-POP 2 #f)
    (list #x920F 'PUSH avr-PUSH 2 #f)
    (list #x9407 'ROR avr-ROR 2 #f)
    (list #x9200 'STS avr-STS-get-args 2 #t)
    (list #x920C 'ST-X avr-ST-X 2 #f)
    (list #x920E 'ST-X-decr avr-ST-X-decr 2 #f)
    (list #x920D 'ST-X-incr avr-ST-X-incr 2 #f)
    (list #x920A 'ST-Y-decr avr-ST-Y-decr 2 #f)
    (list #x9209 'ST-Y-incr avr-ST-Y-incr 2 #f)
    (list #x9202 'ST-Z-decr avr-ST-Z-decr 2 #f)
    (list #x9201 'ST-Z-incr avr-ST-Z-incr 2 #f)
    (list #x9402 'SWAP avr-SWAP 2 #f))))
;; opcodes with a register Rd and a constant data K
(define opcodes-Rd-K
  (make-hash
   (list
    (list #x7000 'ANDI avr-ANDI 2 #f)
    (list #x3000 'CPI avr-CPI 2 #f)
    (list #xE000 'LDI avr-LDI 2 #f)
    (list #x6000 'ORI avr-ORI 2 #f)
    (list #x4000 'SBCI avr-SBCI 2 #f)
    (list #x5000 'SUBI avr-SUBI 2 #f)
    (list #xF800 'BLD avr-BLD 2 #f))))
;; opcodes with a register Rd and a register bit number b
(define opcodes-Rd-b
  (make-hash
   (list
    (list #xFA00 'BST avr-BST 2 #f)
    (list #xFC00 'SBRC avr-SBRC 2 #f)
    (list #xFE00 'SBRS avr-SBRS 2 #f))))
;; opcodes with relative 7-bit address k and register bit number b
(define opcodes-7-bit-k-b
  (make-hash
   (list
    (list #xF400 'BRBC avr-BRBC 2 #f)
    (list #xF000 'BRBS avr-BRBS 2 #f))))
;; opcodes with a 6-bit address displacement q and a register Rd
(define opcodes-6-bit-q-Rd
  (make-hash
   (list
    (list #x8008 'LDD-Y avr-LDD-Y 2 #f)
    (list #x8000 'LDD-Z avr-LDD-Z 2 #f)
    (list #x8208 'STD-Y avr-STD-Y 2 #f)
    (list #x8200 'STD-Z avr-STD-Z 2 #f))))
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
    (list #x9488 'BCLR avr-BCLR 2 #f)
    (list #x9408 'BSET avr-BSET 2 #f))))
;; opcodes with a 6-bit constant K and a register Rd
(define opcodes-6-bit-K-Rd
  (make-hash
   (list
    (list #x9600 'ADIW avr-ADIW 2 #f)
    (list #x9700 'SBIW avr-SBIW 2 #f))))
;; opcodes with a 5-bit IO Addr A and register bit number b
(define opcodes-5-bit-A-b
  (make-hash
   (list
    (list #x9800 'CBI avr-CBI 2 #f)
    (list #x9A00 'SBI avr-SBI 2 #f)
    (list #x9900 'SBIC avr-SBIC 2 #f)
    (list #x9B00 'SBIS avr-SBIS 2 #f))))
;; opcodes with a 6-bit IO Addr A and register Rd
(define opcodes-6-bit-A-Rd
  (make-hash
   (list
    (list #xB000 'IN avr-IN 2 #f)
    (list #xB800 'OUT avr-OUT 2 #f))))
;; opcodes with a relative 12-bit address k
(define opcodes-12-bit-k
  (make-hash
   (list
    (list #xD000 'RCALL avr-RCALL 2 #f)
    (list #xC000 'RJMP avr-RJMP 2 #f))))
;; opcodes with two 4-bit register Rd and Rr
(define opcodes-4-bit-Rd-Rr
  (make-hash
   (list
    (list #x0100 'MOVW avr-MOVW 2 #f)
    (list #x0200 'MULS avr-MULS 2 #f))))
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
    ;; opcode unknown (probably data, or 32-bit instr)
    (cond [(empty? tables-and-masks) #f]
          [else
           (define mapping (car tables-and-masks))
           (define masks (get-masks mapping))
           (define table (get-table mapping))
           (define mask (bitwise-not (apply ior masks)))
           (define an-opcode-info (lookup-opcode (get-table mapping)
                                                 (& opcode mask)))
           (define args (get-args opcode masks))
           (if an-opcode-info
               (apply opcode-info (append (cons opcode
                                                an-opcode-info)
                                          (list args)))
               (loop (cdr tables-and-masks)))])))

;; convert the flash into prepared procedures
;; make sure to get args of 32-bit opcodes
(define (flash->procedures flash)
  ;; vector of procedures with same size
  (define procedures (make-vector FLASHEND #f))
  (define was-32-bit? #f) ;; will ignore the word when "yes"
  (let loop ([addr 0])
    (unless (>= addr FLASHEND)
      (define an-opcode-info (decode (flash-get-word addr)))
      (if (and (not was-32-bit?) an-opcode-info)
          (let ([opcode  (opcode-info-opcode an-opcode-info)]
                [name    (opcode-info-name an-opcode-info)]
                [proc    (opcode-info-proc an-opcode-info)]
                [cycles  (opcode-info-cycles an-opcode-info)]
                [32-bit? (opcode-info-32-bit? an-opcode-info)]
                [args    (opcode-info-args an-opcode-info)])
            ;; 32-bit opcodes need to lookup the next word
            (when (opcode-info-32-bit? an-opcode-info)
              (let ([next-word (flash-get-word (+ addr 1))])
                (set! was-32-bit? #t)
                (set! opcode (+ (<< opcode 16) next-word))
                (define proc-args
                  (apply proc (append args (list next-word))))
                (set! proc (car proc-args))
                (set! args (cadr proc-args))))
            ;; copy
            (define result
              (opcode-info opcode name proc cycles 32-bit? args))
            (vector-set! procedures addr result))
          (set! was-32-bit? #f))
      (loop (+ addr 1))))
  procedures)

(define (flash->procedures!)
  (set! PROCEDURES (flash->procedures FLASH)))

(define (run (n 1))
  (for ([i n])
    (define saved-pc PC)
    (define symbol (lookup-address PC))
    (define instr (vector-ref PROCEDURES PC))
    (inc-pc)
    (set! clock-cycles 0)
    (when instr
      (define 32-bit? (opcode-info-32-bit? instr))
      (define args    (opcode-info-args instr))
      (define proc    (opcode-info-proc instr))
      (define opcode  (opcode-info-opcode instr))
      (when debug?   
        (fprintf OUT "~a|~a|~a|"
                 CURRENT-CLOCK-CYCLE (* (- PC 1) 2)
                 (num->hex opcode)))
      (apply proc args)
      (when debug?
        (when (and symbol symbol-need-to-print?)
          (fprintf OUT " ;; ~a" symbol))
        (fprintf OUT "~n"))
      (when save-intermediate-values?
        (save-values! saved-pc CURRENT-CLOCK-CYCLE
                      (+ CURRENT-CLOCK-CYCLE clock-cycles))
        (when (> clock-cycles 4)
          (printf "~a: >4~n" CURRENT-CLOCK-CYCLE)))
      (set! CURRENT-CLOCK-CYCLE
            (+ CURRENT-CLOCK-CYCLE clock-cycles)))))

(define (print-flash-procedures procs)
  (for ([an-opcode-info procs]
        [i (vector-length procs)])
    (when an-opcode-info
      (define opcode  (opcode-info-opcode  an-opcode-info))
      (define name    (opcode-info-name    an-opcode-info))
      (define proc    (opcode-info-proc    an-opcode-info))
      (define cycles  (opcode-info-cycles  an-opcode-info))
      (define 32-bit? (opcode-info-32-bit? an-opcode-info))
      (define args    (opcode-info-args    an-opcode-info))
      (printf "~a: ~a ~a ~a ~a ~a ~a~n"
              (num->hex i)
              (num->hex opcode)
              name proc cycles 32-bit?
              (map num->hex args)))
    (unless an-opcode-info
      (printf "~a: #f~n" i))))
