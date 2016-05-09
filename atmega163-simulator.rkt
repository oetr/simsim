(require racket racket/main rackunit)
;;(load "instruction-table.rkt")
;; Develop an abstract machine that is loaded with a hex /elf file,
;; is given a start address, and can simulate the execution

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2) hex file reader
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (hex->num hex) (string->number hex 16))
(define (num->hex num) (number->string num 16))
(define (bin->num bin) (string->number bin 2))
(define (num->bin num) (number->string num 2))
;; add a leading "0" to a number and convert it into a string
(define (num->hexb num)
  (if (<= num #xf) 
      (string-append "0" (num->hex num))
      (num->hex num)))
(define (2-complement->num width num)
  (if (! num (- width 1))
        (+ (- (<< 1 (- width 1)))
         (bitwise-bit-field num 0 (- width 1)))
      num))

(define (num->bytes num (n-bytes 32))
  (list->bytes 
   (for/list ([i (range (- (quotient n-bytes 8) 1) -1 -1)])
     (<<& num (* i -8) #xff))))

(define (hex->flash! a-file)
  (define hex (file->lines (expand-user-path a-file)))
  ;; write data
  (for ([line hex])
    (define len (/ (- (string-length line) 3) 2))
    ;; Start code
    (define start-code (substring line 0 1))
    ;; Byte count
    (define byte-count (hex->num (substring line 1 3)))
    ;; Address
    (define address (hex->num (substring line 3 7)))
    ;; Record type
    (define record-type (hex->num (substring line 7 9)))
    ;; Data
    (define end-data-addr (+ 9 (* byte-count 2)))
    (define data (substring line 9 end-data-addr))
    ;; Checksum
    (define checksum
      (hex->num 
       (substring line end-data-addr (+ end-data-addr 2))))
    (define computed-checksum 0)
    (for ([i len])
      (set! computed-checksum 
            (+ computed-checksum
               (hex->num (substring line 
                                    (+ (* i 2) 1) 
                                    (+ (* i 2) 3))))))
    (set! computed-checksum
          (modulo 
           (+ (bitwise-and (bitwise-not computed-checksum) #xff) 1)
           256))
    (when (not (= checksum computed-checksum))
      (error 'read-hex-file "Checksum wrong: expected ~a, got ~a"
             checksum computed-checksum))
    ;; write the data into the memory
    (for ([i (range 0 (string-length data) 4)])
      ;; convert LSB-bytes into MSB-bytes
      (define num (hex->num (substring data i (+ i 4))))
      (define hb1 (bitwise-and num #xf))
      (define hb0 (bitwise-and (arithmetic-shift num -4) #xf))
      (define hb3 (bitwise-and (arithmetic-shift num -8) #xf))
      (define hb2 (bitwise-and (arithmetic-shift num -12) #xf))
      (define msb-num
        (bitwise-ior (arithmetic-shift hb0 12)
                     (arithmetic-shift hb1 8)
                     (arithmetic-shift hb2 4)
                     hb3))
      (flash-set-word (+ (/ address 2) (/ i 4)) msb-num))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; symbol table reader: avr-objdump -C -t main.elf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ADDRESS-TABLE (hash))
(define SYMBOL-TABLE (hash))
(define (load-symbol-table a-file)
  (define syms-lines (file->lines (expand-user-path a-file)))
  (define addrs empty)
  (define syms empty)
  (for ([l syms-lines])
    (define a-match
      (regexp-match #px"([0-9a-f]+).+?([gl]).+?\\.text\t[0-9a-f]+ (.+)$" l))
    (when (list? a-match)
      (define addr (/ (hex->num (cadr a-match)) 2))
      (define symbol (cadddr a-match))
      (set! addrs (cons (cons addr symbol)
                        addrs))
      (set! syms (cons (cons symbol addr)
                       syms))))
  (set! ADDRESS-TABLE (make-hash addrs))
  (set! SYMBOL-TABLE (make-hash syms)))

(define (lookup-address addr)
  (hash-ref ADDRESS-TABLE addr #f))
(define (lookup-symbol a-symbol)
  (hash-ref SYMBOL-TABLE a-symbol #f))
(define (print-symbols)
  (define mapping (sort (hash->list SYMBOL-TABLE) 
                        (lambda (a b) (string<? (car a) (car b)))))
  (for([element mapping])
    (printf "~a: ~a~n" (car element) (num->hex (cdr element)))))
(define (print-addrs)
  (define mapping (sort (hash->list ADDRESS-TABLE) (lambda (a b) (< (car a) (car b)))))
  (for([element mapping])
    (printf "~a: ~a~n" (num->hex (car element))  (cdr element))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1) Atmega163L Specs:
;; 8Kx16 bits program memory, self-programmable
;; 1024x8 bits SRAM
;; 32x8-bit working registers
;; 32 I/O lines
;; 512 bytes EEPROM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 8Kx16 bits program memory, self-programmable
(define FLASHEND #x1fff)
(define FLASH (make-vector FLASHEND))
(define (flash-length) FLASHEND)
;; get and set word use word addresses
(define (flash-get-word addr) (vector-ref FLASH addr))
(define (flash-set-word addr val) (vector-set! FLASH addr val))
;; get-byte use byte addresses
(define (flash-get-byte addr)
  (define word (flash-get-word (arithmetic-shift addr -1)))
  (if (bitwise-bit-set? addr 0)      
      (arithmetic-shift word -8)
      (bitwise-and word #x00ff)))

(define (print-flash)
  (define (dots-when-zero num)
    (if (zero? num) ".." (num->hexb num)))
  (printf "    ")
  (for ([i #x10])
    (printf "~a  " (num->hex i)))
  (define accumulated-bytes (bytes))
  (for ([addr FLASHEND])
    (when (zero? (modulo addr #x10))
      (printf "~a~n~a " 
              accumulated-bytes (num->hex (quotient addr #x10)))
      (when (< (quotient addr #x10) #x10) (printf " "))
      (set! accumulated-bytes (bytes)))
    (define byte (flash-get-byte addr))
    (define char (bytes byte))
    (when (or (< byte 35) (> byte 126))
        (set! char #"."))
    (set! accumulated-bytes (bytes-append accumulated-bytes char))
    (printf "~a " (dots-when-zero byte)))
  (printf "~a~n " accumulated-bytes))

(define RAMEND (+ #x045f 1))
(define SRAM (make-vector RAMEND #x00))
;; get and set bytes
(define (sram-get-byte addr)
  (define address addr)
  (when (>= addr RAMEND)
    (printf "WARNING: address outside RAMEND ~a~n" (num->hex addr))
    (set! address (modulo (+ addr IO-SIZE) RAMEND)))
  (define data (vector-ref SRAM address))
  (save-intermediate-values address)
  (save-intermediate-values data)
  data)
(define (sram-set-byte addr val)
  (define address addr)
  (when (> addr RAMEND)
    (print "WARNING: address outside RAMEND~n")
    (set! address (modulo (+ addr IO-SIZE) RAMEND)))
  (vector-set! SRAM address val)
  (save-intermediate-values address)
  (save-intermediate-values val))
  

(define (sram-set-bit addr i)
  (sram-set-byte addr (ior (sram-get-byte addr) (<< 1 i))))
(define (sram-clear-bit addr i)
  (sram-set-byte addr (& (sram-get-byte addr)
                         (bitwise-not (<< 1 i)))))
(define (sram-get-bit addr i)
  (& (<< (vector-ref SRAM addr) (- i)) 1))
;; map register file
(define (get-x) (ior (<< (sram-get-byte 27) 8)
                     (sram-get-byte 26)))
(define (get-y) (ior (<< (sram-get-byte 29) 8)
                     (sram-get-byte 28)))
(define (get-z) (ior (<< (sram-get-byte 31) 8)
                     (sram-get-byte 30)))
(define (inc-x)
  (define x (+ (get-x) 1))
  (sram-set-byte 27 (<<& x -8 #xff))
  (sram-set-byte 26 (& x #xff)))
(define (inc-y)
  (define y (+ (get-y) 1))
  (sram-set-byte 29 (<<& y -8 #xff))
  (sram-set-byte 28 (& y #xff)))
(define (inc-z)
  (define z (+ (get-z) 1))
  (sram-set-byte 31 (<<& z -8 #xff))
  (sram-set-byte 30 (& z #xff)))
(define (print-sram)
  (define (dots-when-zero num)
    (if (zero? num) ".." (num->hexb num)))
  (printf "    ")
  (for ([i #x10])
    (printf "~a  " (num->hex i)))
  (define accumulated-bytes (bytes))
  (for ([addr RAMEND])
    (when (zero? (modulo addr #x10))
      (printf "~a~n~a " 
              accumulated-bytes (num->hex (quotient addr #x10)))
      (when (< (quotient addr #x10) #x10) (printf " "))
      (set! accumulated-bytes (bytes)))
    (define byte (sram-get-byte addr))
    (define char (bytes byte))
    (when (or (< byte 35) (> byte 126))
        (set! char #"."))
    (set! accumulated-bytes (bytes-append accumulated-bytes char))
    (printf "~a " (dots-when-zero byte)))
  (printf "~a~n " accumulated-bytes))


;; map I/O
(define IO-SIZE 64)
(define (io-set addr value)
  (sram-set-byte (+ addr #x20) value))
(define (io-get addr)
  (sram-get-byte (+ addr #x20)))
(define (io-set-bit addr i)
  (sram-set-bit (+ addr #x20) i))
(define (io-clear-bit addr i)
  (sram-clear-bit (+ addr #x20) i))
(define (io-get-bit addr i)
  (sram-get-bit (+ addr #x20) i))

(define (print-io)
  (define (dots-when-zero num)
    (if (zero? num) ".." (num->hexb num)))
  (printf "    ")
  (for ([i #x10])
    (printf "~a  " (num->hex i)))
  (define accumulated-bytes (bytes))
  (for ([addr IO-SIZE])
    (when (zero? (modulo addr #x10))
      (printf "~a~n~a " 
              accumulated-bytes (num->hex (quotient addr #xf)))
      (when (< (quotient addr #xf) #x10) (printf " "))
      (set! accumulated-bytes (bytes)))
    (define byte (io-get addr))
    (define char (bytes byte))
    (when (or (< byte 35) (> byte 126))
        (set! char #"."))
    (set! accumulated-bytes (bytes-append accumulated-bytes char))
    (printf "~a " (dots-when-zero byte)))
  (printf "~a~n " accumulated-bytes))

(define (print-io)
  (printf "   ")
  (for ([i #x10])
    (printf "~a  " (num->hex i)))
  (define i 0)
  (for ([addr #x40])
    (when (zero? (modulo addr #x10))
      (printf "~n~a " (num->hex (quotient addr #xf))))
    (printf "~a " (num->hexb (io-get addr)))))

(define SR-ADDR #x5f)
;; Status register: GET
(define (sr-get)   (sram-get-byte SR-ADDR))
(define (sr-get-bit b) (sram-get-bit SR-ADDR b))
(define (sr-get-I) (sram-get-bit SR-ADDR 7))
(define (sr-get-T) (sram-get-bit SR-ADDR 6))
(define (sr-get-H) (sram-get-bit SR-ADDR 5))
(define (sr-get-S) (sram-get-bit SR-ADDR 4))
(define (sr-get-V) (sram-get-bit SR-ADDR 3))
(define (sr-get-N) (sram-get-bit SR-ADDR 2))
(define (sr-get-Z) (sram-get-bit SR-ADDR 1))
(define (sr-get-C) (sram-get-bit SR-ADDR 0))
;; Status register: SET
(define (sr-set-bit b) (sram-set-bit SR-ADDR b))
(define (sr-set-I) (sram-set-bit SR-ADDR 7))
(define (sr-set-T) (sram-set-bit SR-ADDR 6))
(define (sr-set-H) (sram-set-bit SR-ADDR 5))
(define (sr-set-S) (sram-set-bit SR-ADDR 4))
(define (sr-set-V) (sram-set-bit SR-ADDR 3))
(define (sr-set-N) (sram-set-bit SR-ADDR 2))
(define (sr-set-Z) (sram-set-bit SR-ADDR 1))
(define (sr-set-C) (sram-set-bit SR-ADDR 0))
;; Status register: CLEAR
(define (sr-clear-bit b) (sram-clear-bit SR-ADDR b))
(define (sr-clear-I) (sram-clear-bit SR-ADDR 7))
(define (sr-clear-T) (sram-clear-bit SR-ADDR 6))
(define (sr-clear-H) (sram-clear-bit SR-ADDR 5))
(define (sr-clear-S) (sram-clear-bit SR-ADDR 4))
(define (sr-clear-V) (sram-clear-bit SR-ADDR 3))
(define (sr-clear-N) (sram-clear-bit SR-ADDR 2))
(define (sr-clear-Z) (sram-clear-bit SR-ADDR 1))
(define (sr-clear-C) (sram-clear-bit SR-ADDR 0))


;; program counter
(define PC 0)
(define (inc-pc) (set! PC (+ PC 1)))
(define (dec-pc) (set! PC (- PC 1)))
(define (next-instruction) (flash-get-word PC))

;; Stack
(define SPL-ADDR #x5D)
(define SPH-ADDR #x5E)
(define (get-sp)
  (ior (&<< (sram-get-byte SPH-ADDR) #b00000111 8)
       (sram-get-byte SPL-ADDR)))
(define (inc-sp (n -2))
  (define sp (+ (get-sp) n))
  (sram-set-byte SPH-ADDR (<<& sp -8 #xff))
  (sram-set-byte SPL-ADDR (& sp #xff)))
(define (stack-push data)
  (define sp (get-sp))
  (sram-set-byte sp data)
  (inc-sp -1))
(define (stack-pop)
  (inc-sp 1)
  (define sp (get-sp))
  (define data (sram-get-byte sp))
  data)
(define (stack-push-word word)
  (define sp (get-sp))
  (define word-low (& word #xff))
  (define word-high (<<& word -8 #xff))
  ;; (when debug?
  ;;     (fprintf OUT "{STACK PUSH: sp: ~a, low: ~a, high: ~a}" 
  ;;             sp (num->hex word-low) (num->hex word-high)))
  (sram-set-byte sp word-low)
  (sram-set-byte (- sp 1) word-high)
  (inc-sp -2))
(define (stack-pop-word)
  (define sp (+ (get-sp) 1))
  (define word-high (sram-get-byte sp))
  (define word-low  (sram-get-byte (+ sp 1)))
  ;; (when debug?
  ;;     (fprintf OUT "{STACK POP: sp: ~a, low: ~a, high: ~a}" 
  ;;             sp (num->hex word-low) (num->hex word-high)))
  (inc-sp 2)
  (ior (<< word-high 8)
       word-low))

(define OUT (current-output-port))
(define CURRENT-CLOCK-CYCLE 0)
(define PREVIOUS-CLOCK-CYCLE #f)

(define (reset-machine (filename #f) #:keep-flash? (keep-flash? #f))
  (unless keep-flash?
    (set! FLASH (make-vector FLASHEND #x00)))
  (set! SRAM (make-vector RAMEND #x00))
  (for ([i IO-SIZE]) (vector-set! SRAM i 0))
  (set! CURRENT-CLOCK-CYCLE 0)
  (set! PREVIOUS-CLOCK-CYCLE #f)
  (set! SAVED-PC #f)
  (set! INTERMEDIATE-VALUES '())
  (set! PC 0)
  (if filename
      (begin
        (when (and (port? OUT)
                   (not (port-closed? OUT))
                   (not (eq? OUT (current-output-port))))
          (close-output-port OUT))
        (set! OUT (open-output-file (expand-user-path filename) 
                                    #:exists 'replace)))
      (begin
        (when (and (port? OUT) 
                   (not (port-closed? OUT))
                   (not (eq? OUT (current-output-port))))
          (close-output-port OUT))
        (set! OUT (current-output-port)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3) opcode interpreter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shift + and
(define (<<& num shift-num (and-num #xf))
  (bitwise-and (arithmetic-shift num shift-num)
               and-num))
;; and + shift
(define (&<< num and-num shift-num)
  (arithmetic-shift (bitwise-and num and-num)
                    shift-num))
(define (bit-ref n i) (<<& n (- i) 1))
(define (n-bit-ref n i) (if (zero? (<<& n (- i) 1)) 1 0))
(define ! bitwise-bit-set?)
(define (n! n i) (not (bitwise-bit-set? n i)))
(define << arithmetic-shift)
(define & bitwise-and)
(define ior bitwise-ior)
(define (one? num) (not (zero? num)))


;; make it easier to access the registers
(define (get-Rd opcode)
  (<<& opcode -4 #b11111))
(define (get-Rr opcode) 
  (ior (<<& opcode -5 #b10000)
       (& opcode #xf)))
;; tests
;;(num->bin (get-Rd #b0000000111110000))
;;(num->bin (get-Rr #b0000001111111111))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get register contents and result and compute flags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (compute-H r1 r2 result (bit 3))
  (if (one? (ior (& (n-bit-ref r1 bit) (bit-ref r2 bit))
                 (& (bit-ref r2 bit) (bit-ref result bit))
                 (& (bit-ref result bit) (n-bit-ref r1 bit))))
             (sr-set-H) (sr-clear-H)))
(define (compute-V r1 r2 result (bit 7))
  (if (one? (ior (& (bit-ref r1 bit)
                    (n-bit-ref r2 bit) 
                    (n-bit-ref result bit))
                 (& (n-bit-ref r1 bit)
                    (bit-ref r2 bit) 
                    (bit-ref result bit))))
      (sr-set-V) (sr-clear-V)))
(define (compute-N result (bit 7))
  (if (! result bit) (sr-set-N) (sr-clear-N)))
(define (compute-S)
  (if (= (bitwise-xor (sr-get-N) (sr-get-V)) 1)
      (sr-set-S) (sr-clear-S)))
(define (compute-Z result)
  (if (zero? result) (sr-set-Z) (sr-clear-Z)))
(define (compute-C r1 r2 result (bit 7))
  (if (one? (ior (& (n-bit-ref r1 bit)
                    (bit-ref r2 bit))
                 (& (bit-ref r2 bit)
                    (bit-ref result bit))
                 (& (bit-ref result bit)
                    (n-bit-ref r1 bit))))
      (sr-set-C) (sr-clear-C)))

(define (compute-C-add r1 r2 result (bit 7))
  (if (one? (ior (& (bit-ref r1 bit)
                    (bit-ref r2 bit))
                 (& (bit-ref r2 bit)
                    (n-bit-ref result bit))
                 (& (n-bit-ref result bit)
                    (bit-ref r1 bit))))
      (sr-set-C) (sr-clear-C)))

;; ???

;; TODO: make sure to fetch 32-bit instructions properly
(define (is-two-word-instruction? addr)
  #f)

(define debug? #f)





(define (fetch-and-decode)
  ;; fetch
  (define opcode (next-instruction))
  (define hb3 (& opcode #xf))
  (define hb2 (<<& opcode -4 #xf))
  (define hb1 (<<& opcode -8 #xf))
  (define hb0 (<<& opcode -12 #xf))
  (define clock-cycles 0)
  (when debug?     
    (fprintf OUT "~a|~a|~a|"
             CURRENT-CLOCK-CYCLE
             (num->hex PC) (num->hex opcode)))
  (define symbol (lookup-address PC))
  (define symbol-need-to-print? #t)
  (inc-pc)
  (cond [(= opcode #x95c8) ;;;;;;;;;;;; LPM
         (define z (get-z))
         (define z-val (flash-get-byte z))
         (sram-set-byte 0 z-val)
         (when debug?
           (print-instruction-uniquely OUT 'LPM)
           (fprintf OUT "LPM R0,Z[~a]" (num->hex z-val)))
         (set! clock-cycles 3)
         ]
        [(and (= hb0 #b1001) ;;;;;;;;;;;; LPM Rd,Z
              (= (& hb1 #b1110) 0)
              (= hb3 #b0100))
         (define z (get-z))
         (define z-val (flash-get-byte z))
         (define Rd (ior (&<< hb1 1 4) hb2))
         (sram-set-byte Rd z-val)
         (when debug?
           (print-instruction-uniquely OUT 'LPMRdZ)
           (fprintf OUT "LPM R~a,Z[~a]" Rd (num->hex z-val)))
         (set! clock-cycles 3)
         ]
        [(and (= hb0 #b1001) ;;;;;;;;;;;; LPM Rd,Z+
              (= (& hb1 #b1110) 0)
              (= hb3 #b0101))
         (define z (get-z))
         (define z-val (flash-get-byte z))
         (define Rd (ior (&<< hb1 1 4) hb2))
         (sram-set-byte Rd z-val)
         (inc-z)
         (when debug?
           (print-instruction-uniquely OUT 'LPMRdZ+)
           (fprintf OUT "LPM R~a,Z+ [~a]" Rd (num->hex z-val)))
         (set! clock-cycles 3)
         ]
        [(= hb0 #b1110)  ;;;;;;;;;;;;; LDI load immediate
         (define K (ior (<< hb1 4) hb3))
         (define Rd (ior #x10 hb2))
         (sram-set-byte Rd K)
         (when debug?
           (print-instruction-uniquely OUT 'LDI)
           (fprintf OUT "LDI R~a,K[~a]" Rd (num->hex K)))
         (set! clock-cycles 1)
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;; LDD Rd <- (X)
              (= (<< hb1 -1) #b000)
              (= hb3 #b1100))
         (define Rd (ior (&<< hb1 1 4) hb2))
         (define x (get-x))
         (define x-val (sram-get-byte x))
         (sram-set-byte Rd x-val)
         (when debug?
           (print-instruction-uniquely OUT 'LDRdX)
           (fprintf OUT "LD R~a,X[~a]" Rd (num->hex x-val)))
         (set! clock-cycles 2)
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;; LDD Rd <- (X+)
              (= (<< hb1 -1) #b000)
              (= hb3 #b1101))
         (define Rd (ior (&<< hb1 1 4) hb2))
         (define x (get-x))
         (define x-val (sram-get-byte x))
         (sram-set-byte Rd x-val)
         (inc-x)
         (when debug?
           (print-instruction-uniquely OUT 'LDRdX+)
           (fprintf OUT "LD R~a,X+[~a]" Rd (num->hex x-val)))
         (set! clock-cycles 2)
         ]
        [(and (= hb0 #b1000)  ;;;;;;;;;;;;; LDD Rd <- (Z)
              (= (<< hb1 -1) #b000)
              (= hb3 0))
         (define Rd (ior (&<< hb1 1 4) hb2))
         (define z (get-z))
         (define z-val (sram-get-byte z))
         (sram-set-byte Rd z-val)
         (when debug?
           (print-instruction-uniquely OUT 'LDRdZ)
           (fprintf OUT "LD R~a,Z[~a]" Rd (num->hex z-val)))
         (set! clock-cycles 2)
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;; LDD Rd <- (Z+)
              (= (<< hb1 -1) #b000)
              (= hb3 #b0001))
         (define Rd (ior (&<< hb1 1 4) hb2))
         (define z (get-z))
         (define z-val (sram-get-byte z))
         (sram-set-byte Rd z-val)
         (inc-z)
         (when debug?
           (print-instruction-uniquely OUT 'LDRdZ+)
           (fprintf OUT "LD R~a,Z+[~a]" Rd (num->hex z-val)))
         (set! clock-cycles 2)
         ]
        [(and (= (& hb0 #b1101) #b1000)  ;;;;;;;;;;;;; LDD Rd <- (Z+q)
              (= (& hb1 #b0010) 0)
              (= (& hb3 #b1000) 0))
         (define Rd (ior (&<< hb1 1 4) hb2))
         (define q (ior (&<< hb0 #b0010 4)
                        (&<< hb1 #b1100 1)
                        (&   hb3 #b0111)))
         (define z (get-z))
         (define z-val (sram-get-byte (+ z q)))
         (sram-set-byte Rd z-val)
         (when debug?
           (print-instruction-uniquely OUT 'LDRdZ+q)
           (fprintf OUT "LD R~a,Z+~a[~a]" Rd q (num->hex z-val)))
         (set! clock-cycles 2)
         ]
        [(and (= hb0 #b1000)  ;;;;;;;;;;;;; LDD Rd <- (Y)
              (= (<< hb1 -1) #b000)
              (= hb3 #b1000))
         (define Rd (ior (&<< hb1 1 4) hb2))
         (define y (get-y))
         (define y-val (sram-get-byte y))
         (sram-set-byte Rd y-val)
         (when debug?
           (print-instruction-uniquely OUT 'LDRdY)
           (fprintf OUT "LD R~a,Y[~a]" Rd (num->hex y-val)))
         (set! clock-cycles 2)
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;; LDD Rd <- (Y+)
              (= (<< hb1 -1) #b000)
              (= hb3 #b1001))
         (define Rd (ior (&<< hb1 1 4) hb2))
         (define y (get-y))
         (define y-val (sram-get-byte y))
         (sram-set-byte Rd y-val)
         (inc-y)
         (when debug?
           (print-instruction-uniquely OUT 'LDRdY+)
           (fprintf OUT "LD R~a,Y+[~a]" Rd (num->hex y-val)))
         (set! clock-cycles 2)
         ]
        [(and (= (<< hb0 -2) #b10)  ;;;;;;;;;;;;; LDD Rd <- (Y+q)
              (n! hb0 0)
              (n! hb1 1)
              (! hb3 3))
         (define Rd (ior (&<< hb1 1 4) hb2))
         (define q (ior (&<< hb0 #b0010 4)
                        (&<< hb1 #b1100 1)
                        (&   hb3 #b0111)))
         (define y (get-y))
         (define val (sram-get-byte (+ y q)))
         (sram-set-byte Rd val)
         (when debug?
           (print-instruction-uniquely OUT 'LDRdY+q)
           (fprintf OUT "LD R~a,Y+~a[~a]" Rd q (num->hex val)))
         (set! clock-cycles 2)
         ]
        [(and (= hb0 #b0010)  ;;;;;;;;;;;;;;;;;;;;;;;; MOV
              (= (<<& hb1 -2) #b11))
         (define Rd (ior (&<< hb1 1 4) hb2))
         (define Rr (ior (&<< hb1 #b0010 3) hb3))
         (when debug? 
           (print-instruction-uniquely OUT 'MOV)
           (fprintf OUT "MOV R~a,R~a[~a]"
                    Rd Rr (num->hex (sram-get-byte Rr))))
         (sram-set-byte Rd (sram-get-byte Rr))
         (set! clock-cycles 1)
         ]
        [(and (= hb0 #b0000)  ;;;;;;;;;;;;;;;;;;;;;;;; MOVW
              (= hb1 #b0001))
         (define Rd (<< hb2 1)) (define Rd+ (+ Rd 1))
         (define Rr (<< hb3 1)) (define Rr+ (+ Rr 1))
         (define Rr-val (sram-get-byte Rr))
         (define Rr+-val (sram-get-byte Rr+))
         (sram-set-byte Rd Rr-val)
         (sram-set-byte Rd+ Rr+-val)
         (when debug?
           (print-instruction-uniquely OUT 'MOVW)
           (fprintf OUT "MOVW R~a:R~a,R~a[~a]:R~a[~a]"  
                    Rd Rd+ Rr 
                    (num->hexb Rr-val)
                    Rr+
                    (num->hexb Rr+-val)))
         (set! clock-cycles 1)
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;;;;; ST (X) <- Rr
              (= (<< hb1 -1) #b001)
              (= hb3 #b1100))
         (define Rr (ior (&<< hb1 1 4) hb2))
         (define x (get-x))
         (define Rr-val (sram-get-byte Rr))
         (sram-set-byte x Rr-val)
         (when debug?
           (print-instruction-uniquely OUT 'STXRr)
           (fprintf OUT "ST X,R~a[~a]" Rr (num->hex Rr-val)))
         (set! clock-cycles 2)]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;;;;; ST(X+) <- Rr
              (= (<< hb1 -1) #b001)
              (= hb3 #b1101))
         (define Rr (ior (&<< hb1 1 4) hb2))
         (define x (get-x))
         (define Rr-val (sram-get-byte Rr))
         (sram-set-byte x Rr-val)
         (inc-x)
         (when debug?
           (print-instruction-uniquely OUT 'STX+Rr)
           (fprintf OUT "ST X+,R~a[~a]" Rr (num->hex Rr-val)))
         (set! clock-cycles 2)
         ]
        [(and (= hb0 #b1000)  ;;;;;;;;;;;;;;;; ST (Z) <- Rr
              (= (<< hb1 -1) #b001)
              (= hb3 #b0000))
         (define Rr (ior (&<< hb1 1 4) hb2))
         (define z (get-z))
         (define Rr-val (sram-get-byte Rr))
         (sram-set-byte z Rr-val)
         (when debug?
           (print-instruction-uniquely OUT 'STZRr)
           (fprintf OUT "ST Z,R~a[~a]" Rr (num->hex Rr-val)))
         (set! clock-cycles 2)
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;;;;; ST(Z+) <- Rr
              (= (<< hb1 -1) #b001)
              (= hb3 #b0001))
         (define Rr (ior (&<< hb1 1 4) hb2))
         (define z (get-z))         
         (define Rr-val (sram-get-byte Rr))
         (sram-set-byte z Rr-val)
         (inc-z)
         (when debug?
           (print-instruction-uniquely OUT 'STZ+Rr)
           (fprintf OUT "ST Z+,R~a[~a]" Rr (num->hex Rr-val)))
         (set! clock-cycles 2)
         ]
        [(and (= (<< hb0 -2) #b10)  ;;;;;;;;;;;;; ST (Z+q) <- Rr
              (n! hb0 0)
              (! hb1 1)
              (n! hb3 3))
         (define Rr (ior (&<< hb1 1 4) hb2))
         (define q (ior (&<< hb0 #b0010 4)
                        (&<< hb1 #b1100 1)
                        (&   hb3 #b0111)))
         (define z (get-z))
         (sram-set-byte (+ z q) (sram-get-byte Rr))
         (when debug?
           (print-instruction-uniquely OUT 'STZ+qRr)
           (fprintf OUT "ST Z+~a,R~a[~a]"
                               q Rr (num->hex (sram-get-byte Rr))))
         (set! clock-cycles 2)
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;;;;; ST(Y+) <- Rr
              (= (<< hb1 -1) #b001)
              (= hb3 #b1001))
         (define Rr (ior (&<< hb1 1 4) hb2))
         (define y (get-y))
         (define Rr-val (sram-get-byte Rr))
         (sram-set-byte y Rr-val)
         (inc-y)
         (when debug?
           (print-instruction-uniquely OUT 'STY+Rr)
           (fprintf OUT "ST Y+,R~a[~a]" Rr (num->hex Rr-val)))
         (set! clock-cycles 2)
         ]
        [(and (= (<< hb0 -2) #b10)  ;;;;;;;;;;;;; STD (Y+q) <- Rr
              (n! hb0 0)
              (! hb1 1)
              (! hb3 3))
         (define Rr (ior (&<< hb1 1 4) hb2))
         (define q (ior (&<< hb0 #b0010 4)
                        (&<< hb1 #b1100 1)
                        (&   hb3 #b0111)))
         (define y (get-y))
         (sram-set-byte (+ y q) (sram-get-byte Rr))
         (when debug?
           (print-instruction-uniquely OUT 'STY+qRr)
           (fprintf OUT "ST Y+~a,R~a[~a]"
                               q Rr 
                               (num->hex (sram-get-byte Rr))))
         (set! clock-cycles 2)
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;;;;;;;;;;; STS (32-bit)
              (= (<<& hb1 -1 #b111) #b001)
              (= hb3 #b0000))
         (define Rr (ior (&<< hb1 1 4) hb2))
         (define Rr-val (sram-get-byte Rr))
         (define k (next-instruction))
         (when debug?
           (print-instruction-uniquely OUT 'STS)
           (fprintf OUT "STS (~a),R~a[~a]" (num->hex k) Rr (num->hex Rr-val)))
         (sram-set-byte k Rr-val)
         (inc-pc)
         (set! clock-cycles 2)
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;;;;;;;;;;;; LDS (32-bit)
              (= (<<& hb1 -1 #b111) #b000)
              (= hb3 #b0000))
         (define Rr (ior (&<< hb1 1 4) hb2))
         (define k (next-instruction))
         (define k-val (sram-get-byte k))
         (sram-set-byte Rr k-val)
         (when debug?
           (print-instruction-uniquely OUT 'LDS)
           (fprintf OUT "LDS R~a,(~a)[~a]" Rr (num->hex k) (num->hex k-val)))
         (inc-pc)
         (set! clock-cycles 2)
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;;;;;;;;;;;; JMP
              (= (<< hb1 -1) #b010)
              (= (<< hb3 -1) #b110))
         (define k (ior (&<< hb1 1 21)
                        (<<  hb2   17)
                        (&<< hb3 1 16)
                        (next-instruction)))
         (set! PC k)
         (when debug?
           (print-instruction-uniquely OUT 'JMP)
           (fprintf OUT "JMP"))
         (set! clock-cycles 3)
         ]
        [(= hb0 #b1100)  ;;;;;;;;;;;;;;;;;;;;;;; RJMP
         (define k (ior (<< hb1 8)
                        (<< hb2 4)
                        hb3))
         (define PC-now PC)
         (set! PC (& (+ PC k) #xfff))
         (when debug?
           (print-instruction-uniquely OUT 'RJMP)
           (fprintf OUT "RJMP ~a" (num->hex (- (& (+ PC k) #xfff) PC -1))))
         (set! clock-cycles 2)
         ]
        [(= hb0 #b1101)  ;;;;;;;;;;;;;;;;;;;;;;; RCALL
         (define k (2-complement->num 12
                                      (ior (<< hb1 8)
                                           (<< hb2 4)
                                           hb3)))
         (stack-push-word PC)
         (set! PC (+ PC k))         
         (when debug?
           (set! symbol-need-to-print? #f)
           (print-instruction-uniquely OUT 'RCALL)
           (fprintf OUT "RCALL ~a" (lookup-address PC))
           )
         (set! clock-cycles 3)
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;;;;;;;;;;;; CALL
              (= (& hb1 #b1110) #b0100)
              (= (& hb3 #b1110) #b1110))
         (define k (ior (&<< hb1 1 21)
                        (<<  hb2   17)
                        (&<< hb3 1 16)
                        (next-instruction)))
         (stack-push-word (+ PC 1))
         (set! PC k)
         (when debug?
           (print-instruction-uniquely OUT 'CALL)
           (fprintf OUT "CALL ~a" (lookup-address PC)))
         (set! clock-cycles 4)
         ]
        [(= opcode #b1001010100001000) ;;;;;;;;;; RET
         (set! PC (stack-pop-word))
         (when debug?
           (print-instruction-uniquely OUT 'RET)
           (fprintf OUT "RET"))
         (set! clock-cycles 4)
         ]
        [(and (= hb0 #b0010)  ;;;;;;;;;;;;;;;;;;;; EOR
              (= (<<& hb1 -2 #b11) #b01))
         (define Rd (ior (&<< hb1 1 4) hb2))
         (define Rr (ior (&<< hb1 #b0010 3) hb3))
         (define Rd-val (sram-get-byte Rd))
         (define Rr-val (sram-get-byte Rr))
         (define R (bitwise-xor Rd-val Rr-val))
         (sram-set-byte Rd R)
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
         (set! clock-cycles 1)
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;;;;;;;;; INC
              (= (<<& hb1 -1 #b111) #b010)
              (= hb3 #b0011))
         (define Rd (ior (&<< hb1 1 4) hb2))
         (define Rd-val (sram-get-byte Rd))
         (define R (& (+ Rd-val 1) #xff))
         (sram-set-byte Rd R)
         (if (= Rd-val #x7f) (sr-set-V) (sr-clear-V))
         (if (! R 7) (sr-set-N) (sr-clear-N))
         (if (one? (bitwise-xor (sr-get-N) (sr-get-V)))
             (sr-set-S) (sr-clear-S))
         (if (zero? R) (sr-set-Z) (sr-clear-Z))
         (when debug?
           (print-instruction-uniquely OUT 'INC)
           (fprintf OUT "INC R~a ; ~a" Rd (num->hex R)))
         (set! clock-cycles 1)
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;;;;;;;;; DEC
              (= (<<& hb1 -1 #b111) #b010)
              (= hb3 #b1010))
         (define Rd (ior (&<< hb1 1 4) hb2))
         (define Rd-val (sram-get-byte Rd))
         (define R (& (- Rd-val 1) #xff))
         (sram-set-byte Rd R)
         (if (= Rd-val #x80) (sr-set-V) (sr-clear-V))
         (if (! R 7) (sr-set-N) (sr-clear-N))
         (if (one? (bitwise-xor (sr-get-N) (sr-get-V)))
             (sr-set-S) (sr-clear-S))
         (if (zero? R) (sr-set-Z) (sr-clear-Z))
         (when debug?
           (print-instruction-uniquely OUT 'DEC)
           (fprintf OUT "DEC R~a[~a] ; ~a" 
                               Rd (num->hex Rd-val) (num->hex R)))
         (set! clock-cycles 1)
         ]
        [(and (= hb0 #b0001)  ;;;;;;;;;;;;;;;;;;;; CPSE
              (= (<<& hb1 -2 #b11) #b00))
         (define Rd (ior (&<< hb1 #b0001 4) hb2))
         (define Rr (ior (&<< hb1 #b0010 3) hb3))
         (define Rd-val (sram-get-byte Rd))
         (define Rr-val (sram-get-byte Rr))

         (when (= Rd-val Rr-val)
           (when (is-two-word-instruction? PC)
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
                               (= Rd-val Rr-val)))
         ]
        [(and (= hb0 #b1011)  ;;;;;;;;;;;;;;;;;;;; OUT
              (! hb1 3))
         (define A (ior (&<< hb1 #b0110 3)
                        hb3))
         (define Rr (ior (&<< hb1 #b0001 4) hb2))
         (define Rr-val (sram-get-byte Rr))
         (io-set A Rr-val)
         (when debug?
           (print-instruction-uniquely OUT 'OUT)
           (fprintf OUT "OUT A[~a],R~a[~a]" 
                    (num->hex A) Rr (num->hex Rr-val)))
         (set! clock-cycles 1)
         ]
        [(and (= hb0 #b1011)  ;;;;;;;;;;;;;;;;;;;; IN
              (n! hb1 3))
         (define A (ior (&<< hb1 #b0110 3)
                        hb3))
         (define A-val (io-get A))
         (define Rd (ior (&<< hb1 #b0001 4) hb2))
         (sram-set-byte Rd A-val)
         (when debug?
           (print-instruction-uniquely OUT 'IN)
           (fprintf OUT "IN R~a,A[~a]" 
                               Rd (num->hex A-val)))
         (set! clock-cycles 1)
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;;;;;;;;; PUSH
              (= (& hb1 #b1110) #b0010)
              (= hb3 #b1111))
         (define Rd (ior (&<< hb1 #b0001 4) hb2))
         (define Rd-val (sram-get-byte Rd))
         (stack-push Rd-val)
         (when debug?
           (print-instruction-uniquely OUT 'PUSH)
           (fprintf OUT "PUSH R~a" Rd))
         (set! clock-cycles 2)
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;;;;;;;;; POP
              (= (& hb1 #b1110) #b0000)
              (= hb3 #b1111))
         (define Rd (ior (&<< hb1 #b0001 4) hb2))
         (define Rd-old-val (sram-get-byte Rd))
         (define Rd-new-val (stack-pop))
         (sram-set-byte Rd Rd-new-val)
         (when debug?
           (print-instruction-uniquely OUT 'POP)
           (fprintf OUT "POP R~a" Rd ))
         (set! clock-cycles 2)
         ]
        [(= hb0 #b0011)  ;;;;;;;;;;;;;;;;;;;; CPI
         (define K (ior (<< hb1 4) hb3))
         (define Rd (ior #b10000 hb2))
         (define Rd-val (sram-get-byte Rd))
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
           (fprintf OUT "CPI R~a[~a],~a ; ~a" Rd (num->hex Rd-val) (num->hex K) (num->hex result)))
         (set! clock-cycles 1)
         ]
        [(and (= hb0 #b0001)  ;;;;;;;;;;;;;;;;;;;; CP
              (= (<< hb1 -2 ) #b01))
         (define Rd (ior (&<< hb1 #b0001 4) hb2))
         (define Rr (ior (&<< hb1 #b0010 3) hb3))
         (define Rd-val (sram-get-byte Rd))
         (define Rr-val (sram-get-byte Rr))
         (define R (& (- Rd-val Rr-val) #xff)) ;; result
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
         (if (zero? R) (sr-set-Z) (sr-clear-Z))
         (if (one? (ior (& (n-bit-ref Rd-val 7)
                           (bit-ref Rr-val 7))
                        (& (bit-ref Rr-val 7)
                           (bit-ref R 7))
                        (& (bit-ref R 7)
                           (n-bit-ref Rd-val 7))))
             (sr-set-C) (sr-clear-C))
         (when debug?
           (print-instruction-uniquely OUT 'CP)
           (fprintf OUT "CP R~a[~a],R~a[~a] ; ~a"
                               Rd (num->hex Rd-val) 
                               Rr (num->hex Rr-val)
                               (num->hex R)))
         (set! clock-cycles 1)
         ]
        [(and (= hb0 #b0000)  ;;;;;;;;;;;;;;;;;;;; CPC
              (= (<< hb1 -2 ) #b01))
         (define Rd (ior (&<< hb1 #b0001 4) hb2))
         (define Rr (ior (&<< hb1 #b0010 3) hb3))
         (define Rd-val (sram-get-byte Rd))
         (define Rr-val (sram-get-byte Rr))
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
         (set! clock-cycles 1)
         ]
        [(and (= hb0 #b0001)  ;;;;;;;;;;;;;;;;;;;; SUB
              (= (& hb1 #b1100) #b1000))
         (define Rd (get-Rd opcode))
         (define Rr (get-Rr opcode))
         (define Rd-val (sram-get-byte Rd))
         (define Rr-val (sram-get-byte Rr))
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
         (set! clock-cycles 1)
         ]

        [(= hb0 #b0101)  ;;;;;;;;;;;;;;;;;;;; SUBI
         (define Rd (ior #b10000 hb2))
         (define Rd-val (sram-get-byte Rd))
         (define K (ior (<< hb1 4) hb3))
         (define R (& (- Rd-val K) #xff))
         (sram-set-byte Rd R)
         (if (one? (ior (& (n-bit-ref Rd-val 3) (bit-ref K 3))
                        (& (bit-ref K 3) (bit-ref R 3))
                        (& (bit-ref R 3) (n-bit-ref Rd-val 3))))
             (sr-set-H) (sr-clear-H))
         (if (one? (ior (& (bit-ref Rd-val 7)
                           (n-bit-ref K 7) 
                           (n-bit-ref R 7))
                        (& (n-bit-ref Rd-val 7)
                           (bit-ref K 7) 
                           (bit-ref R 7))))
             (sr-set-V) (sr-clear-V))
         (if (! R 7) (sr-set-N) (sr-clear-N))
         (if (= (bitwise-xor (sr-get-N) (sr-get-V)) 1)
             (sr-set-S) (sr-clear-S))
         (if (zero? R) (sr-set-Z) (sr-clear-Z))
         (if (one? (ior (& (n-bit-ref Rd-val 7)
                           (bit-ref K 7))
                        (& (bit-ref K 7)
                           (bit-ref R 7))
                        (& (bit-ref R 7)
                           (n-bit-ref Rd-val 7))))
             (sr-set-C) (sr-clear-C))
         (when debug?
           (print-instruction-uniquely OUT 'SUBI)
           (fprintf OUT "SUBI R~a[~a],K[~a] ; ~a" 
                               Rd (num->hex Rd-val) (num->hex K) (num->hex R)))
         (set! clock-cycles 1)
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;;;;;;;;; ADIW
              (= hb1 #b0110))
         (define K (ior (&<< hb2 #b1100 2) hb3))
         (define Rd (+ 24 (&<< hb2 #b0011 1)))
         (define Rd+ (+ Rd 1))
         (define Rdh (sram-get-byte Rd+))
         (define Rdl (sram-get-byte Rd))
         (define Rd-val (ior (<< Rdh 8) Rdl))
         (define R (& (+ Rd-val K) #xffff))
         (sram-set-byte Rd+ (<< R -8))
         (sram-set-byte Rd (& R #xff))
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
         (set! clock-cycles 2)
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;;;;;;;;; SBIW
              (= hb1 #b0111))
         (define K (ior (&<< hb2 #b1100 2) hb3))
         (define Rd (+ 24 (&<< hb2 #b0011 1)))
         (define Rd+ (+ Rd 1))
         (define Rdh (sram-get-byte Rd+))
         (define Rdl (sram-get-byte Rd))
         (define Rd-val (ior (<< Rdh 8) Rdl))
         (define R (& (- Rd-val K) #xffff))
         (sram-set-byte Rd+ (<< R -8))
         (sram-set-byte Rd (& R #xff))
         (if (zero? R) (sr-set-Z) (sr-clear-Z))
         (if (one? (& (bit-ref R 15)
                      (n-bit-ref Rdh 7)))
             (sr-set-C) (sr-clear-C))
         (if (! R 15) (sr-set-N) (sr-clear-N))
         (if (one? (& (n-bit-ref R 15)
                      (bit-ref Rdh 7)))
             (sr-set-V) (sr-clear-V))
         (if (one? (bitwise-xor (sr-get-N) (sr-get-V)))
             (sr-set-S) (sr-clear-S))
         (when debug?
           (print-instruction-uniquely OUT 'SBIW)
           (fprintf OUT "SBIW R~a:R~a[~a],K[~a] ; ~a"
                               Rd Rd+
                               (num->hex Rd-val) 
                               (num->hex K) 
                               (num->hex R)))
         (set! clock-cycles 2)
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;;;;;;;;; CBI
              (= hb1 #b1000))
         (define b (& hb3 #b111))
         (define A (ior (<< hb2 1) 
                        (<< hb3 -3)))
         (io-clear-bit A b)
         (when debug?
           (print-instruction-uniquely OUT 'CBI)
           (fprintf OUT "CBI A[~a],b[~a]" (num->hex A) b))
         (set! clock-cycles 2)
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;;;;;;;;; SBI
              (= hb1 #b1010))
         (define b (& hb3 #b0111))
         (define A (ior (<< hb2 1)
                        (<< hb3 -3)))
         (io-set-bit A b)
         (when debug?
           (print-instruction-uniquely OUT 'SBI)
           (fprintf OUT "SBI A[~a],b[~a]"
                              (num->hex A) b))
         (set! clock-cycles 2)
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;;;;;;;;; SBIC
              (= hb1 #b1001))
         (define b (& hb3 #b111))
         (define A (ior (<< hb2 1)
                        (<< hb3 -3)))
         (define A-bit (io-get-bit A b))
         (when (zero? A-bit)
           (when (is-two-word-instruction? PC)
             (inc-pc)
             (set! clock-cycles (+ clock-cycles 1)))
           (inc-pc)
           (set! clock-cycles (+ clock-cycles 1)))
         (set! clock-cycles (+ clock-cycles 1))
         (when debug?
           (print-instruction-uniquely OUT 'SBIC clock-cycles)
           (fprintf OUT "SBIC A[~a],b[~a] ; ~a"
                              (num->hex A) b A-bit))
           
         ]
        [(= hb0 #b0100)  ;;;;;;;;;;;;;;;;;;;; SBCI
         (define Rd (ior #b10000 hb2))
         (define Rd-val (sram-get-byte Rd))
         (define K (ior (<< hb1 4) hb3))
         (define C (sr-get-C))
         (define R (& (- Rd-val K C) #xff))
         (sram-set-byte Rd R)
         (if (one? (ior (& (n-bit-ref Rd-val 3) (bit-ref K 3))
                        (& (bit-ref K 3) (bit-ref R 3))
                        (& (bit-ref R 3) (n-bit-ref Rd-val 3))))
             (sr-set-H) (sr-clear-H))
         (if (one? (ior (& (bit-ref Rd-val 7)
                           (n-bit-ref K 7) 
                           (n-bit-ref R 7))
                        (& (n-bit-ref Rd-val 7)
                           (bit-ref K 7) 
                           (bit-ref R 7))))
             (sr-set-V) (sr-clear-V))
         (if (! R 7) (sr-set-N) (sr-clear-N))
         (if (= (bitwise-xor (sr-get-N) (sr-get-V)) 1)
             (sr-set-S) (sr-clear-S))
         (unless (zero? R) (sr-clear-Z))
         (if (one? (ior (& (n-bit-ref Rd-val 7)
                           (bit-ref K 7))
                        (& (bit-ref K 7)
                           (bit-ref R 7))
                        (& (bit-ref R 7)
                           (n-bit-ref Rd-val 7))))
             (sr-set-C) (sr-clear-C))
         (when debug?
           (print-instruction-uniquely OUT 'SBCI)
           (fprintf OUT "SBCI R~a[~a],K[~a] ; ~a" 
                              Rd (num->hex Rd-val) (num->hex K) (num->hex R)))
         (set! clock-cycles 1)
         ]
        [(and (= hb0 #b1111)  ;;;;;;;;;;;;;;;;;;;; SBRC
              (= (& hb1 #b1110) #b1100)
              (= (& hb3 #b1000) #b0000))
         (define Rr (ior (&<< hb1 #b0001 4) hb2))
         (define b (& hb3 #b111))
         (define Rr-bit (sram-get-bit Rr b))
         (when (zero? Rr-bit)
           (when (is-two-word-instruction? PC)
             (inc-pc)
             (set! clock-cycles (+ clock-cycles 1)))
           (inc-pc)
           (set! clock-cycles (+ clock-cycles 1)))
         (set! clock-cycles (+ clock-cycles 1))
         (when debug?
           (print-instruction-uniquely OUT 'SBRC clock-cycles)
           (fprintf OUT "SBRC R~a,b[~a]" Rr b))
         ]
        [(and (= hb0 #b1111)  ;;;;;;;;;;;;;;;;;;;; BRNE
              (= (<< hb1 -2 ) #b01)
              (= (& hb3 #b111) #b001))
         (define k (2-complement->num 7
                                      (ior (&<< hb1 #b11 5)
                                           (<< hb2 1)
                                           (&<< hb3 #b1000 -3))))
         (when (zero? (sr-get-Z))
           (set! PC (+ PC k))
           (set! clock-cycles (+ clock-cycles 1)))
         (set! clock-cycles (+ clock-cycles 1))
         (when debug?
           (print-instruction-uniquely OUT 'BRNE clock-cycles)
           (fprintf OUT "BRNE ~a" (zero? (sr-get-Z))))
         ]
        [(and (= hb0 #b1111)  ;;;;;;;;;;;;;;;;;;;; BREQ
              (= (<< hb1 -2 ) #b00)
              (= (& hb3 #b111) #b001))
         (define k (2-complement->num 7
                                      (ior (&<< hb1 #b11 5)
                                           (<< hb2 1)
                                           (&<< hb3 #b1000 -3))))
         (define condition? (one? (sr-get-Z)))
         (when condition?
           (set! PC (+ PC k))
           (set! clock-cycles (+ clock-cycles 1)))
         (set! clock-cycles (+ clock-cycles 1))
         (when debug?
           (print-instruction-uniquely OUT 'BREQ clock-cycles)
           (fprintf OUT "BREQ ~a" condition?))
         ]
        [(and (= hb0 #b1111)  ;;;;;;;;;;;;;;;;;;;; BRCS
              (= (<< hb1 -2 ) #b00)
              (= (& hb3 #b111) #b000))
         (define k (2-complement->num 7
                                      (ior (&<< hb1 #b11 5)
                                           (<< hb2 1)
                                           (&<< hb3 #b1000 -3))))
         (define C (sr-get-C))
         (when (one? C)
           (set! PC (+ PC k))
           (set! clock-cycles (+ clock-cycles 1)))
         (set! clock-cycles (+ clock-cycles 1))
         (when debug?
           (print-instruction-uniquely OUT 'BRCS clock-cycles)
           (fprintf OUT "BRCS ~a" (one? C)))
         ]
        [(and (= hb0 #b1111)  ;;;;;;;;;;;;;;;;;;;; BRTC
              (= (<< hb1 -2 ) #b01)
              (= (& hb3 #b0111) #b110))
         (define k (2-complement->num 7
                                      (ior (&<< hb1 #b11 5)
                                           (<< hb2 1)
                                           (&<< hb3 #b1000 -3))))
         (define T (sr-get-T))
         (when (zero? T)
           (set! PC (+ PC k))
           (set! clock-cycles (+ clock-cycles 1)))
         (set! clock-cycles (+ clock-cycles 1))
         (when debug?
           (print-instruction-uniquely OUT 'BRTC clock-cycles)
           (fprintf OUT "BRTC ~a" (zero? T)))
         ]
        [(and (= hb0 #b1111)  ;;;;;;;;;;;;;;;;;;;; BRTS
              (= (<< hb1 -2 ) #b00)
              (= (& hb3 #b0111) #b110))
         (define k (2-complement->num 7
                                      (ior (&<< hb1 #b11 5)
                                           (<< hb2 1)
                                           (&<< hb3 #b1000 -3))))
         (define T (sr-get-T))
         (when (one? T)
           (set! PC (+ PC k))
           (set! clock-cycles (+ clock-cycles 1)))
         (set! clock-cycles (+ clock-cycles 1))
         (when debug?
           (print-instruction-uniquely OUT 'BRTS clock-cycles)
           (fprintf OUT "BRTS ~a" (one? T)))
         ]
        [(and (= hb0 #b1111)  ;;;;;;;;;;;;;;;;;;;; BRCC
              (= (& hb1 #b1100) #b0100)
              (= (& hb3 #b0111) #b000))
         (define k (2-complement->num 7
                                      (ior (&<< hb1 #b11 5)
                                           (<< hb2 1)
                                           (&<< hb3 #b1000 -3))))
         (define C (sr-get-C))
         (when (zero? C)
           (set! PC (+ PC k))
           (set! clock-cycles (+ clock-cycles 1)))
         (set! clock-cycles (+ clock-cycles 1))
         (when debug?
           (print-instruction-uniquely OUT 'BRCC clock-cycles)
           (fprintf OUT "BRCC ~a" (zero? C)))]
        [(and (= hb0 0)  ;;;;;;;;;;;;;;;;;;;; ADD without carry
              (= (& hb1 #b1100) #b1100))
         (define Rd (ior (&<< hb1 #b0001 4) hb2))
         (define Rr (ior (&<< hb1 #b0010 3) hb3))
         (define Rd-val (sram-get-byte Rd))
         (define Rr-val (sram-get-byte Rr))
         (define R (& (+ Rd-val Rr-val) #xff))
         (sram-set-byte Rd R)
         (if (zero? R) (sr-set-Z) (sr-clear-Z))
         (if (one? (ior (& (bit-ref Rd-val 7)
                           (bit-ref Rr-val 7))
                        (& (bit-ref Rr-val 7)
                           (n-bit-ref R 7))
                        (& (n-bit-ref R 7)
                           (bit-ref Rd-val 7))))
             (sr-set-C) (sr-clear-C))
         (if (! R 7) (sr-set-N) (sr-clear-N))
         (if (one? (ior (& (bit-ref Rd-val 7)
                           (bit-ref Rr-val 7)
                           (n-bit-ref R 7))
                        (& (n-bit-ref Rd-val 7)
                           (n-bit-ref Rr-val 7)
                           (bit-ref R 7))))
             (sr-set-V) (sr-clear-V))
         (if (one? (bitwise-xor (sr-get-N) (sr-get-V)))
             (sr-set-S) (sr-clear-S))
         (if (one? (ior (& (bit-ref Rd-val 3)
                          (bit-ref Rr-val 3))
                       (& (bit-ref Rr-val 3)
                          (n-bit-ref R 3))
                       (& (n-bit-ref R 3)
                          (bit-ref Rd-val 3))))
             (sr-set-H)(sr-clear-H))
         (when debug?
           (print-instruction-uniquely OUT 'ADD)
           (fprintf OUT "ADD R~a[~a],R~a[~a] ; ~a" 
                    Rd (num->hexb Rd-val)
                    Rr (num->hexb Rr-val)
                    (num->hexb R)))
         (set! clock-cycles 1)
         ]
        [(and (= hb0 1)  ;;;;;;;;;;;;;;;;;;;; ADC with carry
              (= (& hb1 #b1100) #b1100))
         (define Rd (ior (&<< hb1 #b0001 4) hb2))
         (define Rr (ior (&<< hb1 #b0010 3) hb3))
         (define Rd-val (sram-get-byte Rd))
         (define Rr-val (sram-get-byte Rr))
         (define C (sr-get-C))
         (define R (& (+ Rd-val Rr-val C) #xff))
         (sram-set-byte Rd R)
         (if (zero? R) (sr-set-Z) (sr-clear-Z))
         (if (one? (ior (& (bit-ref Rd-val 7)
                           (bit-ref Rr-val 7))
                        (& (bit-ref Rr-val 7)
                           (n-bit-ref R 7))
                        (& (n-bit-ref R 7)
                           (bit-ref Rd-val 7))))
             (sr-set-C) (sr-clear-C))
         (if (! R 7) (sr-set-N) (sr-clear-N))
         (if (one? (ior (& (bit-ref Rd-val 7)
                           (bit-ref Rr-val 7)
                           (n-bit-ref R 7))
                        (& (n-bit-ref Rd-val 7)
                           (n-bit-ref Rr-val 7)
                           (bit-ref R 7))))
             (sr-set-V) (sr-clear-V))
         (if (one? (bitwise-xor (sr-get-N) (sr-get-V)))
             (sr-set-S) (sr-clear-S))
         (if (one? (ior (& (bit-ref Rd-val 3)
                          (bit-ref Rr-val 3))
                       (& (bit-ref Rr-val 3)
                          (n-bit-ref R 3))
                       (& (n-bit-ref R 3)
                          (bit-ref Rd-val 3))))
             (sr-set-H)(sr-clear-H))
         (when debug? 
           (print-instruction-uniquely OUT 'ADC)
           (fprintf OUT "ADC R~a[~a],R~a[~a],C[~a] ; ~a" 
                    Rd (num->hexb Rd-val) 
                    Rr (num->hexb Rr-val)
                    C (num->hexb R)))
         (set! clock-cycles 1)
         ]
        [(and (= hb0 #b0010)  ;;;;;;;;;;;;;;;;;;;; AND
              (= (& hb1 #b1100) 0))
         (define Rd (ior (&<< hb1 #b0001 4) hb2))
         (define Rr (ior (&<< hb1 #b0010 3) hb3))
         (define Rd-val (sram-get-byte Rd))
         (define Rr-val (sram-get-byte Rr))
         (define result (& Rd-val Rr-val))
         (sram-set-byte Rd result)
         (sr-clear-V)
         (if (! result 7) (sr-set-N) (sr-clear-N))
         (if (one? (bitwise-xor (sr-get-N) (sr-get-V)))
             (sr-set-S) (sr-clear-S))
         (if (zero? result) (sr-set-Z) (sr-clear-Z))
         (when debug?
           (print-instruction-uniquely OUT 'AND)
           (fprintf OUT "AND R~a[~a],R~a[~a]" Rd (num->hex Rd-val) Rr (num->hex Rr-val)))
         (set! clock-cycles 1)
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;;;;;;;;; ROR
              (= (& hb1 #b1110) #b0100)
              (= hb3 #b0111))
         (define Rd (ior (&<< hb1 #b0001 4) hb2))
         (define C (sr-get-C))
         (define Rd-val (sram-get-byte Rd))
         (define low-bit (bit-ref Rd-val 0))
         (define R (ior (<< C 7) (<< Rd-val -1)))
         (sram-set-byte Rd R)
         (if (one? low-bit) (sr-set-C) (sr-clear-C))
         (if (one? C) (sr-set-N) (sr-clear-N))
         (if (one? (bitwise-xor (sr-get-N) low-bit))
             (sr-set-V) (sr-clear-V))
         (if (one? (bitwise-xor (sr-get-N) (sr-get-V)))
             (sr-set-S) (sr-clear-S))
         (if (zero? R) (sr-set-Z) (sr-clear-Z))
         (when debug?
           (print-instruction-uniquely OUT 'ROR)
           (fprintf OUT "ROR R~a[~a] ; ~a" Rd (num->hex Rd-val) (num->hex R)))
         (set! clock-cycles 1)
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;;;;;;;;; LSR
              (= (& hb1 #b1110) #b0100)
              (= hb3 #b0110))
         (define Rd (ior (&<< hb1 #b0001 4) hb2))
         (define Rd-val (sram-get-byte Rd))
         (define R (<< Rd-val -1))
         (sram-set-byte Rd R)
         (if (! Rd-val 0) (sr-set-C) (sr-clear-C))
         (if (zero? R) (sr-set-Z) (sr-clear-Z))
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
         (set! clock-cycles 1)
         ]
        [(and (= hb0 #b1111) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BST
              (= (& hb1 #b1110) #b1010)
              (= (& hb3 #b1000) 0))
         (define Rd (ior (&<< hb1 #b0001 4) hb2))
         (define Rd-val (sram-get-byte Rd))
         (define b (& hb3 #b0111))
         (if (! Rd-val b) (sr-set-T) (sr-clear-T))
         (when debug?
           (print-instruction-uniquely OUT 'BST)
           (fprintf OUT "BST R~a[~a],b[~a] ; ~a" 
                               Rd (num->hex Rd-val)
                               b (sr-get-T)))
         (set! clock-cycles 1)
         ]
        [(= opcode #x94f8)  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CLI
         (sr-clear-I)
         (when debug?
           (print-instruction-uniquely OUT 'CLI)
           (fprintf OUT "CLI"))
         (set! clock-cycles 1)]
        [(= opcode #b1001010010001000)  ;;;;;;;;;;;;;;;;;;;; CLC
         (sr-clear-C)
         (when debug?
           (print-instruction-uniquely OUT 'CLC)
           (fprintf OUT "CLC"))
         (set! clock-cycles 1)]
        [(= opcode #b1001010011101000)  ;;;;;;;;;;;;;;;;;;;; CLT
         (sr-clear-T)
         (when debug?
           (print-instruction-uniquely OUT 'CLT)
           (fprintf OUT "CLT"))
         (set! clock-cycles 1)]
        [(= opcode #b1001010000001000)  ;;;;;;;;;;;;;;;;;;;; SEC
         (sr-set-C)
         (when debug?
           (print-instruction-uniquely OUT 'SEC)
           (fprintf OUT "SEC"))
         (set! clock-cycles 1)]
        [(= opcode #b1001010001101000)  ;;;;;;;;;;;;;;;;;;;; SET
         (sr-set-T)
         (when debug?
           (print-instruction-uniquely OUT 'SET)
           (fprintf OUT "SET"))
         (set! clock-cycles 1)]
        [(and (= hb0 0)  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SBC
              (= (& hb1 #b1100) #b1000))
         (define Rd (ior (&<< hb1 #b0001 4) hb2))
         (define Rr (ior (&<< hb1 #b0010 3) hb3))
         (define Rd-val (sram-get-byte Rd))
         (define Rr-val (sram-get-byte Rr))
         (define C (sr-get-C))
         (define R (& (- Rd-val Rr-val C) #xff))
         (sram-set-byte Rd R)
         (if (one? (ior (& (n-bit-ref Rd-val 3) (bit-ref R 3))
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
              (print-instruction-uniquely OUT 'SBC)
              (fprintf OUT "SBC R~a[~a],R~a[~a],C[~a] ; ~a"
                                 Rd (num->hex Rd-val) 
                                 Rr (num->hex Rr-val) C 
                                 (num->hex R)))
         (set! clock-cycles 1)]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SWAP
              (= (& hb1 #b1110) #b0100)
              (= hb3 #b0010))
         (define Rd (ior (&<< hb1 #b0001 4) hb2))
         (define Rd-val (sram-get-byte Rd))
         (define R (ior (&<< Rd-val #x0f 4)
                        (&<< Rd-val #xf0 -4)))
         (sram-set-byte Rd R)
         (when debug?
              (print-instruction-uniquely OUT 'SWAP)
              (fprintf OUT "SWAP R~a[~a] ; ~a"
                                 Rd (num->hex Rd-val) (num->hex R)))
         (set! clock-cycles 1)]
        [(and (= hb0 #b1001) ;;;;;;;;;;;;;;;;;;;; MUL
              (= (& hb1 #b1100) #b1100))
         (define Rd (get-Rd opcode))
         (define Rr (get-Rr opcode))
         (define Rd-val (sram-get-byte Rd))
         (define Rr-val (sram-get-byte Rr))
         (define R (* Rd-val Rr-val))
         ;; save the result in R1:R0
         (sram-set-byte 0 (& R #xff))
         (sram-set-byte 1 (<<& R -8 #xff))
         (compute-Z R)
         (if (! R 15) (sr-set-C) (sr-clear-C))
         (when debug?
           (print-instruction-uniquely OUT 'MUL)
           (fprintf OUT "MUL R~a[~a],R~a[~a] ; ~a"
                                 Rd (num->hex Rd-val) 
                                 Rr (num->hex Rr-val) 
                                 (num->hex R)))
         (set! clock-cycles 2)
         ]
        [(and (= hb0 #b0111)) ;;;;;;;;;;;;;;;;;;;; ANDI
         (define Rd (+ hb2 16)) ;; registers 16...31
         (define Rd-val (sram-get-byte Rd))
         (define K (ior (<< hb1 4) hb3))
         (define R (& Rd-val K))
         (sram-set-byte Rd R)
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
         (set! clock-cycles 1)
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;;;;;;;;; NEG
              (= (& hb1 #b1110) #b0100)
              (= hb3 #b0001))
         (define Rd (get-Rd opcode))
         (define Rd-val (sram-get-byte Rd))
         (define R (& (- Rd-val) #xff))
         (sram-set-byte Rd R)
         (if (one? (ior (bit-ref R 3) 
                        (n-bit-ref Rd-val 3)))
             (sr-set-H) (sr-clear-H))
         (if (= R #x80) (sr-set-V)(sr-clear-V))
         (compute-N R)
         (compute-S)
         (compute-Z R)
         (if (zero? R) (sr-clear-C)(sr-set-C))
         (when debug?
           (print-instruction-uniquely OUT 'NEG)
           (fprintf OUT "NEG R~a[~a] ; ~a"
                                 Rd (num->hex Rd-val) 
                                 (num->hex R)))
         (set! clock-cycles 1)]
        [(zero? opcode)  ;;;;;;;;;;;;;;;;;;;; NOP
         (when debug?
           (print-instruction-uniquely OUT 'NOP)
           (fprintf OUT "NOP"))
         (set! clock-cycles 1)]
        [else
         (dec-pc)
          (fprintf OUT "UNKNOWN ~a ~a ~a ~a" 
                  (num->bin hb0)(num->bin hb1)(num->bin hb2)(num->bin hb3))
         ])
  (when debug?    
    (when (and symbol symbol-need-to-print?) 
      (fprintf OUT " ;; ~a" symbol))
    (fprintf OUT "~n"))
  (set! CURRENT-CLOCK-CYCLE (+ CURRENT-CLOCK-CYCLE clock-cycles))
  )

(define (go-address address)
  (run)
  (let loop ()
    (if (= PC address)
        (run)
        (begin (run)
               (loop)))))

(define (print-registers)
  (for ([r 32])
    (printf "R~a = ~a~n" r (num->hex (sram-get-byte r)))))
