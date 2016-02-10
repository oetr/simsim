(require racket racket/main)
;; develop an abstract machine that is loaded with a hex /elf file,
;; is given a start address, and can simulate the execution

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2) hex file reader
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (hex->num hex) (string->number hex 16))
(define (num->hex num) (number->string num 16))
(define (2-complement->num width num)
  (if (! num (- width 1))
        (+ (- (<< 1 (- width 1)))
         (bitwise-bit-field num 0 (- width 1)))
      num))

(define (bin->num bin) (string->number bin 2))
(define (num->bin num) (number->string num 2))

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
;; get and set word use word addresses
(define (flash-get-word addr) (vector-ref FLASH addr))
(define (flash-set-word addr val) (vector-set! FLASH addr val))
;; get-byte use byte addresses
(define (flash-get-byte addr)
  (define word (flash-get-word (arithmetic-shift addr -1)))
  (if (bitwise-bit-set? addr 0)      
      (arithmetic-shift word -8)
      (bitwise-and word #x00ff)))
(num->hex (flash-get-word #x0d))

(define RAMEND (+ #x045f 1))
(define SRAM (make-vector RAMEND #xff))
;; get and set bytes
(define (sram-get-byte addr) (vector-ref SRAM addr))
(define (sram-set-byte addr val) (vector-set! SRAM addr val))
(define (sram-print)
  (for ([addr RAMEND])
    (printf "~a " (num->hex (sram-get-byte addr)))))
(define (sram-set-bit addr i)
  (vector-set! SRAM addr (ior (vector-ref SRAM addr) (<< 1 i))))
(define (sram-clear-bit addr i)
  (vector-set! SRAM addr (& (vector-ref SRAM addr) 
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



;; map I/O
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
(define (num->hexb num)
  (if (<= num #xf) 
      (string-append "0" (num->hex num))
      (num->hex num)))
(define (print-sram)
  (printf "    ")
  (for ([i #x10])
    (printf "~a  " (num->hex i)))
  (define accumulated-bytes (bytes))
  (define i 0)
  (for ([addr RAMEND])
    (when (zero? (modulo addr #x10))
      (printf "~a~n~a " accumulated-bytes
              (num->hex (quotient addr #xf)))
      (when (< (quotient addr #xf) #x10) (printf " "))
      (set! accumulated-bytes (bytes)))
    (define byte (sram-get-byte addr))
    (define char (bytes byte))
    (when (or (< byte 32) (> byte 126))
        (set! char #"."))
    (set! accumulated-bytes (bytes-append accumulated-bytes char))
    (printf "~a " (num->hexb byte))))
(define (print-io)
  (printf "   ")
  (for ([i #x10])
    (printf "~a  " (num->hex i)))
  (define i 0)
  (for ([addr #x40])
    (when (zero? (modulo addr #x10))
      (printf "~n~a " (num->hex (quotient addr #xf))))
    (printf "~a " (num->hexb (io-get addr)))))

;; Status register: GET
(define (sr-get)   (sram-get-byte #x5F))
(define (sr-get-I) (sram-get-bit #x5F 7))
(define (sr-get-T) (sram-get-bit #x5F 6))
(define (sr-get-H) (sram-get-bit #x5F 5))
(define (sr-get-S) (sram-get-bit #x5F 4))
(define (sr-get-V) (sram-get-bit #x5F 3))
(define (sr-get-N) (sram-get-bit #x5F 2))
(define (sr-get-Z) (sram-get-bit #x5F 1))
(define (sr-get-C) (sram-get-bit #x5F 0))
;; Status register: SET
(define (sr-set-I) (sram-set-bit #x5F 7))
(define (sr-set-T) (sram-set-bit #x5F 6))
(define (sr-set-H) (sram-set-bit #x5F 5))
(define (sr-set-S) (sram-set-bit #x5F 4))
(define (sr-set-V) (sram-set-bit #x5F 3))
(define (sr-set-N) (sram-set-bit #x5F 2))
(define (sr-set-Z) (sram-set-bit #x5F 1))
(define (sr-set-C) (sram-set-bit #x5F 0))
;; Status register: CLEAR
(define (sr-clear-I) (sram-clear-bit #x5F 7))
(define (sr-clear-T) (sram-clear-bit #x5F 6))
(define (sr-clear-H) (sram-clear-bit #x5F 5))
(define (sr-clear-S) (sram-clear-bit #x5F 4))
(define (sr-clear-V) (sram-clear-bit #x5F 3))
(define (sr-clear-N) (sram-clear-bit #x5F 2))
(define (sr-clear-Z) (sram-clear-bit #x5F 1))
(define (sr-clear-C) (sram-clear-bit #x5F 0))


;; program counter
(define PC 0)
(define (inc-pc) (set! PC (+ PC 1)))
(define (dec-pc) (set! PC (- PC 1)))
(define (next-instruction) (flash-get-word PC))

;; Stack
(define (get-sp)
  (ior (<< (sram-get-byte #x5e) 8)
       (sram-get-byte #x5d)))
(define (inc-sp (n -2))
  (define sp (+ (get-sp) n))
  (sram-set-byte #x5e (<<& sp -8 #xff))
  (sram-set-byte #x5d (& sp #xff)))
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
  (when debug?
      (fprintf OUT "{STACK PUSH: sp: ~a, low: ~a, high: ~a}" 
              sp (num->hex word-low) (num->hex word-high)))
  (sram-set-byte sp word-low)
  (sram-set-byte (- sp 1) word-high)
  (inc-sp -2))
(define (stack-pop-word)
  (define sp (+ (get-sp) 1))
  (define word-high (sram-get-byte sp))
  (define word-low  (sram-get-byte (+ sp 1)))
  (when debug?
      (fprintf OUT "{STACK POP: sp: ~a, low: ~a, high: ~a}" 
              sp (num->hex word-low) (num->hex word-high)))
  (inc-sp 2)
  (ior (<< word-high 8)
       word-low))

(define OUT (current-output-port))

(define (reset-machine (filename #f))
  (set! FLASH (make-vector FLASHEND #xff))
  (set! SRAM (make-vector RAMEND #xff))
  (for ([i #x60]) (vector-set! SRAM i 0))
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

;; make sure to fetch 32-bit instructions properly

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


(define (is-two-word-instruction? addr)
  #f)

(define (fetch-and-decode)
  ;; fetch
  (define opcode (next-instruction))
  (define hb3 (& opcode #xf))
  (define hb2 (<<& opcode -4 #xf))
  (define hb1 (<<& opcode -8 #xf))
  (define hb0 (<<& opcode -12 #xf))
  ;;(when (not debug?) 
    ;;(fprintf OUT "\r~a" iteration) )
  (when debug? 
    (fprintf OUT "[~a] ~a: " (num->hex PC) (num->hex opcode)))
  ;;(set! iteration (+ iteration 1))
  (inc-pc)
  (cond [(= opcode #x95c8) ;;;;;;;;;;;; LPM
         (define z (get-z))
         (define z-val (flash-get-byte z))
         (sram-set-byte 0 z-val)
         (when debug? (fprintf OUT "LPM R0 <- ~a[~a]" z (num->hex z-val)))
         ]
        [(and (= hb0 #b1001) ;;;;;;;;;;;; LPM 
              (= (& hb1 #b1110) 0)
              (= hb3 #b0101))
         (define z (get-z))
         (define z-val (flash-get-byte z))
         (define Rd (ior (&<< hb1 1 4) hb2))
         (sram-set-byte Rd z-val)
         (inc-z)
         (when debug? (fprintf OUT "LPM R~a <- Z+ ~a [~a]" Rd z (num->hex z-val)))
         ]
        [(= hb0 #b1110)  ;;;;;;;;;;;;; LDI load immediate
         (define K (ior (<< hb1 4) hb3))
         (define Rd (ior #x10 hb2))
         (sram-set-byte Rd K)
         (when debug? (fprintf OUT "LDI R~a <- K[~a]" Rd (num->hex K)))
         ]
        [(and (= hb0 #b1000)  ;;;;;;;;;;;;; LDD Rd <- (Z)
              (= (<< hb1 -1) #b000)
              (= hb3 0))
         (define Rd (ior (&<< hb1 1 4) hb2))
         (define z (get-z))
         (define z-val (sram-get-byte z))
         (sram-set-byte Rd z-val)
         (when debug? (fprintf OUT "LD R~a <- Z(~a)[~a]" Rd z (num->hex z-val)))
         ]
        [(and (= (& hb0 #b1101) #b1000)  ;;;;;;;;;;;;; LDD Rd <- (Z+q)
              (= (& hb1 #b0010) 0)
              (= (& hb3 #b1000) 0))
         (when debug? (fprintf OUT "LD "))
         (define Rd (ior (&<< hb1 1 4) hb2))
         (define q (ior (&<< hb0 #b0010 4)
                        (&<< hb1 #b1100 1)
                        (&   hb3 #b0111)))
         (define z (get-z))
         (define z-val (sram-get-byte (+ z q)))
         (sram-set-byte Rd z-val)
         (when debug? (fprintf OUT "R~a <- Z+~a(~a)[~a]" Rd q (+ z q) (num->hex z-val)))
         ]
        [(and (= hb0 #b1000)  ;;;;;;;;;;;;; LDD Rd <- (Y)
              (= (<< hb1 -1) #b000)
              (= hb3 #b1000))
         (define Rd (ior (&<< hb1 1 4) hb2))
         (define y (get-y))
         (define y-val (sram-get-byte y))
         (sram-set-byte Rd y-val)
         (when debug? (fprintf OUT "LD R~a <- Y(~a)[~a]" Rd y (num->hex y-val)))
         ]
        [(and (= (<< hb0 -2) #b10)  ;;;;;;;;;;;;; LDD Rd <- (Y+k)
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
         (when debug? (fprintf OUT "LD R~a <- Y+~a(~a)[~a]" Rd q (+ y q) 
                               (num->hex val)))
         ]
        [(and (= hb0 #b0010)  ;;;;;;;;;;;;;;;;;;;;;;;; MOV
              (= (<<& hb1 -2) #b11))
         (define Rd (ior (&<< hb1 1 4) hb2))
         (define Rr (ior (&<< hb1 #b0010 3) hb3))
         (when debug? (fprintf OUT "MOV R~a,R~a ; R~a[~a] <- R~a[~a]"
                               Rd Rr Rd
                               (num->hex (sram-get-byte Rd))
                               Rr (num->hex (sram-get-byte Rr))))
         (sram-set-byte Rd (sram-get-byte Rr))
         ]
        [(and (= hb0 #b0000)  ;;;;;;;;;;;;;;;;;;;;;;;; MOVW
              (= hb1 #b0001))
         (define Rd (<< hb2 1)) (define Rd+ (+ Rd 1))
         (define Rr (<< hb3 1)) (define Rr+ (+ Rr 1))
         (sram-set-byte Rd (sram-get-byte Rr))
         (sram-set-byte Rd+ (sram-get-byte Rr+))
         (when debug? (fprintf OUT "MOVW R~a:R~a,R~a:R~a "  Rd Rd+ Rr Rr+ ))
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;;;;; ST(X+) <- Rr
              (= (<< hb1 -1) #b001)
              (= hb3 #b1101))
         (define Rr (ior (&<< hb1 1 4) hb2))
         (define x (get-x))
         (define Rr-val (sram-get-byte Rr))
         (sram-set-byte x Rr-val)
         (inc-x)
         (when debug? (fprintf OUT "ST X+(~a) <- R~a[~a]" x Rr (num->hex Rr-val)))
         ]
        [(and (= hb0 #b1000)  ;;;;;;;;;;;;;;;; ST (Z) <- Rr
              (= (<< hb1 -1) #b001)
              (= hb3 #b0000))
         (define Rr (ior (&<< hb1 1 4) hb2))
         (define z (get-z))
         (define Rr-val (sram-get-byte Rr))
         (sram-set-byte z Rr-val)
         (when debug? (fprintf OUT "ST Z <- R~a[~a]" Rr (num->hex Rr-val)))
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;;;;; ST(Z+) <- Rr
              (= (<< hb1 -1) #b001)
              (= hb3 #b0001))
         (define Rr (ior (&<< hb1 1 4) hb2))
         (define z (get-z))         
         (define Rr-val (sram-get-byte Rr))
         (sram-set-byte z Rr-val)
         (inc-z)
         (when debug? (fprintf OUT "ST Z+(~a) <- R~a[~a]" z Rr (num->hex Rr-val)))
         ]
        [(and (= (<< hb0 -2) #b10)  ;;;;;;;;;;;;; STD (Z+q) <- Rr
              (n! hb0 0)
              (! hb1 1)
              (n! hb3 3))
         (define Rr (ior (&<< hb1 1 4) hb2))
         (define q (ior (&<< hb0 #b0010 4)
                        (&<< hb1 #b1100 1)
                        (&   hb3 #b0111)))
         (define z (get-z))
         (sram-set-byte (+ z q) (sram-get-byte Rr))
         (when debug? (fprintf OUT "STD Z+~a(~a) <- R~a[~a]"
                               q (+ z q) Rr (num->hex (sram-get-byte Rr))))
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
         (when debug? (fprintf OUT "STD Y+~a(~a) <- R~a[~a]" q (+ y q) Rr 
                               (num->hex (sram-get-byte Rr))))
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;;;;;;;;;;; STS (32-bit)
              (= (<<& hb1 -1 #b111) #b001)
              (= hb3 #b0000))
         (define Rr (ior (&<< hb1 1 4) hb2))
         (define Rr-val (sram-get-byte Rr))
         (define k (next-instruction))
         (when debug? (fprintf OUT "STS (~a) <- R~a[~a]" k Rr (num->hex Rr-val)))
         (sram-set-byte k Rr-val)
         (inc-pc)
         ]
        [(and (= hb0 #b1001)  ;; LDS (32-bit)
              (= (<<& hb1 -1 #b111) #b000)
              (= hb3 #b0000))
         (define Rr (ior (&<< hb1 1 4) hb2))
         (define k (next-instruction))
         (define k-val (sram-get-byte k))
         (sram-set-byte Rr k-val)
         (when debug? (fprintf OUT "LDS R~a <- ~a[~a]" Rr k (num->hex k-val)))
         (inc-pc)
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;;;;;;;;;;;; JMP
              (= (<< hb1 -1) #b010)
              (= (<< hb3 -1) #b110))
         (define k (ior (&<< hb1 1 21)
                        (<<  hb2   17)
                        (&<< hb3 1 16)
                        (next-instruction)))
         (set! PC k)
         (when debug? (fprintf OUT "JMP ~a" k))
         ]
        [(= hb0 #b1100)  ;;;;;;;;;;;;;;;;;;;;;;; RJMP
         (define k (ior (<< hb1 8)
                        (<< hb2 4)
                        hb3))
         (set! PC (& (+ PC k) #xfff))
         (when debug? (fprintf OUT "RJMP ~a" (num->hex (- (& (+ PC k) #xfff) PC -1))))
         ]
        [(= hb0 #b1101)  ;;;;;;;;;;;;;;;;;;;;;;; RCALL
         (define k (2-complement->num 12
                                      (ior (<< hb1 8)
                                           (<< hb2 4)
                                           hb3)))
         (stack-push-word PC)
         (set! PC (+ PC k))         
         (when debug?           
           (fprintf OUT "RCALL ~a" (num->hex k))
           )
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
         (when debug? (fprintf OUT "CALL ~a" k))
         ]
        [(= opcode #b1001010100001000) ;;;;;;;;;; RET
         (set! PC (stack-pop-word))
         (when debug? (fprintf OUT "RET -> ~a" (num->hex PC)))
         ]
        [(and (= hb0 #b0010)  ;;;;;;;;;;;;;;;;;;;; EOR
              (= (<<& hb1 -2 #b11) #b01))
         (define Rd (ior (&<< hb1 1 4) hb2))
         (define Rr (ior (&<< hb1 #b0010 3) hb3))
         (define Rd-val (sram-get-byte Rd))
         (define Rr-val (sram-get-byte Rr))
         (define result (bitwise-xor Rd-val Rr-val))
         (sram-set-byte Rd result)
         (sr-clear-V)
         (if (! result 7) (sr-set-N) (sr-clear-N))
         (if (one? (bitwise-xor (sr-get-N) (sr-get-V)))
             (sr-set-S) (sr-clear-S))
         (if (zero? result) (sr-set-Z) (sr-clear-Z))
         (when debug? (fprintf OUT "EOR R~a,R~a ; R~a <- ~a xor ~a" Rd Rr Rd 
                               (num->hex Rd-val)
                               (num->hex Rr-val)))
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
         (when debug? (fprintf OUT "INC R~a ; R~a <- ~a" Rd Rd R))
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
         (when debug? (fprintf OUT "DEC R~a ; R~a <- ~a"  Rd Rd (num->hex Rd-val)))
         ]
        [(and (= hb0 #b0001)  ;;;;;;;;;;;;;;;;;;;; CPSE
              (= (<<& hb1 -2 #b11) #b00))
         (define Rd (ior (&<< hb1 #b0001 4) hb2))
         (define Rr (ior (&<< hb1 #b0010 3) hb3))
         (define Rd-val (sram-get-byte Rd))
         (define Rr-val (sram-get-byte Rr))
         (when (= Rd-val Rr-val)
           ;; TODO: check 32-bit instructions and skip 2 words ins-
           ;; tead of 1
           (set! PC (+ PC 1)))
         (when debug? (fprintf OUT "CPSE R~a,R~a ; ~a == ~a? (~a)" Rd Rr 
                               (num->hex Rd-val)
                               (num->hex Rr-val) (= Rd-val Rr-val)))
         ]
        [(and (= hb0 #b1011)  ;;;;;;;;;;;;;;;;;;;; OUT
              (! hb1 3))
         (define A (ior (&<< hb1 #b0110 3)
                        hb3))
         (define Rr (ior (&<< hb1 #b0001 4) hb2))
         (define Rr-val (sram-get-byte Rr))
         (io-set A Rr-val)
         (when debug? (fprintf OUT "OUT A,Rr ; (~a)<-R~a[~a]" (num->hex A) Rr (num->hex Rr-val)))
         ]
        [(and (= hb0 #b1011)  ;;;;;;;;;;;;;;;;;;;; IN
              (n! hb1 3))
         (define A (ior (&<< hb1 #b0110 3)
                        hb3))
         (define A-val (io-get A))
         (define Rd (ior (&<< hb1 #b0001 4) hb2))
         (sram-set-byte Rd A-val)
         (when debug? (fprintf OUT "IN R~a,A ; R~a<-~a" Rd Rd (num->hex A-val)))
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;;;;;;;;; PUSH
              (= (& hb1 #b1110) #b0010)
              (= hb3 #b1111))
         (define Rd (ior (&<< hb1 #b0001 4) hb2))
         (define Rd-val (sram-get-byte Rd))
         (stack-push Rd-val)
         (when debug? (fprintf OUT "PUSH R~a[~a]" Rd Rd-val))
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;;;;;;;;; POP
              (= (& hb1 #b1110) #b0000)
              (= hb3 #b1111))
         (define Rd (ior (&<< hb1 #b0001 4) hb2))
         (define Rd-old-val (sram-get-byte Rd))
         (define Rd-new-val (stack-pop))
         (sram-set-byte Rd Rd-new-val)
         (when debug? (fprintf OUT "POP R~a[~a] ; <-~a" 
                              Rd Rd-old-val Rd-new-val))
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
         (when debug? (fprintf OUT "CPI R~a,~a ; ~a-~a=~a" Rd K Rd-val K result))
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
         (when debug? (fprintf OUT "CPC R~a,R~a ; ~a-~a-~a=~a" Rd Rr Rd-val Rr-val C R))
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
         (when debug? (fprintf OUT "SUBI R~a,A ; ~a-~a=~a" Rd Rd-val K R))
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;;;;;;;;; CBI
              (= hb1 #b1000))
         (define b (& hb3 #b111))
         (define A (ior (<< hb2 1) 
                        (<< hb3 -3)))
         (io-clear-bit A b)
         (when debug? (fprintf OUT "CBI A[~a],b[~a] ; " A b))
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;;;;;;;;; SBI
              (= hb1 #b1010))
         (define b (& hb3 #b0111))
         (define A (ior (<< hb2 1)
                        (<< hb3 -3)))
         (io-set-bit A b)
         (when debug? (fprintf OUT "SBI A[~a],b[~a]"
                              (num->hex A) b))
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;;;;;;;;; SBIC
              (= hb1 #b1001))
         (define b (& hb3 #b111))
         (define A (ior (<< hb2 1)
                        (<< hb3 -3)))
         (define A-bit (io-get-bit A b))
         (when (zero? A-bit)
           (if (is-two-word-instruction? PC)
               (begin (inc-pc) (inc-pc))
               (inc-pc)))
         (when debug? (fprintf OUT "SBIC A[~a],b[~a] ; ~a " 
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
         (when debug? (fprintf OUT "SBCI R~a,A ; ~a-~a=~a" 
                              Rd Rd-val K R))
         ]
        [(and (= hb0 #b1111)  ;;;;;;;;;;;;;;;;;;;; SBRC
              (= (& hb1 #b1110) #b1100)
              (= (& hb3 #b1000) #b0000))
         (define Rr (ior (&<< hb1 #b0001 4) hb2))
         (define b (& hb3 #b111))
         (define Rr-bit (sram-get-bit Rr b))
         (when (zero? Rr-bit)
           (if (is-two-word-instruction? PC)
               (begin (inc-pc) (inc-pc))
               (inc-pc)))
         (when debug? (fprintf OUT "SBRC R~a,b[~a] " Rr b))
         ]
        [(and (= hb0 #b1111)  ;;;;;;;;;;;;;;;;;;;; BRNE
              (= (<< hb1 -2 ) #b01)
              (= (& hb3 #b111) #b001))
         (define k (2-complement->num 7
                                      (ior (&<< hb1 #b11 5)
                                           (<< hb2 1)
                                           (&<< hb3 #b1000 -3))))
         (when (zero? (sr-get-Z))
           (set! PC (+ PC k)))
         (when debug? (fprintf OUT "BRNE ~a ; ~a -> ~a" k (zero? (sr-get-Z)) (num->hex PC)))
         ]
        [(and (= hb0 #b1111)  ;;;;;;;;;;;;;;;;;;;; BREQ
              (= (<< hb1 -2 ) #b00)
              (= (& hb3 #b111) #b001))
         (define k (2-complement->num 7
                                      (ior (&<< hb1 #b11 5)
                                           (<< hb2 1)
                                           (&<< hb3 #b1000 -3))))
         (when (one? (sr-get-Z))
           (set! PC (+ PC k)))
         (when debug? (fprintf OUT "BREQ ~a ; ~a -> ~a" k (zero? (sr-get-Z)) (num->hex PC)))
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
           (set! PC (+ PC k C)))
         (when debug? (fprintf OUT "BRCS ~a ; ~a -> ~a"  k (one? C) (num->hex PC)))
         ]
        [(and (= hb0 #b1001)  ;;;;;;;;;;;;;;;;;;;; ADDIW
              (= hb1 #b0110))
         (define K (ior (&<< hb2 #b1100 2) hb3))
         (define Rd (+ 24 (&<< hb2 #b0011 1))) (define Rd+ (+ Rd 1))
         (define R (ior (<< (sram-get-byte Rd+) 8)
                        (sram-get-byte Rd)))
         (define result (& (+ R K) #xffff))
         (sram-set-byte Rd+ (<< result -8))
         (sram-set-byte Rd (& result #xff))
         (if (zero? result) (sr-set-Z) (sr-clear-Z))
         (if (one? (& (n-bit-ref result 15)
                      (bit-ref Rd+ 7)))
             (sr-set-C) (sr-clear-C))
         (if (! result 15) (sr-set-N) (sr-clear-N))
         (if (one? (& (bit-ref result 15)
                      (n-bit-ref Rd+ 7)))
             (sr-set-C) (sr-clear-C))
         (if (one? (bitwise-xor (sr-get-N) (sr-get-V)))
             (sr-set-S) (sr-clear-S))
         (when debug? (fprintf OUT "ADDIW R~a:R~a,K[~a] ; ~a+~a=~a" Rd Rd+ K R K result))
         ]
        [(= opcode #x94f8)  ;;;;;;;;;;;;;;;;;;;; CLI
         (sr-clear-I)
         (when debug? (fprintf OUT "CLI"))
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
         (when debug? (fprintf OUT "AND R~a[~a],R~a[~a]" Rd Rd-val Rr Rr-val))
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
         (when debug? (fprintf OUT "ROR R~a[~a] ; <- ~a" Rd Rd-val R))
         ]
        [(= opcode #b1001010010001000)  ;;;;;;;;;;;;;;;;;;;; CLC
         (sr-clear-C)
         (when debug? (fprintf OUT "CLC"))]
        [(= opcode #b1001010000001000)  ;;;;;;;;;;;;;;;;;;;; SEC
         (sr-set-C)
         (when debug? (fprintf OUT "SEC"))]
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
            (when debug? (fprintf OUT "SBC R~a,R~a ; R~a <- ~a - ~a - ~a = ~a"
                                 Rd Rr Rd Rd-val Rr-val C R))]
        [(zero? opcode)  ;;;;;;;;;;;;;;;;;;;; NOP
         (when debug? (fprintf OUT "NOP"))]       
        
        [else
         (dec-pc)
          (fprintf OUT "UNKNOWN ~a ~a ~a ~a" 
                  (num->bin hb0)(num->bin hb1)(num->bin hb2)(num->bin hb3))
         ])
  (when debug?
    (define symbol (lookup-address PC))
    (when symbol (fprintf OUT " ;; ----> ~a" symbol))
    (fprintf OUT "~n"))
  )

(define (go-address address)
  (let loop ()
    (if (= PC address)
        'DONE
        (begin (fetch-and-decode)
               (loop)))))

(define (print-registers)
  (for ([r 32])
    (printf "R~a = ~a~n" r (num->hex (sram-get-byte r)))))


(define iteration 0)
(define debug? #f)
(define debug? #t)
(reset-machine "~/Documents/Programming/dump.txt")
(reset-machine)
(hex->flash! "../AES_for_both_Cards/main.hex")


;; Get the right symbol table
(define symbol-file "../AES_for_both_Cards/disassembly.txt")
;;(define symbol-file "~/Dropbox/Papers/Fault-Injection-Watermarks/src/maskedAES-PreGeneratedMasks/main-dump.txt")
(load-symbol-table symbol-file)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; script to run AES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(reset-machine)
(reset-machine "~/Documents/Programming/dump.txt")
(hex->flash! "../AES_for_both_Cards/main.hex")
;;(hex->flash! "~/Dropbox/Papers/Fault-Injection-Watermarks/src/maskedAES/main.hex")
;;(hex->flash! "~/Dropbox/Papers/Fault-Injection-Watermarks/src/maskedAES-PreGeneratedMasks/main.hex")
(define iteration 0)
(define debug? #f)
(go-address (lookup-symbol "main"))
;;(set! PC (lookup-symbol "aes_encrypt"))
(set! PC (lookup-symbol "aes_encrypt"))
(define debug? #t)
;; set key address
(define kl #xa5) (define kh #x00)
(define pl #xb5) (define ph #x00)
(sram-set-byte 22 kl) ;; key low
(sram-set-byte 23 kh) ;; key high
(sram-set-byte 24 pl) ;; pt low
(sram-set-byte 25 ph) ;; pt high
(for ([pt #(#x32 #x43 #xf6 #xa8 #x88 #x5a #x30 #x8d #x31 #x31 #x98 #xa2 #xe0 #x37 #x07 #x34)]
      [key  #(#x2b #x7e #x15 #x16 #x28 #xae #xd2 #xa6 #xab #xf7 #x15 #x88 #x09 #xcf #x4f #x3c)]
      [i 16])
  (sram-set-byte (+ kl i) key)
  (sram-set-byte (+ pl i) pt))
(go-address (lookup-symbol "__stop_program"))
(close-output-port OUT)

(fetch-and-decode)

(go-address (lookup-symbol "transmit_ATR"))
(go-address (lookup-symbol "receive_APDU"))
(go-address #x5f)
(go-address #x61)
(go-address #xcf3)


(go-address (lookup-symbol "delayparity")))
(go-address #xcf7)
(go-address #xd05)
(fetch-and-decode)

(set! PC (lookup-symbol "aes_initkey"))
(set! PC (lookup-symbol "do_aes_encrypt"))


(print-symbols)

(set! PC #x725)

(print-sram)
(print-registers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Opcodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 
;; keep track of current clock cycle current clock cycle
