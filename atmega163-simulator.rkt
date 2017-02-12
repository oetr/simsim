;;(require racket racket/main rackunit)
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
;; make signed
(define (2-complement->num width num)
  (if (! num (- width 1))
      (+ (- (<< 1 (- width 1)))
         (bitwise-bit-field num 0 (- width 1)))
      num))
;; make unsigned
(define (num->2-complement width num)
  (& num (- (expt 2 width) 1)))

(define (num->bytes num (n-bits 32))
  (list->bytes
   (for/list ([i (range (- (quotient n-bits 8) 1) -1 -1)])
     (<<& num (* i -8) #xff))))

(define (hamming-weight n)
  (define result 0)
  (for ([bit (integer-length n)])
    (when (bitwise-bit-set? n bit)
      (set! result (+ result 1))))
  result)

(define (hamming-distance n0 n1)
  (define xored-number (bitwise-xor n0 n1))
  (hamming-weight xored-number))

(define (hex->flash! a-file)
  (define hex (file->lines (expand-user-path a-file)))
  (define bytes-so-far 0)
  ;; write data
  (for ([line hex])
    (define len (/ (- (string-length line) 3) 2))
    (define start-code (substring line 0 1))
    (define byte-count (hex->num (substring line 1 3)))
    (define address (hex->num (substring line 3 7)))
    (define record-type (hex->num (substring line 7 9)))
    (define end-data-addr (+ 9 (* byte-count 2)))
    (define data (substring line 9 end-data-addr))   
    (define checksum
      (hex->num
       (substring line end-data-addr (+ end-data-addr 2))))
    ;; See whether the binary is bigger than our flash
    (set! bytes-so-far (+ bytes-so-far byte-count))
    (when (>= (/ bytes-so-far 2) FLASHEND)
      (error 'hex->flash! "Binary is bigger than available flash."))
    ;; compute the checksum
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
      (error 'hex->flash! "Checksum wrong: expected ~a, got ~a"
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
      (regexp-match
       #px"([0-9a-f]+).+?([gl]).+?(\\.text|\\.bss|\\.data)\t[0-9a-f]+ (.+)$" l))
    (when (list? a-match)
      (define gl     (list-ref a-match 2))
      (define type   (list-ref a-match 3))
      (define symbol (list-ref a-match 4))
      (define addr (hex->num (list-ref a-match 1)))
      (when (string=? type ".text")
        (set! addr (/ addr 2)))
      ;; addresses of static variables in the .bss section
      ;; are shifted, shift them back here
      (when (or (string=? type ".bss") (string=? type ".data"))
        (set! addr (- addr #x800000)))
      (set! addrs (cons (cons addr   symbol) addrs))
      (set! syms  (cons (cons symbol addr)   syms))))
  (set! ADDRESS-TABLE (make-hash addrs))
  (set! SYMBOL-TABLE (make-hash syms)))

(define (lookup-address addr)
  (hash-ref ADDRESS-TABLE addr #f))

(define (lookup-symbol a-symbol (approximate-matching? #f)
                       (closest-to #f))
  (define syms (list a-symbol))
  ;; go through all extracted symbols and match approximate name
  (define count 0)
  (when approximate-matching?
    (set! syms (list))
    (for ([(symbol addr) SYMBOL-TABLE])
      (define a-match (regexp-match (~a a-symbol "[0-9]*") symbol))
      (when a-match
        (set! syms (cons (car a-match) syms))
        (set! count (+ count 1)))))
  (define results
    (sort (filter identity
                  (map (lambda (x)
                         (hash-ref SYMBOL-TABLE x #f)) syms))
          <))
  (define r #f)
  (if (zero? count)
      ;;(car results)
      (set! r (car results))
      (if closest-to
          (let loop ([results results]
                     [prev #f])
            (if (empty? results)
                (set! r prev)
                (if (<= closest-to (car results))
                    (if (and prev
                             (>= (- closest-to prev)
                                 (- (car results) closest-to)))
                        (set! r prev)
                        (set! r (car results)))
                    (loop (cdr results) (car results)))))
          (set! r (car results))))
  r)

(define (print-symbols)
  (define mapping (sort (hash->list SYMBOL-TABLE)
                        (lambda (a b) (string<? (car a) (car b)))))
  (for([element mapping])
    (printf "~a: ~a~n" (car element) (num->hex (cdr element)))))
(define (print-addrs)
  (define mapping (sort (hash->list ADDRESS-TABLE)
                        (lambda (a b)
                          (< (car a) (car b)))))
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
;;(define FLASHEND #xffff)
(define FLASHEND #x1fff)
(define FLASH (make-vector FLASHEND))
(define (flash-length) FLASHEND)
;; get and set word use word addresses
(define (flash-get-word addr) (vector-ref FLASH addr))
(define (flash-set-word addr val)
  (when (>= addr FLASHEND)
    (error 'flash-set-word
           "Flash address 0x~a is larger than flash size 0x~a~n"
           (num->hexb addr) (num->hexb FLASHEND)))
  (vector-set! FLASH addr val))
;; get-byte use byte addresses
(define *flash-address* 0)
(define *flash-data* 0)
(define (flash-get-byte addr)
  (when save-hamming-distance?
    (save-intermediate-values (bitwise-xor addr *flash-address*))
    (set! *flash-address* addr))
  (define word (flash-get-word (arithmetic-shift addr -1)))
  (if (bitwise-bit-set? addr 0)
      (let ([a-byte (arithmetic-shift word -8)])
        (when save-hamming-distance?
          (save-intermediate-values (bitwise-xor a-byte
                                                 *flash-data*))
          (set! *flash-data* a-byte))
        a-byte)
      (let ([a-byte (bitwise-and word #x00ff)])
        (when save-hamming-distance?
          (save-intermediate-values (bitwise-xor a-byte
                                                 *flash-data*))
          (set! *flash-data* a-byte))
        a-byte)))

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
(define *sram-prev-data* 0)
(define *sram-prev-addr* 0)
;; get and set bytes
(define (sram-get-byte addr)
  (define address addr)
  (when (>= addr RAMEND)
    (printf "WARNING: address outside RAMEND ~a (~a) [PC: ~a]~n"
            (num->hex addr) addr PC)
    (set! address (modulo (+ addr IO-SIZE) RAMEND)))
  (define data (vector-ref SRAM address))
  ;;(save-intermediate-values address)
  (save-intermediate-values data)
  ;; Hamming distance
  (when save-hamming-distance?
    (save-intermediate-values (bitwise-xor
                               address *sram-prev-addr*))
    (set! *sram-prev-addr* address)
    (save-intermediate-values (bitwise-xor
                               data *sram-prev-data*))
    (set! *sram-prev-data* data))
  data)
(define (sram-set-byte addr data)
  (when (> data #xff)
    (printf "\"sram-set-byte\" WARNING: expected a byte, got something bigger: ~a <- ~a~n" addr data))
  (define address addr)
  (when (>= addr RAMEND)
    (printf "WARNING: address outside RAMEND ~a [PC: ~a]~n" addr PC)
    (set! address (modulo (+ addr IO-SIZE) RAMEND)))
  (vector-set! SRAM address data)
  ;;(save-intermediate-values address)
  (save-intermediate-values data)
  ;; Hamming distance
  (when save-hamming-distance?
    (save-intermediate-values (bitwise-xor
                               address *sram-prev-addr*))
    (set! *sram-prev-addr* address)
    (save-intermediate-values (bitwise-xor
                               data *sram-prev-data*))
    (set! *sram-prev-data* data)))

(define (get-register reg)
  (when (>= reg 32)
    (error "ERROR: register higher than 32 ~a~n"
           (num->hex reg)))
  (define data (vector-ref SRAM reg))
  ;;(save-intermediate-values reg)
  (save-intermediate-values data)
  data)
(define *bus-prev-byte* 0)
(define (set-register reg val)
  (when (>= reg 32)
    (error 'set-register
           "ERROR: register higher than 32 ~a~n"
           (num->hex reg)))
  (when save-hamming-distance?
    (define reg-prev-val (vector-ref SRAM reg))
    (save-intermediate-values (bitwise-xor reg-prev-val val)))
  (vector-set! SRAM reg val)
  ;;(save-intermediate-values reg)
  (save-intermediate-values val)
  )

;; set two consecutive registers to a word
(define (set-register-w reg val)
  (set-register reg (& val #xff)) ;; low value
  (set-register (+ reg 1) (<< val -8)) ;; high value
  )
;; concatenate two registers
(define (get-registers . regs)
  (define result 0)
  (for ([reg (reverse regs)]
        [i (length regs)])
    (printf "~a~n" i)
    (define reg-val (get-register reg))
    (set! result (bitwise-ior result (<< reg-val (* i 8)))))
  result)

(define *sram-prev-byte* 0)
(define *sram-prev-bit-addr* 0)

(define (sram-set-bit addr i)
  (define data (vector-ref SRAM addr))
  (define result (ior data (<< 1 i)))
  (vector-set! SRAM addr result)
  (when save-hamming-distance?
    (save-intermediate-values (n-bit-ref *sram-prev-byte* i))
    (set! *sram-prev-byte* result))
  ;; (save-intermediate-values addr)
  (save-intermediate-values 1))

(define (sram-clear-bit addr i)
  (define data (vector-ref SRAM addr))
  (define result (& (vector-ref SRAM addr)
                    (bitwise-not (<< 1 i))))
  (vector-set! SRAM addr result)
  (when save-hamming-distance?
    (save-intermediate-values (bit-ref *sram-prev-byte* i))
    (set! *sram-prev-byte* result))
  ;; (save-intermediate-values addr)
  (save-intermediate-values 0))

(define (sram-get-bit addr i)
  (& (<< (vector-ref SRAM addr) (- i)) 1))

;; map register file
(define (get-x) (ior (<< (get-register 27) 8)
                     (get-register 26)))
(define (get-y) (ior (<< (get-register 29) 8)
                     (get-register 28)))
(define (get-z) (ior (<< (get-register 31) 8)
                     (get-register 30)))
(define (inc-x)
  (define x (+ (get-x)
               1))
  (set-register 27 (<<& x -8 #xff))
  (set-register 26 (& x #xff)))
(define (dec-x)
  (define x (- (get-x) 1))
  (set-register 27 (<<& x -8 #xff))
  (set-register 26 (& x #xff)))
(define (inc-y)
  (define y (+ (get-y) 1))
  (set-register 29 (<<& y -8 #xff))
  (set-register 28 (& y #xff)))
(define (dec-y)
  (define y (- (get-y) 1))
  (set-register 29 (<<& y -8 #xff))
  (set-register 28 (& y #xff)))
(define (inc-z)
  (define z (+ (get-z) 1))
  (set-register 31 (<<& z -8 #xff))
  (set-register 30 (& z #xff)))
(define (dec-z)
  (define z (- (get-z) 1))
  (set-register 31 (<<& z -8 #xff))
  (set-register 30 (& z #xff)))
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
(define (set-pc! new-pc)
  (when save-hamming-distance?
    (save-intermediate-values (bitwise-xor PC new-pc)))
  (set! PC new-pc))
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
  (sram-set-byte sp word-low)
  (sram-set-byte (- sp 1) word-high)
  (inc-sp -2))
(define (stack-pop-word)
  (define sp (+ (get-sp) 1))
  (define word-high (sram-get-byte sp))
  (define word-low  (sram-get-byte (+ sp 1)))
  (inc-sp 2)
  (ior (<< word-high 8)
       word-low))

(define OUT (current-output-port))
(define CURRENT-CLOCK-CYCLE 0)
(define PREVIOUS-CLOCK-CYCLE #f)

(define (reset-machine (filename #f) #:keep-flash? (keep-flash? #f))
  (unless keep-flash?
    (for ([i FLASHEND]) (vector-set! FLASH i 0)))
  (set! SRAM (make-vector RAMEND #x00))
  (for ([i IO-SIZE]) (vector-set! SRAM i 0))
  (set! CURRENT-CLOCK-CYCLE 0)
  (set! PREVIOUS-CLOCK-CYCLE #f)
  (set! SAVED-PC 0)
  (set! INTERMEDIATE-VALUES-INDEX 0)
  (set! PC 0)
  (set! *flash-address* 0)
  (set! *flash-data* 0)
  (set! *sram-prev-data* 0)
  (set! *sram-prev-addr* 0)
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

(define (close-if-file a-port)
  (when (and (port? OUT)
             (not (port-closed? OUT))
             (not (eq? OUT (current-output-port))))
    (close-output-port OUT)))

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
(define (one? num) (= num 1))

;; make it easier to access the registers
(define (get-Rd opcode)
  (<<& opcode -4 #b11111))
(define (get-Rr opcode)
  (ior (<<& opcode -5 #b10000)
       (& opcode #xf)))

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

(define debug? #f)

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
