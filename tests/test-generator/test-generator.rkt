(module test-generator racket/main
  (require racket racket/main rackunit racket/system)
  (provide (except-out (all-defined-out)
                       *instructions-grammar*))

  ;; instruction arguments and their range
  (define *instructions-grammar*
    (list->vector
     `((MOV (reg 0 31) (reg 0 31))
       (LPM)
       (LPM (reg 0 31) (Z nil post+))
       (LDI (reg 16 31) (const 0 255))
       (ORI (reg 16 31) (const 0 255))
       (LD (reg 0 31) (X nil pre- post+))
       (LD (reg 0 31) (Y nil pre- post+))
       (LDD (reg 0 31) (displacement Y (const 0 63)))
       (LD (reg 0 31) (Z nil pre- post+))
       (LDD (reg 0 31) (displacement Z (const 0 63)))
       (MOVW (reg 0 31 2) (reg 0 31 2))
       (MULS (reg 16 31) (reg 16 31))
       (ST (X nil pre- post+) (reg 0 31))
       (ST (Y nil pre- post+) (reg 0 31))
       (STD (displacement Y (const 0 63)) (reg 0 31))
       (ST (Z nil pre- post+) (reg 0 31))
       (STD (displacement Z (const 0 63)) (reg 0 31))
       (STS (const 0 65535) (reg 0 31)) ;; 32 bit
       (STS (const 0 127) (reg 16 31)) ;; 16 bit
       (LDS (reg 0 31) (const 0 65535)) ;; 32 bit
       (LDS (reg 16 31) (const 0 127)) ;; 32 bit
       (EOR (reg 0 31) (reg 0 31))
       (INC (reg 0 31))
       (DEC (reg 0 31))
       (CPSE (reg 0 31) (reg 0 31))
       (OUT (const 0 63) (reg 0 31))
       (IN (reg 0 31) (const 0 63))
       (PUSH (reg 0 31))
       (POP (reg 0 31))
       (CPI (reg 16 31) (const 0 255))
       (CP (reg 0 31) (reg 0 31))
       (CPC (reg 0 31) (reg 0 31))
       (SUB (reg 0 31) (reg 0 31))
       (SUBI (reg 16 31) (const 0 255))
       (BLD (reg 0 31) (const 0 7))
       (ADIW (reg 24 31 2) (const 0 63))
       (SBIW (reg 24 31 2) (const 0 63))
       (CBI (const 0 31) (const 0 7))
       (SBI (const 0 31) (const 0 7))
       (SBIC (const 0 31) (const 0 7))
       (SBIS (const 0 31) (const 0 7))
       (SBCI (reg 16 31) (const 0 255))
       (SBRC (reg 0 31) (const 0 7))
       (SBRS (reg 0 31) (const 0 7))
       (ADD (reg 0 31) (reg 0 31))
       (ADC (reg 0 31) (reg 0 31))
       (AND (reg 0 31) (reg 0 31))
       (ROR (reg 0 31))
       (LSR (reg 0 31))
       (BST (reg 0 31) (const 0 7))
       (CLI)
       (CLC)
       (BCLR (const 0 7))
       (CLT)
       (SEC)
       (BSET (const 0 7))
       (SET)
       (SBC (reg 0 31) (reg 0 31))
       (NOP)
       (SWAP (reg 0 31))
       (MUL (reg 0 31) (reg 0 31))
       (OR (reg 0 31) (reg 0 31))
       (ANDI (reg 16 31) (const 0 255))
       (NEG (reg 0 31))
       )))

  ;; Handle jumps separately
  ;; (JMP (const 0 8388606 2))
  ;; (RJMP (const -2048 2047 2))
  ;; (RCALL (const ))
  ;; (CALL (const))
  ;; (RET)
  ;; (BRBC (const 0 7) (const -64 63))
  ;; (BREQ (const -64 63))
  ;; (BRBS (const 0 7) (const -64 63))
  ;; (BRTC (const -64 63))
  ;; (BRTS (const -64 63))
  ;; (BRCC (const -64 63))
  ;; (IJMP)


  (define (range->N from to (step 1))
    (+ (quotient (- to from) step) 1))

  (define (range-i->n i from (step 1))
    (+ from (* i step)))

  (define (random-range from to (step 1))
    (range-i->n (random (range->N from to step)) from step))

  (define (process-reg-const reg (name ""))
    (define rng (cdr reg))
    (define n (apply random-range rng))
    (~a name n))

  (define (is-*? expr symb)
    (and (list? expr) (eq? (car expr) symb)))

  (define (process-operator reg op)
    (match op
      ['nil (~a reg)]
      ['pre- (~a "-" reg)]
      ['pre+ (~a "+" reg)]
      ['post- (~a reg "-")]
      ['post+ (~a reg "+")]
      [_ (error 'process-operator "unknown operator: ~a." op)]))

  (define (randomize-indirect-address-register reg-op-expr)
    (define reg (car reg-op-expr))
    (define ops (cdr reg-op-expr))
    (define op (list-ref ops (random (length ops))))
    (process-operator reg op))


  (define (process-displacement displacement-expr)
    (define reg (cadr displacement-expr))
    (define n (car (interpret-args (cddr displacement-expr))))
    (~a reg "+" n))

  (define (interpret-args args)
    (cond [(empty? args) '()]
          [(is-*? (car args) 'reg)
           (cons (process-reg-const (car args) 'r)
                 (interpret-args (cdr args)))]
          [(is-*? (car args) 'const)
           (cons (process-reg-const (car args))
                 (interpret-args (cdr args)))]
          [(is-*? (car args) 'displacement)
           (cons (process-displacement (car args))
                 (interpret-args (cdr args)))]
          [(is-*? (car args) ':)
           
           (printf ": ~n")]
          [else ;; symbol
           (cons (randomize-indirect-address-register (car args))
                 (interpret-args (cdr args)))]))

  (define (concat instr a-list (sep ", "))
    (~a instr " " (string-join a-list sep)))

  (define (make-random-instr instr)
    (define name (car instr))
    (concat name (interpret-args (cdr instr))))

  (define (generate-random-code n)
    (define N-instr (vector-length *instructions-grammar*))
    (for/list ([i n])
      (define instruction-index (random N-instr))
      (make-random-instr (vector-ref *instructions-grammar*
                                     instruction-index))))
  )
