;;(module data racket
;;  (provide print-instruction-uniquely)

;; name| id| plot name
(define instructions
  '((LPM     0 "LPM" 3)
    (LPMRdZ  0 "LPM Rd,Z" 3)
    (LPMRdZ+ 0 "LPM Z+" 3)

    (LDI 1 "LDI" 1)

    (LD      2 "LD" 2)
    (LDRdX   2 "LD Rd,X" 2)
    (LDRdX+  2 "LD Rd,X+" 2)
    (LDRdX+q 2 "LD Rd,X+q" 2)
    (LDRdY   2 "LD Rd,Y" 2)
    (LDRdY+  2 "LD Rd,Y+" 2)
    (LDRdY+q 2 "LD Rd,Y+q" 2)
    (LDRdZ   2 "LD Rd,Z" 2)
    (LDRdZ+  2 "LD Rd,Z+" 2)
    (LDRdZ+q 2 "LD Rd,Z+q" 2)
    
    (MOV 3 "MOV" 1)
    (MOV 3 "MOV Rd,Rr" 1)

    (MOVW 4 "MOVW" 1)
    (MOVW 4 "MOVW Rd,Rr" 1)

    (ST      5 "ST" 2)
    (STXRr   5 "ST X,Rr" 2)
    (STX+Rr  5 "ST X+,Rr" 2)
    (STX+qRr 5 "ST X+q,Rr" 2)
    (STYRr   5 "ST Y,Rr" 2)
    (STY+Rr  5 "ST Y+,Rr" 2)
    (STY+qRr 5 "ST Y+q,Rr" 2)
    (STZRr   5 "ST Z,Rr" 2)
    (STZ+Rr  5 "ST Z+,Rr" 2)
    (STZ+qRr 5 "ST Z+q,Rr" 2)
    
    (STS 6 "STS" 2)
    (STS 6 "STS k,Rr" 2)

    (LDS 7 "LDS" 2)
    (LDS 7 "LDS Rr,k" 2)

    (JMP 8 "JMP" 3)

    (RJMP 9 "RJMP" 2)
    
    (RCALL 10 "RCALL" 3)

    (CALL 11 "CALL" 4)

    (RET 12 "RET" 4)

    (EOR 13 "EOR" 1)
    (EOR 13 "EOR Rd,Rr" 1)

    (INC 14 "INC" 1)
    (INC 14 "INC Rd" 1)

    (DEC 15 "DEC" 1)
    (DEC 15 "DEC Rd" 1)

    (CPSE 16 "CPSE" 1 2 3)
    (CPSE 16 "CPSE Rd,Rr" 1 2 3)

    (OUT 17 "OUT" 1)
    (OUT 17 "OUT A,Rr" 1)

    (IN 18 "IN" 1)
    (IN 18 "IN Rd,A" 1)
    
    (PUSH 19 "PUSH" 2)
    (PUSH 19 "PUSH Rd" 2)

    (POP 20 "POP" 2)
    (POP 20 "POP Rd" 2)

    (CPI 21 "CPI" 1)
    (CPI 21 "CPI Rd,K" 1)
    
    (CP 22 "CP" 1)
    (CP 22 "CP Rd,Rr" 1)

    (CPC 23 "CPC" 1)
    (CPC 23 "CPC Rd,Rr" 1)

    (SUB 240 "SUB" 1)
    (SUB 240 "SUB Rd,Rr" 1)

    (SUBI 24 "SUBI" 1)
    (SUBI 24 "SUBI Rd,K" 1)

    (ADIW 25 "ADIW" 2)
    (ADIW 25 "ADIW Rd,K" 2)

    (SBIW 26 "SBIW" 2)
    (SBIW 26 "SBIW Rd,K" 2)

    (CBI 27 "CBI" 2)
    (CBI 27 "CBI A,b" 2)
    
    (SBI 28 "SBI" 2)
    (SBI 28 "SBI A,b" 2)

    (SBIC 29 "SBIC" 1 2)
    (SBIC 29 "SBIC A,b" 1 2)

    (SBCI 30 "SBCI" 1)
    (SBCI 30 "SBCI Rd,K" 1)

    (SBRC 31 "SBRC" 1 2)
    (SBRC 31 "SBRC Rr,b" 1 2)

    (BRBC 32 "BRBC" 1 2)
    (BRNE 3200 "BRNE" 1 2)

    (BREQ 33 "BREQ" 1 2)

    (BRBS 34 "BRBS" 1 2)
    
    (BRTC 35 "BRTC" 1 2)

    (BRTS 36 "BRTS" 1 2)

    (BRCC 37 "BRCC" 1 2)

    (ADD 38 "ADD" 1)
    (ADD 38 "ADD Rd,Rr" 1)

    (ADC 39 "ADC" 1)
    (ADC 39 "ADC Rd,Rr" 1)

    (AND 40 "AND" 1)
    (AND 40 "AND Rd,Rr" 1)

    (ROR 41 "ROR" 1)
    (ROR 41 "ROR Rd" 1)

    (LSR 42 "LSR" 1)
    (LSR 42 "LSR Rd" 1)

    (BST 43 "BST" 1)
    (BST 43 "BST Rd" 1)

    (CLI 44 "CLI" 1)

    (CLC 45 "CLC" 1)

    (BCLR 46 "BCLR" 1)
    (CLT 4600 "CLT" 1)

    (SEC 47 "SEC" 1)

    (BSET 48 "BSET" 1)
    (SET 4800 "SET" 1)

    (SBC 49 "SBC" 1)
    
    (NOP 50 "NOP" 1)

    (SWAP 51 "SWAP" 1)
    (SWAP 51 "SWAP Rd" 1)

    (MUL 52 "MUL" 2)
    (MUL 52 "MUL Rd,Rr" 2)

    (ANDI 53 "ANDI" 1)
    (ANDI 53 "ANDI Rd,K" 1)

    (NEG 54 "NEG" 1)
    (NEG 54 "NEG Rd" 1)

    ))

(define instruction-table (make-hash instructions))
;; returns id and plot name of the instruction
(define (instr-get-id unique-name)
  (hash-ref instruction-table unique-name))

;; save names
;; go over the list once and take the first names 
;; as the one to be shown in individual instruction statistics
(define highestID (cadr (argmax cadr instructions)))
(define uniqueIDs (make-vector (+ highestID 1) #f))
(for ([instr instructions])
  (unless (vector-ref uniqueIDs (cadr instr))
    (vector-set! uniqueIDs (cadr instr) (caddr instr))))

(define (get-unique-name id)
  (vector-ref uniqueIDs id))

(define (print-instruction-uniquely port unique-instr-name 
                                    (duration-dynamic? #f))
  (define id (instr-get-id unique-instr-name))
  (define unique-name (get-unique-name (car id)))
  (define clock-cycles (caddr id))
  (when duration-dynamic?
    (set! clock-cycles duration-dynamic?))
  (fprintf port "~a|~a|~a|~a|" 
           unique-name clock-cycles 
           (car id) (cadr id)))
;;  )
