;;(module data racket
;;  (provide print-instruction-uniquely)

(struct instr (id name ccs) #:transparent)

;; lists signify groups that should get the same ID
;; simple name, name and used registers, clock cycles
(define *instruction-groups*
  (list
   (list
    (list 'LPM      "LPM" 3)
    (list 'LPMRdZ   "LPM Rd,Z" 3)
    (list 'LPMRdZ+  "LPM Z+" 3))
   (list
    (list 'LDI   "LDI" 1))
   (list
    (list 'ORI   "ORI" 1)
    (list 'ORI   "ORI Rd,K" 1))
   (list
    (list 'LD        "LD" 2)
    (list 'LDRdX     "LD Rd,X" 2)
    (list 'LDRdX+    "LD Rd,X+" 2)
    (list 'LDRdX-    "LD Rd,X-" 2)
    (list 'LDRdX+q   "LD Rd,X+q" 2)
    (list 'LDRdY     "LD Rd,Y" 2)
    (list 'LDRdY+    "LD Rd,Y+" 2)
    (list 'LDRdY-    "LD Rd,Y-" 2)
    (list 'LDRdY+q   "LD Rd,Y+q" 2)
    (list 'LDRdZ     "LD Rd,Z" 2)
    (list 'LDRdZ+    "LD Rd,Z+" 2)
    (list 'LDRdZ-    "LD Rd,Z-" 2)
    (list 'LDRdZ+q   "LD Rd,Z+q" 2))
   (list
    (list 'MOV   "MOV" 1)
    (list 'MOV   "MOV Rd,Rr" 1))
   (list
    (list 'MOVW   "MOVW" 1)
    (list 'MOVW   "MOVW Rd,Rr" 1))
   (list
    (list 'MULS   "MULS" 2)
    (list 'MULS   "MULS Rd,Rr" 2))
   (list
    (list 'ST        "ST" 2)
    (list 'STXRr     "ST X,Rr" 2)
    (list 'STX+Rr    "ST X+,Rr" 2)
    (list 'STX-Rr    "ST X-,Rr" 2)
    (list 'STX+qRr   "ST X+q,Rr" 2)
    (list 'STYRr     "ST Y,Rr" 2)
    (list 'STY+Rr    "ST Y+,Rr" 2)
    (list 'STY-Rr    "ST Y-,Rr" 2)
    (list 'STY+qRr   "ST Y+q,Rr" 2)
    (list 'STZRr     "ST Z,Rr" 2)
    (list 'STZ+Rr    "ST Z+,Rr" 2)
    (list 'STZ-Rr    "ST Z-,Rr" 2)
    (list 'STZ+qRr   "ST Z+q,Rr" 2))
   (list
    (list 'STS   "STS" 2)
    (list 'STS   "STS k,Rr" 2))
   (list
    (list 'LDS   "LDS" 2)
    (list 'LDS   "LDS Rr,k" 2))
   (list
    (list 'JMP   "JMP" 3))
   (list
    (list 'RJMP   "RJMP" 2))
   (list
    (list 'RCALL   "RCALL" 3))
   (list
    (list 'CALL   "CALL" 4))
   (list
    (list 'RET   "RET" 4))
   (list
    (list 'EOR   "EOR" 1)
    (list 'EOR   "EOR Rd,Rr" 1))
   (list
    (list 'INC   "INC" 1)
    (list 'INC   "INC Rd" 1))
   (list
    (list 'DEC   "DEC" 1)
    (list 'DEC   "DEC Rd" 1))
   (list
    (list 'CPSE   "CPSE" (list 1 2 3))
    (list 'CPSE   "CPSE Rd,Rr" (list 1 2 3)))
   (list
    (list 'OUT   "OUT" 1)
    (list 'OUT   "OUT A,Rr" 1))
   (list
    (list 'IN   "IN" 1)
    (list 'IN   "IN Rd,A" 1))
   (list
    (list 'PUSH   "PUSH" 2)
    (list 'PUSH   "PUSH Rd" 2))
   (list
    (list 'POP   "POP" 2)
    (list 'POP   "POP Rd" 2))
   (list
    (list 'CPI   "CPI" 1)
    (list 'CPI   "CPI Rd,K" 1))
   (list
    (list 'CP   "CP" 1)
    (list 'CP   "CP Rd,Rr" 1))
   (list
    (list 'CPC   "CPC" 1)
    (list 'CPC   "CPC Rd,Rr" 1))
   (list
    (list 'SUB   "SUB" 1)
    (list 'SUB   "SUB Rd,Rr" 1))
   (list
    (list 'SUBI   "SUBI" 1)
    (list 'SUBI   "SUBI Rd,K" 1))
   (list
    (list 'BLD   "BLD" 1)
    (list 'BLD   "BLD Rd,b" 1))
   (list
    (list 'ADIW   "ADIW" 2)
    (list 'ADIW   "ADIW Rd,K" 2))
   (list
    (list 'SBIW   "SBIW" 2)
    (list 'SBIW   "SBIW Rd,K" 2))
   (list
    (list 'CBI   "CBI" 2)
    (list 'CBI   "CBI A,b" 2))
   (list
    (list 'SBI   "SBI" 2)
    (list 'SBI   "SBI A,b" 2))
   (list
    (list 'SBIC   "SBIC" (list 1 2 3))
    (list 'SBIC   "SBIC A,b" (list 1 2 3)))
   (list
    (list 'SBIS   "SBIS" (list 1 2 3))
    (list 'SBIS   "SBIS A,b" (list 1 2 3)))
   (list
    (list 'SBCI   "SBCI" 1)
    (list 'SBCI   "SBCI Rd,K" 1))
   (list
    (list 'SBRC   "SBRC" (list 1 2 3))
    (list 'SBRC   "SBRC Rr,b" (list 1 2 3)))
   (list
    (list 'SBRS   "SBRS" (list 1 2 3))
    (list 'SBRS   "SBRS Rr,b" (list 1 2 3)))
   (list
    (list 'BRBC   "BRBC" (list 1 2))
    (list 'BRNE   "BRNE" (list 1 2)))
   (list
    (list 'BREQ   "BREQ" (list 1 2)))
   (list
    (list 'BRBS   "BRBS" (list 1 2))
    (list 'BRTC   "BRTC" (list 1 2)))
   (list
    (list 'BRTS   "BRTS" (list 1 2)))
   (list
    (list 'BRCC   "BRCC" (list 1 2)))
   (list
    (list 'ADD   "ADD" 1)
    (list 'ADD   "ADD Rd,Rr" 1))
   (list
    (list 'ADC   "ADC" 1)
    (list 'ADC   "ADC Rd,Rr" 1))
   (list
    (list 'AND   "AND" 1)
    (list 'AND   "AND Rd,Rr" 1))
   (list
    (list 'ROR   "ROR" 1)
    (list 'ROR   "ROR Rd" 1))
   (list
    (list 'LSR   "LSR" 1)
    (list 'LSR   "LSR Rd" 1))
   (list
    (list 'BST   "BST" 1)
    (list 'BST   "BST Rd" 1))
   (list
    (list 'CLI   "CLI" 1))
   (list
    (list 'CLC   "CLC" 1))
   (list
    (list 'BCLR   "BCLR" 1))
   (list
    (list 'CLT   "CLT" 1))
   (list
    (list 'SEC   "SEC" 1))
   (list
    (list 'BSET   "BSET" 1))
   (list
    (list 'SET   "SET" 1))
   (list
    (list 'SBC   "SBC" 1)
    (list 'SBC   "SBC Rd,Rr" 1))
   (list         
    (list 'NOP   "NOP" 1))
   (list
    (list 'SWAP   "SWAP" 1)
    (list 'SWAP   "SWAP Rd" 1))
   (list
    (list 'MUL   "MUL" 2)
    (list 'MUL   "MUL Rd,Rr" 2))
   (list
    (list 'OR   "OR" 1)
    (list 'OR   "OR Rd,Rr" 1))
   (list         
    (list 'ANDI   "ANDI" 1)
    (list 'ANDI   "ANDI Rd,K" 1))
   (list
    (list 'NEG   "NEG" 1)
    (list 'NEG   "NEG Rd" 1))
   (list
    (list 'IJMP   "IJMP" 2))))

;; give each instruction group a number
(define (assign-group-id instruction-groups)
  (define result empty)
  (for ([group instruction-groups]
        [group-id (length instruction-groups)])
    (for ([instruction-data group])
      (set! result
            (cons
             (cons (car instruction-data)
                   (apply instr (cons group-id
                                      (cdr instruction-data))))
             result))))
  (reverse result))

(define instructions (assign-group-id *instruction-groups*))

(define instruction-table (make-hash instructions))
;; returns id and plot name of the instruction
(define (get-instr unique-name)
  (hash-ref instruction-table unique-name))

;; save names
;; go over the list once and take the first names 
;; as the one to be shown in individual instruction statistics
(define highestID (instr-id (argmax instr-id
                                    (map cdr instructions))))
(define uniqueIDs (make-vector (+ highestID 1) #f))
(for ([instr (map cdr instructions)])
  (unless (vector-ref uniqueIDs (instr-id instr))
    (vector-set! uniqueIDs (instr-id instr) (instr-name instr))))

(define (get-unique-name id)
  (vector-ref uniqueIDs id))

(define (print-instruction-uniquely port unique-instr-name
                                    (ccs-when-dynamic #f))
  (define instr (get-instr unique-instr-name))
  (define unique-name (get-unique-name (instr-id instr)))
  (define ccs (instr-ccs instr))
  (when ccs-when-dynamic (set! ccs ccs-when-dynamic))
  (fprintf port "~a|~a|~a|~a|" 
           unique-name ccs
           (instr-name instr) (instr-id instr)))
;;  )
