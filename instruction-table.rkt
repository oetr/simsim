;;(module data racket
;;  (provide print-instruction-uniquely)

(struct instr (id name ccs))

;; name| id| plot name
(define instructions
  (list
   (cons 'LPM      (instr 0 "LPM" 3))
   (cons 'LPMRdZ   (instr 0 "LPM Rd,Z" 3))
   (cons 'LPMRdZ+  (instr 0 "LPM Z+" 3))

   (cons 'LDI  (instr 1 "LDI" 1))

   (cons 'LD       (instr 2 "LD" 2))
   (cons 'LDRdX    (instr 2 "LD Rd,X" 2))
   (cons 'LDRdX+   (instr 2 "LD Rd,X+" 2))
   (cons 'LDRdX+q  (instr 2 "LD Rd,X+q" 2))
   (cons 'LDRdY    (instr 2 "LD Rd,Y" 2))
   (cons 'LDRdY+   (instr 2 "LD Rd,Y+" 2))
   (cons 'LDRdY-   (instr 2 "LD Rd,Y-" 2))
   (cons 'LDRdY+q  (instr 2 "LD Rd,Y+q" 2))
   (cons 'LDRdZ    (instr 2 "LD Rd,Z" 2))
   (cons 'LDRdZ+   (instr 2 "LD Rd,Z+" 2))
   (cons 'LDRdZ+q  (instr 2 "LD Rd,Z+q" 2))
   
   (cons 'MOV  (instr 3 "MOV" 1))
   (cons 'MOV  (instr 3 "MOV Rd,Rr" 1))

   (cons 'MOVW  (instr 4 "MOVW" 1))
   (cons 'MOVW  (instr 4 "MOVW Rd,Rr" 1))

   (cons 'ST       (instr 5 "ST" 2))
   (cons 'STXRr    (instr 5 "ST X,Rr" 2))
   (cons 'STX+Rr   (instr 5 "ST X+,Rr" 2))
   (cons 'STX+qRr  (instr 5 "ST X+q,Rr" 2))
   (cons 'STYRr    (instr 5 "ST Y,Rr" 2))
   (cons 'STY+Rr   (instr 5 "ST Y+,Rr" 2))
   (cons 'STY+qRr  (instr 5 "ST Y+q,Rr" 2))
   (cons 'STZRr    (instr 5 "ST Z,Rr" 2))
   (cons 'STZ+Rr   (instr 5 "ST Z+,Rr" 2))
   (cons 'STZ+qRr  (instr 5 "ST Z+q,Rr" 2))
   
   (cons 'STS  (instr 6 "STS" 2))
   (cons 'STS  (instr 6 "STS k,Rr" 2))

   (cons 'LDS  (instr 7 "LDS" 2))
   (cons 'LDS  (instr 7 "LDS Rr,k" 2))

   (cons 'JMP  (instr 8 "JMP" 3))

   (cons 'RJMP  (instr 9 "RJMP" 2))
   
   (cons 'RCALL  (instr 10 "RCALL" 3))

   (cons 'CALL  (instr 11 "CALL" 4))

   (cons 'RET  (instr 12 "RET" 4))

   (cons 'EOR  (instr 13 "EOR" 1))
   (cons 'EOR  (instr 13 "EOR Rd,Rr" 1))

   (cons 'INC  (instr 14 "INC" 1))
   (cons 'INC  (instr 14 "INC Rd" 1))

   (cons 'DEC  (instr 15 "DEC" 1))
   (cons 'DEC  (instr 15 "DEC Rd" 1))

   (cons 'CPSE  (instr 16 "CPSE" (list 1 2 3)))
   (cons 'CPSE  (instr 16 "CPSE Rd,Rr" (list 1 2 3)))

   (cons 'OUT  (instr 17 "OUT" 1))
   (cons 'OUT  (instr 17 "OUT A,Rr" 1))

   (cons 'IN  (instr 18 "IN" 1))
   (cons 'IN  (instr 18 "IN Rd,A" 1))
   
   (cons 'PUSH  (instr 19 "PUSH" 2))
   (cons 'PUSH  (instr 19 "PUSH Rd" 2))

   (cons 'POP  (instr 20 "POP" 2))
   (cons 'POP  (instr 20 "POP Rd" 2))

   (cons 'CPI  (instr 21 "CPI" 1))
   (cons 'CPI  (instr 21 "CPI Rd,K" 1))
   
   (cons 'CP  (instr 22 "CP" 1))
   (cons 'CP  (instr 22 "CP Rd,Rr" 1))

   (cons 'CPC  (instr 23 "CPC" 1))
   (cons 'CPC  (instr 23 "CPC Rd,Rr" 1))

   (cons 'SUB  (instr 240 "SUB" 1))
   (cons 'SUB  (instr 240 "SUB Rd,Rr" 1))

   (cons 'SUBI  (instr 24 "SUBI" 1))
   (cons 'SUBI  (instr 24 "SUBI Rd,K" 1))

   (cons 'ADIW  (instr 25 "ADIW" 2))
   (cons 'ADIW  (instr 25 "ADIW Rd,K" 2))

   (cons 'SBIW  (instr 26 "SBIW" 2))
   (cons 'SBIW  (instr 26 "SBIW Rd,K" 2))

   (cons 'CBI  (instr 27 "CBI" 2))
   (cons 'CBI  (instr 27 "CBI A,b" 2))
   
   (cons 'SBI  (instr 28 "SBI" 2))
   (cons 'SBI  (instr 28 "SBI A,b" 2))

   (cons 'SBIC  (instr 29 "SBIC" (list 1 2)))
   (cons 'SBIC  (instr 29 "SBIC A,b" (list 1 2)))

   (cons 'SBCI  (instr 30 "SBCI" 1))
   (cons 'SBCI  (instr 30 "SBCI Rd,K" 1))

   (cons 'SBRC  (instr 31 "SBRC" (list 1 2 3)))
   (cons 'SBRC  (instr 31 "SBRC Rr,b" (list 1 2 3)))

   (cons 'SBRS  (instr 3100 "SBRS" (list 1 2 3)))
   (cons 'SBRS  (instr 3100 "SBRS Rr,b" (list 1 2 3)))

   (cons 'BRBC  (instr 32 "BRBC" (list 1 2)))
   (cons 'BRNE  (instr 3200 "BRNE" (list 1 2)))

   (cons 'BREQ  (instr 33 "BREQ" (list 1 2)))

   (cons 'BRBS  (instr 34 "BRBS" (list 1 2)))
   
   (cons 'BRTC  (instr 35 "BRTC" (list 1 2)))

   (cons 'BRTS  (instr 36 "BRTS" (list 1 2)))

   (cons 'BRCC  (instr 37 "BRCC" (list 1 2)))

   (cons 'ADD  (instr 38 "ADD" 1))
   (cons 'ADD  (instr 38 "ADD Rd,Rr" 1))

   (cons 'ADC  (instr 39 "ADC" 1))
   (cons 'ADC  (instr 39 "ADC Rd,Rr" 1))

   (cons 'AND  (instr 40 "AND" 1))
   (cons 'AND  (instr 40 "AND Rd,Rr" 1))

   (cons 'ROR  (instr 41 "ROR" 1))
   (cons 'ROR  (instr 41 "ROR Rd" 1))

   (cons 'LSR  (instr 42 "LSR" 1))
   (cons 'LSR  (instr 42 "LSR Rd" 1))

   (cons 'BST  (instr 43 "BST" 1))
   (cons 'BST  (instr 43 "BST Rd" 1))

   (cons 'CLI  (instr 44 "CLI" 1))

   (cons 'CLC  (instr 45 "CLC" 1))

   (cons 'BCLR  (instr 46 "BCLR" 1))
   (cons 'CLT  (instr 4600 "CLT" 1))

   (cons 'SEC  (instr 47 "SEC" 1))

   (cons 'BSET  (instr 48 "BSET" 1))
   (cons 'SET  (instr 4800 "SET" 1))

   (cons 'SBC  (instr 49 "SBC" 1))
   
   (cons 'NOP  (instr 50 "NOP" 1))

   (cons 'SWAP  (instr 51 "SWAP" 1))
   (cons 'SWAP  (instr 51 "SWAP Rd" 1))

   (cons 'MUL  (instr 52 "MUL" 2))
   (cons 'MUL  (instr 52 "MUL Rd,Rr" 2))

   (cons 'OR  (instr 520 "OR" 1))
   (cons 'ANDI  (instr 53 "ANDI" 1))
   (cons 'ANDI  (instr 53 "ANDI Rd,K" 1))

   (cons 'NEG  (instr 54 "NEG" 1))
   (cons 'NEG  (instr 54 "NEG Rd" 1))
   ))

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
