; CREF.ASM
;********************************;
;
;          INTEL ASSEMBLER
;       CROSS REFERENCE PROGRAM
;
;           VERSION 1.0
;
;         JEFF KRAVITZ
;
;********************************;

;********************************;
;
;          MAIN LOOP
;
;********************************;
             ORG        100H                 ;ORIGIN ADDRESS
XREF:        LXI        SP,STACK             ;SET STACK POINTER
             CALL       SETUP                ;INITIALIZE
MAIN:        CALL       GETBT                ;GET A BYTE FROM SOURCE FILE
             CALL       SAVBT                ;SAVE BYTE IN PRINT BUFFER
MAIN2:       CALL       CKNUM                ;TEST FOR NUMERIC
             JNC        LNUM                 ;YES, FOUND A NUMBER, PROCESS
             CALL       CKALP                ;TEST FOR ALPHABETIC
             JNC        LALPH                ;YES, PROCESS
             LXI        H,CTAB1              ;POINT TO CHARACTER TABLE
             CALL       LOOK                 ;LOOK UP CHAR IN CHAR TABLE
             JC         LIGN                 ;NOT FOUND, IGNORE
             PCHL                            ;EXECUTE ROUTINE

;********************************;
;
;      FINAL SYMBOL TABLE PRINT
;
;********************************;
DONE:        CALL       PAGE                 ;ISSUE PAGE EJECT
             LHLD       SYMBT                ;GET SYMBOL TABLE BOTTOM
             SHLD       SYM                  ;SET SYMBOL POINTER
             LHLD       SYMTP                ;GET SYMBOL TABLE TOP
             MVI        M,0FFH               ;END OFF SYMBOL TABLE
DLP1:        LHLD       SYM                  ;GET SYMBOL TABLE POINTER
             CALL       PSYM                 ;PRINT SYMBOL
             LHLD       SYM
             LXI        D,6                  ;OFFSET TO REF LINK
             DAD        D
             MOV        E,M
             INX        H
             MOV        D,M                  ;GET REF BLOCK ADDR
             XCHG                            ;INTO HL
             SHLD       REF
             CALL       PREFS                ;PRINT REFERENCES
             LHLD       SYM                  ;GET SYMBOL TABLE POINTER
             LXI        D,SSIZ               ;SIZE OF SYM TABLE ENTRY
             DAD        D
             SHLD       SYM
             MOV        A,M                  ;GET BYTE
             CPI        0FFH                 ;END OF TABLE?
             JZ         BOOT
             JMP        DLP1                 ;LOOP

;********************************;
;
;     SYMBOL PRINT ROUTINE
;
;********************************;
PSYM:        MVI        B,5                  ;SYMBOL SIZE
PSYM2:       MOV        E,M                  ;GET BYTE
             CALL       PBYT                 ;PRINT BYTE
             INX        H
             DCR        B
             JNZ        PSYM2
             MVI        E,' '
             CALL       PBYT                 ;PRINT 2 SPACES
             CALL       PBYT
             RET

;********************************;
;
;     REFERENCE PRINT ROUTINE
;
;********************************;
PREFS:       LHLD       REF                  ;GET REF BLOCK ADDR
             INX        H
             INX        H                    ;BUMP TO FIRST REF NUMBER
             SHLD       TEMP                 ;SAVE REF NUM ADDR
             MVI        A,(REFSZ-2)/2        ;NUMBER OF REF SLOTS
             STA        SYMCT                ;SAVE IN SYMCT
PREF:        LHLD       TEMP                 ;GET REF SLOT ADDR
             MOV        E,M
             INX        H
             MOV        D,M                  ;GET REF
             LXI        H,0000               ;ZERO?
             CALL       CPHL
             JZ         PREFX                ;YES, DONE
             XCHG                            ;GET NUM IN HL
             CALL       DECOT                ;CONVERT
             LXI        H,DEC                ;POINT TO DEC STRING
             MVI        M,' '                ;BLANK LEADING ZERO
             MVI        B,5
PREF2:       MOV        E,M
             CALL       PBYT                 ;PRINT BYTE
             INX        H
             DCR        B
             JNZ        PREF2                ;PRINT REFERENCE NUMBER
             LHLD       TEMP                 ;GET REF SLOT ADDR
             INX        H
             INX        H                    ;BUMP TO NEXT SLOT
             SHLD       TEMP
             LDA        SYMCT                ;GET COUNT
             DCR        A                    ;DECREMENT
             STA        SYMCT
             JNZ        PREF
             LHLD       REF                  ;GET REF BLOCK ADDRESS
             MOV        E,M
             INX        H
             MOV        D,M                  ;GET LINK TO NEXT BLOCK
             LXI        H,0000
             CALL       CPHL                 ;ANY MORE BLOCKS?
             JZ         PREFX                ;NO, EXIT
             XCHG                            ;YES, SET NEXT BLOCK POINTER IN REF
             SHLD       REF
             CALL       CRLF                 ;PRINT CR,LF
             MVI        B,07
PREF3:       MVI        E,' '
             CALL       PBYT                 ;PRINT SPACES
             DCR        B
             JNZ        PREF3                ;PRINT 6 SPACES
             JMP        PREFS
PREFX:       CALL       CRLF                 ;PRINT CR,LF
             RET

;********************************;
;
;   CHARACTER PARSING ROUTINES
;
;********************************;
LALPH:       LXI        H,SBUF               ;POINT TO SYMBOL BUFFER
             MVI        C,05
             MVI        A,' '
LALX:        MOV        M,A
             INX        H
             DCR        C
             JNZ        LALX                 ;CLEAR SYMBOL BUFFER
             LXI        H,SBUF
             SHLD       SYMPT
             MVI        A,00
             STA        SYMCT                ;RESET SYMBOL POINTER+COUNT
             LDA        CHAR                 ;GET CHARACTER AGAIN
             CALL       GTSYM                ;COLLECT IDENTIFIER
LALC:        CALL       GETBT                ;GET A BYTE FROM SOURCE FILE
             CALL       SAVBT                ;SAVE BYTE IN PRINT BUFFER
             CALL       CKNUM                ;TEST FOR NUMBER
             JNC        LAL3                 ;YES, CONTINUE
             CALL       CKALP                ;TEST FOR ALPHABETIC
             JNC        LAL3                 ;YES, CONTINUE
             CALL       CRES                 ;TEST FOR RESERVED WORD
             JC         LAL1                 ;NO, CONTINUE
LAL0:        LDA        CHAR                 ;GET CHARACTER THAT ENDED ID
             JMP        MAIN2                ;CONTINUE SCAN
LAL1:        CALL       FIND                 ;SEE IF DEFINED
             JC         LAL2                 ;NO, CONTINUE
             CALL       ADDRF                ;YES, ADD REFERENCE
             JMP        LAL0                 ;DONE
LAL2:        CALL       ENSYM                ;ENTER SYMBOL DEFINITION
             CALL       ADDRF                ;ADD REFERENCE
             JMP        LAL0                 ;CONTINUE
LAL3:        CALL       GTSYM                ;COLLECT IDENTIFIER
             JMP        LALC                 ;CONTINUE

LNUM:        CALL       GETBT                ;GET BYTE
             CALL       SAVBT                ;SAVE BYTE IN PRINTER BUFFER
             CALL       CKNUM                ;TEST FOR NUMERIC
             JNC        LNUM                 ;YES, CONTINUE
             CALL       CKALP                ;TEST FOR ALPHABETIC
             JNC        LNUM                 ;YES, CONTINUE
             JMP        MAIN2                ;CONTINUE WITH MAIN SCAN

LQUOT:       CALL       GETBT                ;GET A BYTE
             CALL       SAVBT                ;SAVE BYTE IN PRINTER BUFFER
             CPI        ''''                 ;SEE IF STRING QUOTE
             JNZ        LQUOT                ;NO, KEEP LOOPING
             CALL       GETBT                ;GET NEXT BYTE
             CALL       SAVBT                ;SAVE BYTE
             CPI        ''''                 ;TEST FOR DOUBLES
             JZ         LQUOT                ;YES, START SCAN AGAIN
             JMP        MAIN2                ;NO, CONTINUE IN MAIN SCAN

LSEMI:       CALL       GETBT                ;GET A BYTE
             CALL       SAVBT                ;SAVE BYTE
             CPI        0DH                  ;WAIT FOR CR
             JNZ        LSEMI                ;CONTINUE
             JMP        MAIN2                ;ENTER MAIN LOOP

LCR:         CALL       PLINE                ;PRINT LINE
             LHLD       LCNT                 ;GET LINE NUMBER
             INX        H                    ;BUMP LINE NUMBER
             SHLD       LCNT                 ;STORE
             JMP        MAIN                 ;CONTINUE

LIGN:        JMP        MAIN                 ;RE-ENTER MAIN LOOP

LLF          EQU        LIGN
LSPC         EQU        LIGN
LTAB         EQU        LIGN
LDOL         EQU        LIGN
LDEL         EQU        LIGN

;********************************;
;
;          SUBROUTINES
;
;********************************;

;********************************;
;
;         INITIALIZATION
;
;********************************;
SETUP:       LXI        D,TFCB               ;POINT TO FCB
             CALL       FOPEN                ;OPEN FCB
             LXI        H,PBUF
             SHLD       LPNT                 ;SET PRINT POINTER
             LXI        H,00001
             SHLD       LCNT
             LXI        H,SYMT               ;GET ADDRESS OF SYMBOL TABLE
             SHLD       SYM
             SHLD       SYMBT
             SHLD       SYMTP                ;SET SYMBOL TABLE POINTERS
             LHLD       MEMSZ                ;GET AVAILABLE MEMORY ADDRESS
             DCX        H
             SHLD       REF
             SHLD       REFBT
             SHLD       REFTP                ;SET REFERENCE TABLE POINTERS
             CALL       PAGE                 ;ISSUE PAGE EJECT
             RET

;********************************;
;
;    CHECK FOR RESERVED WORD
;
;********************************;
CRES:        LXI        H,RTAB               ;POINT TO RESERVED WORD TABLE
             SHLD       TEMP                 ;SAVE IN TEMP WORD
CRES1:       LHLD       TEMP                 ;GET TABLE POINTER
             LXI        D,SBUF               ;POINT TO SYMBOL
             MVI        B,5                  ;SYMBOL SIZE
CRES2:       LDAX       D                    ;GET SYMBOL BYTE
             CMP        M                    ;COMPARE AGAINST TABLE ENTRY
             RC                              ;LESS, NOT IN TABLE
             JNZ        CRES3                ;GREATER, GET NEXT TABLE ENTRY
             INX        D                    ;BUMP POINTERS
             INX        H
             DCR        B                    ;DECREMENT BYTE COUNT
             JNZ        CRES2                ;KEEP TESTING
             JMP        CRES4                ;FOUND
CRES3:       LHLD       TEMP                 ;GET TABLE POINTER
             LXI        D,RSIZ               ;SIZE OF ENTRY
             DAD        D                    ;BUMP POINTER
             SHLD       TEMP                 ;STORE NEW POINTER
             MOV        A,M                  ;GET TABLE BYTE
             CPI        0FFH                 ;END OF TABLE?
             JNZ        CRES1                ;NO, LOOP
             STC                             ;SET CARRY (NOT IN TABLE)
             RET
CRES4:       ORA        A                    ;RESET CARRY
             RET

;********************************;
;
;     FIND SYMBOL IN TABLE
;
;********************************;
FIND:        LHLD       SYMBT                ;GET BEGIN OF SYM TABLE
             SHLD       SYM                  ;SET TEMP POINTER
FIND1:       LHLD       SYM                  ;GET TEMP POINTER
             LXI        D,SBUF               ;POINT TO CURRENT SYMBOL
             MVI        B,5                  ;SYMBOL SIZE
FIND2:       LDAX       D                    ;GET BYTE FROM SBUF
             CMP        M                    ;COMPARE TO SYM TABLE BYTE
             RC                              ;GREATER, NOT IN TABLE
             JNZ        FIND3                ;LESS, GET NEXT TABLE ENTRY
             INX        D                    ;BUMP POINTER
             INX        H                    ;BUMP POINTER
             DCR        B                    ;DECREMENT BYTE COUNT
             JNZ        FIND2                ;LOOP
             RET                             ;TRUE ZERO, FOUND
FIND3:       LHLD       SYM                  ;GET CURRENT POINTER
             LXI        D,SSIZ               ;SYMBOL TABLE ENTRY SIZE
             DAD        D                    ;BUMP POINTER
             XCHG                            ;INTO DE
             LHLD       SYMTP                ;GET TOP OF SYMBOL TABLE
             CALL       CPHL                 ;TEST FOR END OF TABLE
             JZ         FIND4                ;YES, DONE
             JC         FERR                 ;TABLE OVERFLOW, ERROR
             XCHG                            ;CURRENT POINTER INTO HL
             SHLD       SYM                  ;SET CURRENT POINTER
             JMP        FIND1                ;LOOP
FIND4:       STC                             ;SET CARRY FOR NOT FOUND
             LHLD       SYMTP                ;GET CURRENT TOP
             SHLD       SYM                  ;SET CURRENT POINTER
             RET
FERR:        LXI        D,EMSG1              ;POINTER TO ERROR MESSAGE
             MVI        C,09                 ;WRITE CONSOLE
             CALL       CPM                  ;ISSUE ERROR MESSAGE
             JMP        BOOT                 ;EXIT

;********************************;
;
;   ADD REFERENCE TO REF TABLE
;
;********************************;
ADDRF:       LHLD       SYM                  ;GET SYMBOL POINTER
             LXI        D,6                  ;OFFSET PAST SYMBOL&FLAGS
             DAD        D
             MOV        E,M
             INX        H
             MOV        D,M                  ;GET REFERENCE POINTER
             LXI        H,0000
             CALL       CPHL                 ;TEST FOR ZERO REF PTR
             JZ         BLDRF                ;YES, BUILD REFERENCE ENTRY
LINK:        XCHG                            ;REF PTR IN HL
             MOV        E,M                  ;GET REF LINK
             INX        H
             MOV        D,M                  ;INTO DE
             DCX        H                    ;REPOSITION HL
             PUSH       H                    ;SAVE REF PTR
             LXI        H,0000
             CALL       CPHL                 ;IF LINK IS ZERO
             POP        H
             JNZ        LINK                 ;NON ZERO, GET NEXT LINK
             SHLD       REF                  ;SAVE REF POINTER
             INX        H
             INX        H                    ;SKIP TO FIRST REF NUMBER
             MVI        B,(REFSZ-2)/2        ;NUMBER OF REF NUMBERS/ENTRY
LINK3:       MOV        E,M                  ;GET REF NUMBER
             INX        H
             MOV        D,M
             DCX        H                    ;REPOSITION
             PUSH       H                    ;SAVE REF NUM ADDR
             LXI        H,0000
             CALL       CPHL                 ;SEE IF REF NUM IS ZERO
             POP        H
             JZ         ENREF                ;YES, ENTER REFERENCE
             INX        H
             INX        H                    ;SKIP TO NEXT REF NUM
             DCR        B                    ;DECREMENT COUNT
             JNZ        LINK3                ;TRY AGAIN AT NEXT SLOT
             CALL       ADBLK                ;ADD NEW REF BLOCK
             LHLD       REF                  ;GET REF POINTER
             INX        H
             INX        H                    ;SKIP TO FIRST REF SLOT
ENREF:       PUSH       H                    ;SAVE REF SLOT ADDR
             LHLD       LCNT                 ;GET LINE NUMBER
             XCHG                            ;INTO DE
             POP        H                    ;GET REF SLOT ADDR
             MOV        M,E
             INX        H
             MOV        M,D                  ;STORE LINE REF
             RET                             ;DONE
;********************************;
;
;     BUILD REF TABLE BLOCK
;
;********************************;
BLDRF:       LHLD       SYM                  ;GET SYMBOL POINTER
             LXI        D,6                  ;OFFSET TO REF POINTER
             DAD        D
             SHLD       REF                  ;SET TEMP REF POINTER TO HERE
             CALL       ADBLK                ;ADD BLOCK
             LHLD       REF                  ;GET REAL REF POINTER
             INX        H
             INX        H                    ;POSITION TO FIRST REF SLOT
             JMP        ENREF                ;ADD REFERENCE
ADBLK:       LHLD       REFBT                ;GET REF BOTTOM
             LXI        D,REFSZ              ;SUBTRACT REF SIZE
             MOV        A,L
             SUB        E
             MOV        L,A
             MOV        A,H
             SBB        D
             MOV        H,A
             SHLD       TEMP                 ;SAVE NEW REF BOTTOM
             XCHG                            ;INTO DE ALSO
             LHLD       SYMTP                ;GET SYMBOL TOP
             CALL       CPHL                 ;CHECK FOR BUMP
             JZ         FERR                 ;YES, NO ROOM
             JNC        FERR                 ;NO ROOM
             LHLD       TEMP                 ;GET REF BOTTOM
             XCHG                            ;INTO DE
             LHLD       REF                  ;GET REF POINTER
             MOV        M,E                  ;SET LINK
             INX        H
             MOV        M,D                  ;TO NEW REF BLOCK
             LHLD       TEMP                 ;GET NEW REF BLOCK ADDR
             SHLD       REF                  ;STORE IN REF
             MVI        B,REFSZ              ;SIZE OF REF BLOCK
             MVI        A,00
ADB2:        MOV        M,A                  ;ZERO THE REF BLOCK
             INX        H
             DCR        B
             JNZ        ADB2
             LHLD       TEMP                 ;GET NEW REF BOTTOM
             SHLD       REFBT                ;SET REFBT
             RET

;********************************;
;
;     ENTER SYMBOL IN SYM TABLE
;
;********************************;
ENSYM:       LHLD       SYM                  ;GET SYMBOL POINTER
             XCHG                            ;INTO DE
             LHLD       SYMTP                ;GET SYMBOL TABLE TOP
             CALL       CPHL                 ;CHECK FOR END OF TABLE
             JZ         NWSYM                ;YES, ADD SYMBOL AT END
             LXI        D,SSIZ               ;SYMBOL TABLE ENTRY SIZE
             DAD        D                    ;CALCULATE NEW END OF TABLE
             XCHG                            ;INTO DE
             LHLD       REFBT                ;REFERENCE TABLE BOTTOM
             CALL       CPHL                 ;TEST FOR TABLE OVERFLOW
             JZ         FERR                 ;FULL, ERROR
             JC         FERR                 ;YES, ERROR
             LHLD       SYMTP                ;GET TABLE TOP
             LXI        D,SSIZ-1             ;BUMP TO END OF ENTRY
             DAD        D
             SHLD       TO                   ;STORE IN TO ADDRESS
             LXI        D,SSIZ
             MOV        A,L
             SUB        E
             MOV        L,A
             MOV        A,H
             SBB        D
             MOV        H,A                  ;SUBTRACT SIZE OF ONE ENTRY
             SHLD       FROM                 ;STORE AS FROM ADDRESS
             LHLD       SYM                  ;GET CURRENT POINTER
             SHLD       LIMIT                ;STORE AS LIMIT ADDRESS
             CALL       MVUP                 ;MOVE TABLE UP IN MEMORY
NWSYM:       LHLD       SYM                  ;GET CURRENT POINTER
             LXI        D,SBUF               ;POINT TO SYMBOL
             MVI        B,5                  ;SIZE OF SYMBOL
             CALL       MOVE                 ;COPY SYMBOL TO TABLE
             MVI        A,0
             MOV        M,A
             INX        H
             MOV        M,A
             INX        H
             MOV        M,A                  ;SET POINTERS TO 0000
             LHLD       SYMTP                ;GET SYMBOL TABLE TOP
             LXI        D,SSIZ               ;GET SYMBOL ENTRY SIZE
             DAD        D                    ;BUMP
             SHLD       SYMTP                ;STORE EW TOP
             RET

;********************************;
;
;    MOVE SYMBOL TABLE UP
;
;********************************;
MVUP:        LHLD       TO                   ;GET TO POINTER
             MOV        B,H
             MOV        C,L                  ;INTO BC
             LHLD       FROM                 ;GET FROM POINTER
             XCHG                            ;INTO DE
             LHLD       LIMIT                ;GET LIMIT ADDRESS
MVUP2:       LDAX       D                    ;GET FROM BYTE
             STAX       B                    ;STORE AT TO ADDRESS
             CALL       CPHL                 ;COMPARE FROM TO LIMIT
             RZ                              ;EXIT IF DONE
             DCX        B                    ;DECREMENT TO
             DCX        D                    ;DECRMENT FROM
             JMP        MVUP2                ;LOOP

;********************************;
;
;  GENERAL PURPOSE MOVE ROUTINE
;
;********************************;
MOVE:        LDAX       D                    ;GET BYTE
             MOV        M,A                  ;STORE BYTE
             INX        D
             INX        H                    ;BUMP POINTERS
             DCR        B                    ;DECREMENT COUNT
             JNZ        MOVE                 ;LOOP
             RET

;********************************;
;
;    BINARY TO DECIMAL CONVERSION
;
;********************************;
DECOT:       LXI        D,DEC
             XCHG
             LXI        B,10000
             CALL       DIG
             LXI        B,1000
             CALL       DIG
             LXI        B,100
             CALL       DIG
             LXI        B,10
             CALL       DIG
             LXI        B,1
             CALL       DIG
             RET

DIG:         MVI        M,'0'
DI0:         MOV        A,E
             SUB        C
             MOV        E,A
             MOV        A,D
             SBB        B
             MOV        D,A
             JM         DI2
             INR        M
             JMP        DI0
DI2:         MOV        A,E
             ADD        C
             MOV        E,A
             MOV        A,D
             ADC        B
             MOV        D,A
             INX        H
             RET

;********************************;
;
;    TEST FOR ALPHABETIC CHAR.
;
;********************************;
CKALP:       CPI        'A'                  ;ASCII 'A'
             RC                              ;NO, EXIT
             CPI        'Z'+1
             CMC
             RET

;********************************;
;
;       TEST FOR NUMERIC CHAR
;
;********************************;
CKNUM:       CPI        '0'
             RC
             CPI        '9'+1
             CMC
             RET

;********************************;
;
;  LOOK UP CHAR IN PARSE TABLE
;
;********************************;
LOOK:        LXI        D,0003               ;TABLE ENTRY SIZE
             MOV        B,A                  ;ARGUMENT BYTE IN B
LOOK2:       MOV        A,M                  ;GET TABLE BYTE
             CPI        0FFH                 ;END OF TABLE?
             JZ         LOOKN                ;YES, NOT FOUND
             CMP        B                    ;COMPARE
             JZ         LOOKY                ;FOUND
             DAD        D                    ;BUMP POINTER
             JMP        LOOK2                ;LOOP
LOOKN:       STC                             ;CARRY = NOT FOUND
             RET

LOOKY:       INX        H                    ;SKIP TO TABLE BYTE
             MOV        E,M
             INX        H
             MOV        D,M                  ;TABLE ENTRY IN DE
             XCHG                            ;INTO HL
             RET

;********************************;
;
;    SAVE BYTE IN LINE BUFFER
;
;********************************;
SAVBT:       STA        CHAR                 ;SAVE CHAR IN CHAR
             LHLD       LPNT                 ;GET LINE POINTER
             MOV        M,A                  ;SAVE BYTE
             INX        H                    ;BUMP POINTER
             SHLD       LPNT                 ;SAVE POINTER
             RET

;********************************;
;
;  PRINT SOURCE LINE WITH NUMBER
;
;********************************;
PLINE:       LHLD       LCNT                 ;GET LINE NUMBER
             CALL       DECOT                ;CONVERT TO DECIMAL
             LXI        H,DEC                ;POINT TO DEC STRING
PL2:         MOV        E,M                  ;GET STRING BYTE
             MOV        A,E
             CPI        0DH                  ;DONE?
             JZ         PL3                  ;YES
             CALL       PBYT                 ;PRINT BYTE
             INX        H                    ;BUMP POINTER
             JMP        PL2
PL3:         MVI        E,':'
             CALL       PBYT                 ;PRINT ':'
             MVI        E,' '
             CALL       PBYT                 ;PRINT ' '
             CALL       PBYT                 ;PRINT SPACE
             LXI        H,PBUF               ;POINT TO PRINT BUFFER
PL4:         MVI        A,00
             STA        COL                  ;SET COLUMN COUNT
PL41:        MOV        E,M                  ;GET BYTE
             MOV        A,E
             CPI        0DH                  ;DONE?
             JZ         PL5
             CPI        0AH                  ;LF?
             JZ         PL4A                 ;YES, IGNORE
             CPI        09H                  ;TAB?
             JNZ        PL42                 ;NO, CONTINUE
             PUSH       H                    ;SAVE HL
PL43:        MVI        E,' '
             CALL       PBYT                 ;PRINT SPACE
             LXI        H,COL
             INR        M
             MOV        A,M
             ANI        07H                  ;MODULO 8
             JNZ        PL43
             POP        H
             JMP        PL4A
PL42:        LDA        COL
             INR        A
             STA        COL
             CALL       PBYT                 ;PRINT BYTE
PL4A:        INX        H
             JMP        PL41
PL5:         CALL       CRLF                 ;PRINT CR,LF
             LXI        H,PBUF
             SHLD       LPNT                 ;RESET LINE POINTER
             RET

;********************************;
;
;      COLLECT SYMBOL IN SYM BUF
;
;********************************;
GTSYM:       MOV        B,A                  ;SAVE CHAR
             LDA        SYMCT                ;GET SYMBOL COUNT
             CPI        05                   ;MAX?
             RNC                             ;YES, DONE
             INR        A
             STA        SYMCT
             LHLD       SYMPT
             MOV        M,B
             INX        H                    ;BUMP SYMBOL POINTER
             SHLD       SYMPT
             RET

;********************************;
;
;       PRINTER INTERFACES
;
;********************************;

;********************************;
;
;       PRINT A SINGLE BYTE
;
;********************************;
PBYT:        PUSH       B
             PUSH       D
             PUSH       H
             MVI        C,05
             CALL       CPM
             MVI        C,11                 ;CHECK CONSOLE STATUS
             CALL       CPM
             ORA        A                    ;IF ZERO, OK
             JNZ        BOOT                 ;EXIT
             POP        H
             POP        D
             POP        B
             RET

;*********************************;
;
;      ISSUE PAGE EJECT
;
;*********************************;
PAGE:        MVI        E,0CH
             CALL       PBYT
             MVI        E,00H
             MVI        B,10
PAGE2:       CALL       PBYT                 ;PRINT 10 NULLS
             DCR        B
             JNZ        PAGE2
             MVI        A,00
             STA        LINES                ;SET LINE COUNT
             RET

;********************************;
;
;      ISSUE CR, LF & TEST PAGE
;
;********************************;
CRLF:        MVI        E,0DH
             CALL       PBYT
             MVI        E,0AH
             CALL       PBYT
             MVI        E,00
             CALL       PBYT
             CALL       PBYT                 ;PRINT 2 NULLS
             LDA        LINES
             INR        A
             STA        LINES                ;INCREMENT LINE COUNT
             CPI        56                   ;TEST LINE COUNT
             CZ         PAGE                 ;IF 56 THEN NEW PAGE
             RET


;********************************;
;
;       CHARACTER PARSING TABLE
;
;********************************;
CTAB1:       DB         0DH
             DW         LCR
             DB         0AH
             DW         LLF
             DB         ''''
             DW         LQUOT
             DB         '                    ;'
             DW         LSEMI
             DB         ' '
             DW         LSPC
             DB         09H
             DW         LTAB
             DB         '$'
             DW         LDOL
             DB         '('
             DW         LDEL
             DB         ')'
             DW         LDEL
             DB         '+'
             DW         LDEL
             DB         '-'
             DW         LDEL
             DB         '*'
             DW         LDEL
             DB         '/'
             DW         LDEL
             DB         ','
             DW         LDEL
             DB         ':'
             DW         LDEL
             DB         EOF
             DW         DONE
             DB         0FFH
             DW         0000H
EOF          EQU        1AH                  ;EOF CODE

;********************************;
;
;     RESERVED WORD TABLE
;
;********************************;
RTAB:        DB         'A    '
             DB         'ACI  '
             DB         'ADC  '
             DB         'ADD  '
             DB         'ADI  '
             DB         'ANA  '
             DB         'AND  '
             DB         'ANI  '
             DB         'B    '
             DB         'C    '
             DB         'CALL '
             DB         'CC   '
             DB         'CM   '
             DB         'CMA  '
             DB         'CMC  '
             DB         'CMP  '
             DB         'CNC  '
             DB         'CNZ  '
             DB         'CP   '
             DB         'CPE  '
             DB         'CPI  '
             DB         'CPO  '
             DB         'CZ   '
             DB         'D    '
             DB         'DAA  '
             DB         'DAD  '
             DB         'DB   '
             DB         'DCR  '
             DB         'DCX  '
             DB         'DI   '
             DB         'DS   '
             DB         'DW   '
             DB         'E    '
             DB         'EI   '
             DB         'END  '
             DB         'ENDIF'
             DB         'ENDM '
             DB         'EQU  '
             DB         'H    '
             DB         'HLT  '
             DB         'IF   '
             DB         'IN   '
             DB         'INR  '
             DB         'INX  '
             DB         'JC   '
             DB         'JM   '
             DB         'JMP  '
             DB         'JNC  '
             DB         'JNZ  '
             DB         'JP   '
             DB         'JPE  '
             DB         'JPO  '
             DB         'JZ   '
             DB         'L    '
             DB         'LDA  '
             DB         'LDAX '
             DB         'LHLD '
             DB         'LXI  '
             DB         'M    '
             DB         'MACRO'
             DB         'MOD  '
             DB         'MOV  '
             DB         'MVI  '
             DB         'NOP  '
             DB         'NOT  '
             DB         'OR   '
             DB         'ORA  '
             DB         'ORG  '
             DB         'ORI  '
             DB         'OUT  '
             DB         'PCHL '
             DB         'POP  '
             DB         'PSW  '
             DB         'PUSH '
             DB         'RAL  '
             DB         'RAR  '
             DB         'RC   '
             DB         'RET  '
             DB         'RLC  '
             DB         'RM   '
             DB         'RNC  '
             DB         'RNZ  '
             DB         'RP   '
             DB         'RPE  '
             DB         'RPO  '
             DB         'RRC  '
             DB         'RST  '
             DB         'RZ   '
             DB         'SBB  '
             DB         'SBI  '
             DB         'SET  '
             DB         'SHL  '
             DB         'SHLD '
             DB         'SHR  '
             DB         'SP   '
             DB         'SPHL '
             DB         'STA  '
             DB         'STAX '
             DB         'STC  '
             DB         'SUB  '
             DB         'SUI  '
             DB         'TITLE'
             DB         'XCHG '
             DB         'XOR  '
             DB         'XRA  '
             DB         'XRI  '
             DB         'XTHL '
             DB         0FFH                 ;END OF RESERVED WORD TABLE
RSIZ         EQU        05                   ;SIZE OF TABLE ENTRY

;********************************;
;
;       MISCELLANEOUS DATA
;
;********************************;

EMSG1:       DB         'SYMBOL TABLE ERROR',0DH,0AH,'$'
SSIZ         EQU        8                    ;SYMBOL TABLE ENTRY SIZE
SYMBT:       DS         2                    ;SYMBOL TABLE BOTTOM ADDRESS
SYMTP:       DS         2                    ;SYMBOL TABLE TOP ADDRESS
REFBT:       DS         2                    ;REFERENCE TABLE BOTTOM ADDRESS
REFTP:       DS         2                    ;REFERENCE TABLE TOP ADDRESS
SYM:         DS         2                    ;CURRENT SYMBOL TABLE ADDRESS
REFSZ        EQU        2+(8*2)              ;NUMBER OF BYTES IN REF BLOCK
REF:         DS         2                    ;CURRENT REFERENCE TABLE ADDRESS
FROM:        DS         2                    ;MOVE POINTER
TO:          DS         2                    ;TO POINTER
LIMIT:       DS         2                    ;LIMIT POINTER
COL:         DS         1
CHAR:        DS         1
LCNT:        DS         2                    ;LINE COUNTER
LPNT:        DS         2
DEC:         DS         5
             DB         0DH
PBUF:        DS         132
SYMCT:       DS         1
SYMPT:       DS         2
SBUF:        DS         5
LINES:       DB         0                    ;PRINT LINE COUNT

;********************************;
;
;       OPERATING SYSTEM EQUATES
;
;********************************;

BOOT         EQU        0000H                ;REBOOT ENTRY POINT
CPM          EQU        0005H                ;CPM ENTRY POINT

MEMSZ        EQU        0006H                ;END OF MEMORY POINTER
TBUF         EQU        0080H                ;TRANS. BUFFER
TFCB         EQU        005CH                ;TRANS. FCB

OPEN         EQU        15                   ;OPEN FUNCTION CODE
READ         EQU        20                   ;READ FUNCTION CODE



;********************************;
;           F O P E N            ;
;  ROUTINE TO OPEN A DISK FILE   ;
;                                ;
;  INPUT:     DE=A(FCB)          ;
; OUTPUT:     CARRY=ERROR        ;
;********************************;

FOPEN:       MVI        C,OPEN               ;OPNE CODE
             CALL       CPM                  ;ISSUE OPEN
             CPI        0FFH                 ;ERROR?
             JZ         FOERR                ;YES
             XRA        A                    ;CLEAR CARRY
             RET
FOERR:       STC
             RET



;********************************;
;            G E T B T           ;
;   ROUTINE TO READ A BYTE       ;
;                                ;
;  OUTPUTS:     A=BYTE           ;
;               CARRY=ERROR      ;
;********************************;

GETBT:       LXI        H,TBUF+128
             XCHG                            ;BUFFER END ADDR. IN DE
             LHLD       INPTR                ;CURRENT POINTER IN HL
             CALL       CPHL                 ;TEST FOR END OF BUFFER
             JZ         GETB2                ;YES, READ
GETB1:       MOV        A,M                  ;GET BYTE
             INX        H                    ;BUMP POINTER
             SHLD       INPTR                ;SAVE POINTER
             ORA        A                    ;RESET CARRY
             RET
GETB2:       MVI        C,READ               ;READ CODE
             LXI        D,TFCB               ;FCB ADDRESS
             CALL       CPM                  ;ISSUE READ
             CPI        00                   ;ERROR?
             JNZ        IERR                 ;YES
             LXI        H,TBUF               ;RESET BUFFER POINTER
             SHLD       INPTR
             JMP        GETB1                ;CONTINUE
IERR:        STC
             RET

;********************************;
;   MISCELLANEOUS SUBROUTINES    ;
;********************************;


;********************************;
;             C P H L            ;
;  ROUTINE TO COMPARE HL VS DE   ;
;********************************;

CPHL:        MOV        A,H
             CMP        D
             RNZ
             MOV        A,L
             CMP        E
             RET

;********************************;
;             D A T A            ;
;********************************;

TEMP:        DS         2                    ;TEMP SAVE WORD

INPTR:       DW         TBUF+128             ;INPUT POINTER

             DS         64
STACK        EQU        $

;***********************************;
;         SYMBOL TABLE AREA
;
;    THE SYMBOL TABLE MUST BE
;    THE LAST BYTE OF THE PROGRAM
;
;***********************************;
             ORG        ($ AND 0FFF0H) + 10H ;FORCE 16 BYTE BOUNDARY
SYMT:        DB         0FFH
             END
