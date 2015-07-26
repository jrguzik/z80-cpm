;------------------------------------------------------------------------------
; Contents of this file are copyright Grant Searle
; HEX routines from Joel Owens.
;
; You have permission to use this for NON COMMERCIAL USE ONLY
; If you wish to use it elsewhere, please include an acknowledgement to myself.
;
; http://searle.hostei.com/grant/index.html
;
; eMail: home.micros01@btinternet.com
;
; If the above don't work, please perform an Internet search to see if I have
; updated the web page hosting service.
;
;------------------------------------------------------------------------------

;
; Mods by jguzik
;   - Removed support for second serial point - only port B 57600 8N1
;   - Based on a 4MHGZ crystal for the processor and
;       a 1.85432MHZ for the SIO
;   - Lots of untested stuff right now - the bios disk I/O is currently
;       a work in progress and will hopefully support a MMC card via
;       a shift register and a binary counter to provide performace
;   - C_READSTR is a reimplementaton that hopefully handles tabs
;   - Removed IHEX load from monitor
;   - added XMODEM support to the monitor
;   - added LOADTPA using XMODEM (basically XMODEM to $0100)
;   - added EXECTPA that executes the TPA at $0100
;   - added DMPMEM and EDMEM routines to monitor
;   - added FILLMEM to erase memory
;   - added REPLACEMON that XMODEMS a new monitor down
;   - lots of comments
;
;
; Currently tested programs
;   basic.asm    - as a CP/M program, able to play super star trek
;   bbcbasic.com - generic CP/M version
;   epro.com     - Testbad for the C_READSTR function
;
; Wants
;  PL/I          - Blast from the past
;

;------------------------------------------------------------------------------
; Page ZERO layout
;------------------------------------------------------------------------------
P_TERMCPM    .EQU   $0000
WBOOTJP      .EQU   $0000
WBOOTADDR    .EQU   $0001
IOBYTE       .EQU   $0003
DSKBYTE      .EQU   $0004
BDOSJP       .EQU   $0005
BDOSADDR     .EQU   $0006
RSTV1        .EQU   $0008
RSTV2        .EQU   $0010
RSTV3        .EQU   $0018
RSTV4        .EQU   $0020
RSTV5        .EQU   $0028
RSTV6        .EQU   $0030
RSTV7        .EQU   $0038
WORKAREA     .EQU   $0040
FILEBUFF     .EQU   $0080
TPABASE      .EQU   $0100

;------------------------------------------------------------------------------
; ASCII Ctrl Char Names
;------------------------------------------------------------------------------
NUL          .EQU   $00
SOH          .EQU   $01
STX          .EQU   $02
CTRLC        .EQU   $03                ; Alt ETX
ETX          .EQU   $03
EOT          .EQU   $04
CTRLE        .EQU   $05                ; Alt ENQ
ENQ          .EQU   $05
ACK          .EQU   $06
BEL          .EQU   $07
BS           .EQU   $08
HT           .EQU   $09
TAB          .EQU   $09
LF           .EQU   $0A
VT           .EQU   $0B
CLS          .EQU   $0C                ; Alt FF
; FF           .EQU   $0C              ; TOO Much Like a Hex code
CR           .EQU   $0D
SO           .EQU   $0E
SI           .EQU   $0F
DLE          .EQU   $10
CTRLPRT      .EQU   $10                ; Alt DLE
XON          .EQU   $11                ; ALT DC 1
DC1          .EQU   $11
CTRLR        .EQU   $12                ; Alt DC 2
DC2          .EQU   $12
XOFF         .EQU   $13                ; ALT DC 3
DC3          .EQU   $13
DC4          .EQU   $14
NAK          .EQU   $15
SYN          .EQU   $16
ETB          .EQU   $17
CAN          .EQU   $18
EM           .EQU   $19
SUB          .EQU   $1A
ESC          .EQU   $1B
FS           .EQU   $1C
GS           .EQU   $1D
RS           .EQU   $1E
WS           .EQU   $1F
DEL          .EQU   $7F

;------------------------------------------------------------------------------
; Contants for Serial Devices
;------------------------------------------------------------------------------
SER_BUFSIZE   .EQU  $0100
SER_FULLSIZE  .EQU  $60
SER_EMPTYSIZE .EQU  $05

RTS_HIGH     .EQU   $e8
RTS_LOW      .EQU   $ea

SIOA_D       .EQU   $00
SIOA_C       .EQU   $02
SIOB_D       .EQU   $01
SIOB_C       .EQU   $03

SPKRIO       .EQU   $04

;------------------------------------------------------------------------------
; Contants for disks
;------------------------------------------------------------------------------
DRVCOUNT     .EQU   $01
AVLSIZE      .EQU   257

;------------------------------------------------------------------------------
; Contants for MMC
;------------------------------------------------------------------------------
MMCREADBLK:  .EQU   17
MMCWRITEBLK: .EQU   24
MMCDATAPKT:  .EQU   $fe

MMCSEL       .EQU   $08
MMCDESEL     .EQU   $0c
MMCREAD      .EQU   $10
MMCCLK       .EQU   $12                ; TODO(jguzik) - May need to change
MMCWRITE     .EQU   $13

;------------------------------------------------------------------------------
;                         START OF MONITOR ROM
;------------------------------------------------------------------------------
             .ORG   $F000

FBASE:       JP     CPM22              ; Jump to CPM22

             JP     BOOT               ;-3: Cold start routine
BIOS:        JP     WBOOT              ; 0: Warm boot - reload command processor
             JP     CONST              ; 3: Console status
             JP     CONIN              ; 6: Console input
             JP     CONOUT             ; 9: Console output
             JP     LIST               ;12: Printer output
             JP     PUNCH              ;15: Paper tape punch output
             JP     READER             ;18: Paper tape reader input
             JP     HOME               ;21: Move disc head to track 0
             JP     SELDSK             ;24: Select disc drive
             JP     SETTRK             ;27: Set track number
             JP     SETSEC             ;30: Set sector number
             JP     SETDMA             ;33: Set DMA address
             JP     READ               ;36: Read a sector
             JP     WRITE              ;39: Write a sector

             ; CP/M 2 extra jumps
             JP     LISTST             ;42: Status of list device
             JP     SECTRAN            ;45: Sector translation for skewing

             ; Interrupt vectors
SERIALINTVEC .EQU   $ - FBASE
             .DW    SERIALINT

;------------------------------------------------------------------------------
; BIOS -3 - Cold BOOT
;------------------------------------------------------------------------------
BOOT:        DI                        ; Disable interrupts
             LD     DE,$0000           ; Clear ZERO page
             LD     HL,$0100
             CALL   FILLMEMNXT

             LD     DE,BIOSRAMS        ; Reset BIOS Area
             LD     HL,BIOSRAMSZ
             CALL   FILLMEMNXT

             LD     A,DRVCOUNT
             LD     IX,DPBASE
             LD     DE,DPB0
             LD     HL,ALV00

BOOTDPBASE:  LD     (IX + $0a),D
             LD     (IX + $0b),E
             LD     DE,DPB
             LD     (IX + $09),FILEBUFF
             LD     (IX + $0e),H
             LD     (IX + $0f),L
             DEC    A
             JR     Z,BOOTDPBASED
             LD     BC,AVLSIZE
             ADD    HL,BC
             JR     BOOTDPBASE

BOOTDPBASED:
             LD     IX,DPB0
             LD     (IX + $00),128
             LD     (IX + $02),5
             LD     (IX + $03),31
             LD     (IX + $04),1
             LD     (IX + $05),2043 % 256
             LD     (IX + $06),2043 / 256
             LD     (IX + $07),511  % 256
             LD     (IX + $08),511  / 256
             LD     (IX + $09),248
             LD     (IX + $0e),1

;  Attempting to write only one disk for now, support for multiple later
;            LD     IX,DPB
;            LD     (IX + $00),128
;            LD     (IX + $02),5
;            LD     (IX + $03),31
;            LD     (IX + $04),1
;            LD     (IX + $05),2047 % 256
;            LD     (IX + $06),2047 / 256
;            LD     (IX + $07),511  % 256
;            LD     (IX + $08),511  / 256

             ; Load WBOOT and BDOS Jump Instructions
             LD     A,$C3
             LD     (WBOOTJP),A
             LD     (BDOSJP),A
             LD     HL,WBOOT
             LD     (WBOOTADDR),HL
             LD     HL,FBASE
             LD     (BDOSADDR),HL

             LD     HL,SERBUF          ; Initialized Serial Input buffer
             LD     (SERINPTR),HL
             LD     (SERRDPTR),HL

             XOR    A
             LD    (SERBUFUSED),A

             ; Initialise SIO

             ; ----------------------------------------------------------
             ; Channel A
             LD     A,$00              ; (00)(000)(000) - SEL WR0
             OUT    (SIOA_C),A
             LD     A,$18              ; (00)(011)(000) - 3 Reset Channel
             OUT    (SIOA_C),A

             ; ----------------------------------------------------------
             ; Channel B
             LD     B,$06
             LD     C,SIOB_C
             LD     DE,INITSERREG
             LD     HL,INITSERSEQ
INITSER:     LD     A,(DE)
             OUT    (C),A
             INC    DE
             OUTI
             JR     NZ,INITSER

             ; Interrupt vector in page $f0
             LD     A,$F0
             LD     I,A
             IM     2

             LD     SP,STACK
             LD     C,CLS              ; Clear screen
             CALL   CONOUT

             CALL   BEEP
             JR     WBOOT1

;------------------------------------------------------------------------------
; BIOS  0 - Warm Boot
;------------------------------------------------------------------------------
WBOOT:       LD     SP,STACK           ; Set the Stack Pointer
             CALL   WRCRLF

WBOOTCLR:    LD     (HL),A
             INC    HL
             DJNZ   WBOOTCLR
             DEC    C
             JR     NZ,WBOOTCLR
WBOOT1:      EI                        ; Enable Interrupts

             LD     DE,SIGNON          ; Print SIGNON message
             CALL   C_WRITESTR

;------------------------------------------------------------------------------
; Monitor command loop
;------------------------------------------------------------------------------
MAIN:        LD     HL,MAIN            ; Save entry point for Monitor
             PUSH   HL                 ; This is the return address
MAIN0:       LD     DE,MONPROMPT
             CALL   C_WRITESTR

MAIN1:       CALL   CONIN              ; Get a character from the input port
             CP     ' '                ; <spc> or less?
             JR     C,MAIN0            ; Go back
             CALL   WRCHR              ; Print char on console

             CP     '?'
             JP     Z,HELP

             AND    $5F                ; Make character uppercase
             CP     'R'
             JP     Z,BOOT

             CP     'G'
             JP     Z,GOTO

             CP     'D'
             JP     Z,DMPMEM

             CP     'E'
             JP     Z,EDMEM

             CP     'F'
             JP     Z,FILLMEM

             CP     'L'
             JP     Z,LOADTPA

             CP     'T'
             JP     Z,EXECTPA

             CP     'M'
             JP     Z,REPLACE

             CP     'X'
             JP     Z,XMODEM

             LD     A,'?'              ; Get a "?"
             CALL   C_WRITE            ; Print it
             JR     MAIN0

;------------------------------------------------------------------------------
; BIOS 2 - Check if there is a character in the input buffer
;------------------------------------------------------------------------------
CONST:       LD     A,(SERBUFUSED)
             RET

;------------------------------------------------------------------------------
; BIOS 3 - Wait until the keyboard is ready to provide a character
;          Return
;              A = character
;------------------------------------------------------------------------------
CONIN:       PUSH   HL                ; Save working registers
CONIN1:      LD     A,(SERBUFUSED)    ; Wait for character
             CP     $00
             JR     Z,CONIN1

             LD     HL,(SERRDPTR)     ; Point to next char
             LD     A,L
             INC    A
             OR     $80
             LD     L,A
             DI
             LD     (SERRDPTR),HL

             LD     A,(SERBUFUSED)    ; Count the char
             DEC    A
             LD     (SERBUFUSED),A

             CP     SER_EMPTYSIZE     ; Set RTS if need be
             JR     NC,CONIN2
             LD     A,$05
             OUT    (SIOB_C),A
             LD     A,RTS_LOW
             OUT    (SIOB_C),A

CONIN2:      LD     A,(HL)            ; Load the return char
             EI

             POP    HL                ; Restore working registers
             RET                      ; Char ready in A

;------------------------------------------------------------------------------
; BIOS 4 - Write the character in C to the screen.
;------------------------------------------------------------------------------
CONOUT:      PUSH   AF
CONOUT1:     XOR    A
             OUT    (SIOB_C),A
             IN     A,(SIOB_C)        ; Status byte D2=TX Buff Empty, D0=RX char ready
             RRCA                     ; Rotates RX status into Carry Flag,
             BIT    1,A               ; Set Zero flag if still transmitting character
             JR     Z,CONOUT1         ; Loop until SIO flag signals ready
             LD     A,C               ; Output the character
             OUT    (SIOB_D),A
             POP    AF
             RET

;------------------------------------------------------------------------------
; BIOS 5 - Write the character in C to the printer.
;          If the printer isn't ready, wait until it is.
;------------------------------------------------------------------------------
LIST:        JP     P_TERMCPM         ; Just reboot for now

;------------------------------------------------------------------------------
; BIOS 6 - Write the character in C to the "paper tape punch"
;          - or whatever the current auxiliary device is.
;          If the printer isn't ready, wait until it is.
;------------------------------------------------------------------------------
PUNCH:       JP     P_TERMCPM         ; Just reboot for now

;------------------------------------------------------------------------------
; BIOS 7 - Read a character from the "paper tape reader"
;          - or whatever the current auxiliary device is.
;          If the printer isn't ready, wait until it is.
;          The character will be returned in A.
;          If this device isn't implemented, return character 26 (^Z).
;------------------------------------------------------------------------------
READER:      LD     A,$1A             ; ^Z - not implemented
             RET

;------------------------------------------------------------------------------
; BIOS 8 - Move the current drive to track 0.
;------------------------------------------------------------------------------
HOME:        LD     BC,$0000
             JR     SETTRK

;------------------------------------------------------------------------------
; BIOS 9 - Select the disc drive in register C (0=A:, 1=B: ...)
;          If bit 0 of E is 0, then the disc is logged in as if new;
;             if the format has to be determined from the boot sector,
;             for example, this will be done.
;          If bit 0 if E is 1, then the disc has been logged in before.
;          The disc is not accessed; the DPH address (or zero) is
;               The address of a Disc Parameter Header (DPH) in HL.
;               If the disc could not be selected it returns HL=0
;               The exact format of a DPH varies between CP/M versions
;------------------------------------------------------------------------------
SELDSK:      LD     HL,$0000
             LD     A,C
             CP     DRVCOUNT
             RET    NC                 ; Illegal drive number
             LD     DSKSEL,A
             LD     HL,DPBASE
             ADD    A,A                ; Offset into the DPBASE Array
             ADD    A,A
             ADD    A,A
             ADD    A,A
             LD     B,0
             LD     C,A
             ADD    HL,BC
             RET

;------------------------------------------------------------------------------
; BIOS 10 - Set the track in BC
;------------------------------------------------------------------------------
SETTRK:      LD     DSKTRK,BC
             RET

;------------------------------------------------------------------------------
; BIOS 11 - Set the sector in BC
;------------------------------------------------------------------------------
SETSEC:      LD     DSKSEC,BC
             RET

;------------------------------------------------------------------------------
; BIOS 12 - The next disc operation will read its data from
;           (or write its data to) the address given in BC
;------------------------------------------------------------------------------
SETDMA:      LD     DSKDMA,BC
             RET

;
; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;  Totally untested READ/WRITE routines - checked in for now
; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;

;------------------------------------------------------------------------------
; BIOS 13 - Read the currently set track and sector at the current DMA address.
;           Returns A=0 for OK
;                   A=1 for unrecoverable error
;                   A=0FFh if media changed.
;------------------------------------------------------------------------------
READ:        LD     A,$00
             CALL   NEWTRANSFER
             JR     C,BADREAD          ; Was read successful?
             CALL   BUFFEROFFSET       ; Buffer->DMA transfer
             LDIR
             LD     A,$00              ; Return OK
             RET

BADREAD:     LD     A,$01
             RET

;------------------------------------------------------------------------------
; BIOS 14 - Write the currently set track and sector. C contains a deblocking code:
;           C=0 - Write can be deferred
;           C=1 - Write must be immediate
;           C=2 - Write can be deferred, no pre-read is necessary.
;           Returns A=0 for OK
;                   A=1 for unrecoverable error
;                   A=2 if disc is readonly
;                   A=0FFh if media changed.
;------------------------------------------------------------------------------
WRITE:       PUSH   BC
             LD     A,C
             CALL   NEWTRANSFER
             JR     C,BADWRITE0

             CALL   BUFFEROFFSET       ; DMA->Buffer transfer
             EX     DE,HL
             LDIR

             LD     A,$01              ; Mark buffer as dirty
             LD     (DIRTYBUFFER),A

             POP    BC
             LD     A,C                ; Immediate Write?
             CP     $01
             JR     NZ,WRITERET        ;  No  - can wait
             CALL   WRITESECT          ;  Yes - do it now
             JR     C,BADWRITE1        ;        Checking for error
WRITERET:    LD     A,$00
             RET

BADWRITE0:   POP    BC
BADWRITE1:   LD     A,$01
             RET

;------------------------------------------------------------------------------
; BIOS HLP - Calculate offset into RDBUFFER[HL], loadDMA[DE] and size[BC] = 128
;------------------------------------------------------------------------------
BUFFEROFFSET:LD     A,(DSKSEC)         ; Calc offset into buffer
             AND    $03
             LD     D,A
             LD     E,$00
             LD     HL,RDBUFFER        ; Add to buffer
             ADD    HL,DE
             LD     DE,(DSKDMA)        ; Dest DMA into DE
             LD     C,128              ; count = 128 (one CP/M sector)
             LD     B,$00

;------------------------------------------------------------------------------
; BIOS HLP - New transfer requested from disk
;            A = $00 - possible write, read sector
;                $01 - just possible write
;            Sets F[Carry] on any error
;------------------------------------------------------------------------------
NEWTRANSFER: LD     C,A                ; Save parameter
             LD     A,(DSKSEC)         ; Is the buffer the one selected
             RRA
             RRA
             AND    $1f
             LD     B,A
             LD     A,(CURSEC)
             CP     B
             JR     NZ,NEWTRANSDIFF
             LD     HL,(DSKTRK)
             LD     DE,(CURTRK)
             AND    A                  ; Clear carry flag
             SBC    HL,DE
             JR     NZ,NEWTRANSDIFF
             LD     A,(DSKSEL)
             LD     B,A
             LD     A,(CURSEL)
             CP     B
             RET    Z                  ; All the same, no need to save/load

NEWTRANSDIFF:LD     A,(DIRTYBUFFER)    ; Is the buffer dirty?
             CP     $00
             CALL   NZ,WRITESECT       ;  Yes - write it out
             RET    C                  ; Error out if write unsuccessful

             LD     A,C                ; Was a read requested??
             CP     $01
             JR     Z,NEWTANSDONE      ;  No - just return

             CALL   CALCLBAWLD         ; Calculate LBA
             LD     A,MMCREADBLK       ; Send cmd to read
             CALL   MMCOUTADDR
             RET    C

             LD     A,$00              ; Empty byte
             CALL   MMCOUTBYTE

             LD     A,MMCDATAPKT       ; Send data packet start
             CALL   MMCOUTBYTE

             LD     HL,RDBUFFER        ; Write the buffer out
             LD     C,$03
             LD     B,$00
NEWTRANSRD:  CALL   MMCOUTBYTE
             LD     (HL),A
             INC    HL
             DJNZ   NEWTRANSRD
             DEC    C
             JR     NZ,NEWTRANSRD

             LD     A,$ff              ; Read CRC
             CALL   MMCOUTBYTE
             LD     A,$ff              ; Read CRC
             CALL   MMCOUTBYTE

             LD     (LBA0),HL          ; Save the current position information
             LD     (LBA1),DE
             LD     A,(DSKSEL)
             LD     (CURSEL),A
             LD     HL,(DSKTRK)
             LD     (CURTRK),HL
             LD     A,(DSKSEC)
             RRA
             RRA
             AND    $1f
             LD     (CURSEC),A

NEWTANSDONE: OR     A                  ; Clear carry/error flag
             RET

;------------------------------------------------------------------------------
; MMC HLP - Write the current sector to the disk
;------------------------------------------------------------------------------
WRITESECT:   LD     A,MMCWRITEBLK
             LD     HL,(LBA0)
             LD     DE,(LBA1)
             CALL   MMCOUTADDR
             RET    C

             LD     A,$00                ; Wait bytes
             CALL   MMCOUTBYTE

             LD     HL,RDBUFFER          ; Write data out
             LD     C,$03
             LD     B,$00
WRITESECTR:  LD     A,(HL)
             CALL   MMCOUTBYTE
             INC    HL
             DJNZ   WRITESECTR
             DEC    C
             JR     NZ,WRITESECTR

             LD     A,$00                ; Phony CRC
             CALL   MMCOUTBYTE
             LD     A,$00
             CALL   MMCOUTBYTE

WRITESECWT:  LD     A,$ff                       ; Wait for the response byte
             CALL   MMCOUTBYTE
             CP     $ff
             JR     Z,WRITESECWT

             AND    $1f                  ; Check return code
             CP     $05
             SCF
             RET    NZ                   ; Unexpected value - return error

             LD     A,$00                ; Mark buffer as clean
             LD     (DIRTYBUFFER),A
             OR     A                    ; Clear carry/error flag
             RET

;------------------------------------------------------------------------------
; MMC HLP - Write a address cmd to MMC (A=CMD index), Address in DEHL
;           Wait for response code
;           Returns:
;               F[Carry] = set if error returned
;------------------------------------------------------------------------------
MMCOUTADDR:  CALL   MMCOUTBYTE
             LD     A,D
             CALL   MMCOUTBYTE
             LD     A,E
             CALL   MMCOUTBYTE
             LD     A,H
             CALL   MMCOUTBYTE
             LD     A,L
             CALL   MMCOUTBYTE

MOAWAIT:     LD     A,$ff
             CALL   MMCOUTBYTE
             BIT    7,A
             JR     NZ,MOAWAIT
             AND    $7c
             JR     NZ,MOAERROR
             OR     A                    ; Clear carry/error flaog
             RET

MOAERROR:    SCF
             RET

;------------------------------------------------------------------------------
; MMC HLP - Output a byte to the MMC and return the byte found
;           Returns:
;               A=Byte
;------------------------------------------------------------------------------
MMCOUTBYTE:  OUT    (MMCSEL),A           ; Select the MMC
             OUT    (MMCWRITE),A         ; Stage the byte in the shift reg
             OUT    (MMCCLK),A           ; Clock out the data
                                         ; TODO(jguzik) - may need to change
             OUT    (MMCCLK),A
             OUT    (MMCCLK),A
             OUT    (MMCCLK),A
             OUT    (MMCCLK),A
             OUT    (MMCCLK),A
             OUT    (MMCCLK),A
             OUT    (MMCCLK),A
             OUT    (MMCDESEL),A
             IN     A,(MMCREAD)
             RET

;------------------------------------------------------------------------------
; BIOS HLP - Translate DSK[D]/TRK[BC]/SECT[HL] to DEHL
;------------------------------------------------------------------------------
; LBA - 3333333322222222 11111111 00000000
;       7654321076543210 76543210 76543210
;       11100000000000DD DDTTTTTT TTTSSSSS
;                     32 10876543 21043210 - hst
;                     32 10876543 210edcba - sek
;  DDD = hstdsk = sekdsk = set by SELDSK: BIOS 9
;  TTT = hsttrk = sektrk = set by SETTRK: BIOS 10
;  SSS = hstsek = sekhst = seksec >> 2 - set by SETSEC: BIOS 11
CALCLBAWLD:  LD     A,(DSKSEL)
             LD     D,A
             LD     BC,(DSKTRK)
             LD     HL,(DSKSEC)
CALCLBA:     LD     A,H                ; SEC[d..9] into L[0..4]
             RRA
             RRA
             AND    $1f
             LD     L,A

             LD     A,C                ; TRK[0..2] into L[5..7]
             RLA
             RLA
             RLA
             RLA
             RLA
             AND    $e0
             OR     L                  ; Add in SEC
             LD     L,A                ; L is now complete

             LD     A,C                ; TRK[7..3] into H[0..4]
             RLA
             RLA
             RLA
             AND    $1f
             LD     H,A

             BIT    0,C                ; TRK[8] into H[5]
             JR     Z,TRK8NS
             SET    5,H

TRK8NS:      LD     H,A
             LD     A,D                ; DSK[0..1] into H[6..7]
             RLA
             RLA
             RLA
             RLA
             RLA
             RLA
             AND    $0c
             OR     H                  ; Add in DSK
             LD     H,A                ; H is now complete

             LD     A,D                ; DSK[2..4] of disk into E[0..1]
             RRA
             RRA
             AND    $03
             LD     E,A
             LD     D,$e0
             RET

;------------------------------------------------------------------------------
; BIOS 15 - Return status of current printer device.
;           Returns A=0 (not ready)
;                   A=0FFh (ready).
;------------------------------------------------------------------------------
LISTST:      LD     A,$00
             RET

;------------------------------------------------------------------------------
; BIOS 16 - Translate sector numbers to take account of skewing.
;           On entry
;              BC=logical sector number (zero based)
;              DE=address of translation table
;           Returns
;              HL=physical sector number
;           On a system with hardware skewing,
;              this would normally ignore DE and return either BC or BC+1.
;------------------------------------------------------------------------------
SECTRAN:     LD     H,B
             LD     L,C
             RET

;------------------------------------------------------------------------------
; MON - Display Help command
;------------------------------------------------------------------------------
HELP:        LD     DE,HLPTXT        ; Print Help message
             JP     C_WRITESTR

;------------------------------------------------------------------------------
; MON - GOTO command
;------------------------------------------------------------------------------
GOTO:        CALL   GETDE              ; ENTRY POINT FOR <G>oto addr. Get XXXX from user.
             RET    C                  ; Return if invalid
             EX     DE,HL              ; "jump" to address
             JP     (HL)

;------------------------------------------------------------------------------
; MON - Dump memory between two address locations
;------------------------------------------------------------------------------
DMPMEM:      CALL   GETRANGE
             RET    C
DMPMEMSLN:   LD     B,$10              ; Number of chars per line
             CALL   WRCRLF
             CALL   DEOUT              ; Emit current address
             PUSH   DE                 ; Save address for ASCII print
DMPMEMNXT:   LD     A,' '
             CALL   CNTDOUT
             CALL   DMPLOC
             INC    DE
             DEC    HL
             LD     A,H
             OR     L
             JR     Z,DMPMEMEND
DMPMEMTST:   DJNZ   DMPMEMNXT
             LD     B,$10              ; Emit Ascii for full line
             POP    DE
             CALL   DMPASCII
             JR     DMPMEMSLN

DMPMEMEND:   DEC    B                  ; Count the last char printed
             LD     H,B                ; Save B for print
             JR     Z,DMPMEMALST
             LD     A, ' '             ; Space fill the last chars
DMPMEMFLL:   CALL   CNTDOUT
             CALL   CNTDOUT
             CALL   CNTDOUT
             DJNZ   DMPMEMFLL

DMPMEMALST:  POP    DE                 ; Restore begin of line
             LD     A,$10              ; Calc number of ASCII to print for last line
             SUB    H
             LD     B,A

DMPASCII:    LD     A,' '
             CALL   CNTDOUT
             CALL   CNTDOUT
DMPASCIINXT: LD     A,(DE)
             LD     C,'?'
             CP     $20
             JP     M,DMPASCIINP
             CP     $7F
             JP     C,DMPASCIIP
DMPASCIINP:  LD     A,'?'
DMPASCIIP:   CALL   CNTDOUT
             INC    DE
             DJNZ   DMPASCIINXT
             RET

;------------------------------------------------------------------------------
; MON - Edit memory from a starting address until canceled
;------------------------------------------------------------------------------
EDMEM:       CALL   GETDE
             RET    C
EDMEMNXT:    CALL   WRCRLF
             CALL   DEOUT
             LD     A,' '
             CALL   CNTDOUT
             CALL   DMPLOC
             LD     A,' '
             CALL   CNTDOUT
             CALL   GETHEX
             RET    C
             LD     (DE),A
             LD     A,' '
             CALL   CNTDOUT
             CALL   DMPLOC
             INC    DE
             JR     EDMEMNXT

;------------------------------------------------------------------------------
; MON - Fill Memory with Zeros Start(DE) Length(HL)
;------------------------------------------------------------------------------
FILLMEM:     CALL   GETRANGE
             RET    C                  ; Return if invalid
FILLMEMNXT:  XOR    A
             LD     (DE),A
             INC    DE
             DEC    HL
             LD     A,H
             OR     L
             JR     NZ,FILLMEMNXT
             RET

;------------------------------------------------------------------------------
; MON - Load CPM Program
;------------------------------------------------------------------------------
LOADTPA:     LD     DE,TPABASE       ; Space mem
             LD     HL,$ef00
             CALL   FILLMEMNXT
             LD     DE,TPABASE
             JR     XMODEMGO

;------------------------------------------------------------------------------
; MON - Execute TPA Progam
;------------------------------------------------------------------------------
EXECTPA:     CALL   WRCRLF           ; Start on blank line
             LD     SP,TPASTACK      ; Establish 8 byte TPA stack
             LD     DE,WBOOT
             PUSH   DE               ;  with CCP on stack
             LD     HL,TPABASE       ; Execute TPA
             JP     (HL)

;------------------------------------------------------------------------------
; MON - Replace the Monitor
;------------------------------------------------------------------------------
REPLACE:     CALL   LOADTPA
             RET    C
             LD     DE,$B0ED    ; LDIR
             LD     ($0042),DE  ;    Free area
             LD     A,$C7       ; RST 0
             LD     ($0044),A
             LD     BC,$1000    ; Load Copy
             LD     DE,$F000
             LD     HL,$0100
             JP     $0042       ; Go Copy

;------------------------------------------------------------------------------
; MON - XMODEM a File
;------------------------------------------------------------------------------
XMODEM:      CALL   GETDE
             RET    C

XMODEMGO:    LD     (TMP),DE           ; Save starting address
             LD     DE,XMODEMTXT
             CALL   C_WRITESTR

XMODEMNAK:   LD     C,NAK
XMODEMRSP:   CALL   CONOUT
             LD     B,$00              ; Wait counters
             LD     C,$00
             LD     D,$04

XMODEMWAIT:  LD     A,(SERBUFUSED)
             CP     $00
             JR     NZ,XMODEMRDY
             DJNZ   XMODEMWAIT
             DEC    C
             JR     NZ,XMODEMWAIT
             DEC    D
             JR     NZ,XMODEMWAIT
             JR     XMODEMNAK

XMODEMRDY:   CALL   CONIN
             CP     SOH
             JR     NZ,XMODEMNONBLK

XMODEMBLOCK: CALL   CONIN              ; Get block number and calc place in memory
             LD     B,A                ; Load block into BC
             LD     C,0
             DEC    B                  ; Block is 1 based, convert to 0 based
             SRL    B                  ; Block is 128 bytes, so shift up a bit
             RR     C
             LD     HL,(TMP)           ;  Add the block offset to original address
             ADD    HL,BC
             CALL   CONIN              ; Ignore the inverse block number

             LD     B,128              ; Bytes in a packet
             LD     C,0                ; Checksum
XMODEMREAD:  CALL   CONIN              ; Read byte and load into correct address
             LD     (HL),A
             INC    HL                 ; Next address
             ADD    A,C                ; Calc checksum
             LD     C,A
             DJNZ   XMODEMREAD         ; Continue reading until end of block

XMODEMCKSM:  CALL   CONIN              ; Read checksum value
             CP     C                  ; Compare to checksum
             JR     NZ,XMODEMNAK       ; Wrong value so goto NAK
             LD     C,ACK              ; Load ACK
             JR     XMODEMRSP          ; and emit ti

XMODEMNONBLK:CP     EOT                ; Not the start of block, maybe the end???
             JR     NZ,XMODEMNOTEOT    ;  - No, so check other things (e.g. ctrl-C)
             LD     C,ACK              ; ACK the EOT
             CALL   CONOUT
             RET

XMODEMNOTEOT:CP     CTRLC              ; Not EOT, give the used a chance to BRK
             RET    Z                  ; Yup a break, so exit
             JR     XMODEMNAK          ;  - Not a break, nor a EOT so NAK it

;------------------------------------------------------------------------------
; Emit BEEP
;------------------------------------------------------------------------------
BEEP:        PUSH   BC
             LD     B,0x00
             LD     C,B
BEEPNXT:     DEC    C
             JR     NZ,BEEPNXT
             OUT    (SPKRIO),A
             DEC    B
             JR     NZ,BEEPNXT
             POP    BC
             RET

;------------------------------------------------------------------------------
; Filtered Character I/O - Write char in A
;------------------------------------------------------------------------------
WRCHR:       CP     CR
             JR     Z,WRCRLF          ; When CR, write CRLF
             CP     ' '               ; Don't write out any other control codes
             RET    C                 ;   ie. < space
             CALL   CNTDOUT
             RET

;------------------------------------------------------------------------------
; Print CR/LF
;------------------------------------------------------------------------------
WRCRLF:      LD     A,CR
             CALL   CNTDOUT
             LD     A,LF
             CALL   CNTDOUT
             RET

;------------------------------------------------------------------------------
; Get byte from console as hex into A; ctrl-c & ESC returns with carry flag set
;------------------------------------------------------------------------------
GETHEX:      CALL   GETNIBBLE
             RET    C
             RLCA
             RLCA
             RLCA
             RLCA
             PUSH   BC
             LD     B,A
GETHEXRD2:   CALL   GETNIBBLE
             JR     C,GETHEXRET
             OR     B
             AND    A
GETHEXRET:   POP    BC
             RET

;------------------------------------------------------------------------------
; Get nibble from console as hex into the lower half of A;
;    ctrl-c & ESC returns with the carry flag set
;------------------------------------------------------------------------------
GETNIBBLEINV:LD     C,BEL
             CALL   CONOUT
             POP    BC
GETNIBBLE:   CALL   CONIN              ; Entry point
             CP     CTRLC              ; Is it Ctrl-C
             JR     Z,GETNIBBLECAN
             CP     ESC                ; Or ESC
             JR     Z,GETNIBBLECAN
             PUSH   BC
             LD     C,A                ; Save the char
             CALL   ASCHEX
             JR     C,GETNIBBLEINV
             LD     B,A
             LD     A,C
             CALL   CNTDOUT
             LD     A,B
             POP    BC
             RET
GETNIBBLECAN:SCF
             RET

;------------------------------------------------------------------------------
; Gets the last four hex chars as a number from the console into DE
;------------------------------------------------------------------------------
GETDE:       LD     DE,$0000          ; Start with $0000
GETDENXT:    CALL   CONIN
             SCF                       ; Assume cancel for now
             CP     CTRLC              ; Is it Ctrl-C
             JR     Z,GETDECAN
             CP     ESC                ; Or ESC
             JR     Z,GETDECAN
             CP     CR
             JR     Z,GETDEDN
             CP     LF
             JR     Z,GETDEDN
             CP     ' '
             JR     Z,GETDEDN
             CP     ','
             JR     Z,GETDEDN
             PUSH   BC
             LD     C,A
             CALL   ASCHEX
             JR     C,GETDEILL
             EX     DE,HL
             ADD    HL,HL
             ADD    HL,HL
             ADD    HL,HL
             ADD    HL,HL
             EX     DE,HL
             OR     E
             LD     E,A
             LD     A,C
             POP    BC
             CALL   CNTDOUT
             JR     GETDENXT
GETDEILL:    LD     C,BEL
             CALL   CONOUT
             POP    BC
             JR     GETDENXT
GETDEDN:     CALL   CNTDOUT
             RET
GETDECAN:    SCF
             RET

;------------------------------------------------------------------------------
; Convert a ASCII bit to a HEX nibble, sets Carry Flag on illegal HEX char
;------------------------------------------------------------------------------
ASCHEX:      SUB    $30                ; Shift ASCII '0' to $00
             RET    C                  ;  Char < '0'
             CP     $0A                ; Check vs upper range of decimal digit
             CCF                       ;  Return if decimal digit
             RET    NC
             OR     $20                ; Make lower case - already down $30
             SUB    $31                ; Shift 'a' to $00
             RET    C                  ;  between '9' and 'a' - illegal
             CP     $06                ; Check > 'f'
             CCF
             RET    C
             ADD    A,$0A              ; Valid 'a'-'f', add the offset back
             RET                       ;  Add will not genertate carry

;------------------------------------------------------------------------------
; Get a range with DE has start, HL has legnth, Carry flag set on cancel
;------------------------------------------------------------------------------
GETRANGE:    CALL   GETDE     ; Starting address
             RET    C
             EX     DE,HL     ; HL has starting
             CALL   GETDE     ; Ending Address
             RET    C
             EX     DE,HL     ; HL - end , DE - start
             SBC    HL,DE     ; Calculate length; Carry flag is clear
             INC    HL        ; Count 1 more fence post
             AND    A         ; Clear carry flag
             RET

;------------------------------------------------------------------------------
; Print a byte at HL to console
;------------------------------------------------------------------------------
DMPLOC:      LD     A,(DE)
             JP     HEXOUT

;------------------------------------------------------------------------------
; Print D followed by E
;------------------------------------------------------------------------------
DEOUT:       LD     A,D
             CALL   HEXOUT
             LD     A,E
             CALL   HEXOUT
             RET

;------------------------------------------------------------------------------
; Output byte to console as hex
;------------------------------------------------------------------------------
HEXOUT:      PUSH   AF
             PUSH   BC
             LD     B,A
             RRCA
             RRCA
             RRCA
             RRCA
             AND    $0f
             CALL   HEXASC
             CALL   CNTDOUT
             LD     A,B
             AND    $0f
             CALL   HEXASC
             CALL   CNTDOUT
             POP    BC
             POP    AF
             RET

;------------------------------------------------------------------------------
; Convert nybble to ASCII char
;------------------------------------------------------------------------------
HEXASC:      ADD    A,$90
             DAA
             ADC    A,$40
             DAA
             RET

;------------------------------------------------------------------------------
; Serial interrupt handlers
; Same interrupt called if either of the inputs receives a character
; so need to check the status of each SIO input.
;------------------------------------------------------------------------------
SERIALINT:   PUSH   AF
             PUSH   HL

             LD     HL,(SERINPTR)
             LD     A,L
             INC    A
             OR     $80
             LD     L,A
             LD     (SERINPTR),HL
             IN     A,(SIOB_D)
             LD     (HL),A
             LD     A,(SERBUFUSED)
             INC    A
             LD     (SERBUFUSED),A
             CP     SER_FULLSIZE
             JR     C,RTS0
             LD     A,$05
             OUT    (SIOB_C),A
             LD     A,RTS_HIGH
             OUT    (SIOB_C),A
RTS0:        POP    HL
             POP    AF
             EI
             RETI

;-----------------------------------------------------------------------------
; Tests a char in A if it is a ctrl char, following flags are set
;   Z - LF/CR/TAB/BS
;   C - ctrl char
;-----------------------------------------------------------------------------
CHKCTRL:     CP    TAB
             RET   Z
             CP    CR
             RET   Z
             CP    LF
             RET   Z
             CP    BS
             RET   Z
             CP    ' '
             RET

;-----------------------------------------------------------------------------
; Outputs the char in A using counting the chars
;-----------------------------------------------------------------------------
CNTDOUT:     PUSH  BC                  ; Save working registers
             PUSH  HL
             LD    C,A
             CALL  CONOUT
             CP    DEL
             JR    Z,CNTDOUTRET        ; rubouts don't count vs CURPOS
             LD    HL,CURPOS

             CP    ' '                 ; Ctrl char?
             JR    C,CNTDDUTCTRL
             INC   (HL)                ; No so just count it
             JR    CNTDOUTRET

CNTDDUTCTRL: LD    A,(HL)              ; Ignore ctrl chars at begin of line
             OR    A
             LD    A,C
             JR    Z,CNTDOUTRET

             CP    LF                  ; LF?
             JR    NZ,CNTDDUTNOTLF     ;   no - check bs
             LD    (HL),$00            ;   yes - reset postition
             JR    CNTDOUTRET

CNTDDUTNOTLF:CP    BS                  ; Back space
             JR    NZ,CNTDOUTRET       ;  All other ctrl chars don't count

             DEC   (HL)                ; BS the char
             LD    C,' '               ; Remove the char
             CALL  CONOUT
             LD    C,BS
             CALL  CONOUT

CNTDOUTRET:  POP   HL                  ; Restore working registers
             POP   BC
             RET

;-----------------------------------------------------------------------------
; CPM22 - CP/M V2.2 BIOS
; Params
;  C     - function number
;  DE    - 16 bit parameter
;  E     - 8 bit parameter
; Return;  HL/BA - return values (clones)
;-----------------------------------------------------------------------------
CPM22:       LD     (TPASTACKSAV),SP   ; Establish new stack (old one maybe to small)
             LD     SP,STACK

             LD     A,C                ; Prelim check; ensure valid func number
             CP     NFUNCTS
             JR     NC,CPM2NOFUNC      ; Return right away

             LD     HL,CPM2DONE        ; Going to call JP (HL), push a valid RET address
             PUSH   HL

             LD     HL,CPMFUNCS        ; Load function address
             LD     B,0
             ADD    HL,BC
             ADD    HL,BC
             LD     B,(HL)
             INC    HL
             LD     H,(HL)
             LD     L,B

             LD     A,E                ; Lots of functions need E in A
             JP     (HL)               ; Really CALL (HL) as CPM2RET is now on the stack

CPM2NOFUNC:  XOR    A
             LD     H,A

CPM2DONE:    LD     L,A                ; Pre 1.4 version used AB
             LD     B,H
             LD     SP,(TPASTACKSAV)   ; Restore original stack
             RET

CPM_NOTIMP:  RET

;-----------------------------------------------------------------------------
; Wait for a character from the keyboard; then echo it to the screen and return it.
; Returns A=L=character.
;-----------------------------------------------------------------------------
C_READ:      CALL   CONIN
             CP     CR                 ; Don;t Echo CR
             RET    Z
             CALL   CHKCTRL            ; Is the char a non-display char
             RET    C                  ;   Yes - don't display it

             ; Fall thru to C_WRITE

;-----------------------------------------------------------------------------
; Send the character in A to the screen.
; Tabs are expanded to spaces.
; Output can be paused with ^S and restarted with ^Q (or any key under versions prior to CP/M 3).
; While the output is paused, the program can be terminated with ^C.
;-----------------------------------------------------------------------------
C_WRITE:     CP     TAB                ; Is the char a tab?
             JP     NZ,CNTDOUT         ;  no -just print it
             LD     H,A                ; Save the char - in case someone wants it
C_WRITETAB:  LD     A,' '              ; Expand tabs with spaces
             CALL   CNTDOUT
             LD     A,(CURPOS)         ;  until curpos is multiple of 8
             AND    $07
             JR     NZ,C_WRITETAB
             LD     A,H
             RET

;-----------------------------------------------------------------------------
; Wait for a character from the Aux device; then echo it to the screen and return it.
; Returns A=character.
;-----------------------------------------------------------------------------
A_READ:      JP     A_READ             ; Hang as this will never send a char

;-----------------------------------------------------------------------------
; Auxiliary (Punch) output
;-----------------------------------------------------------------------------
A_WRITE:     RET                       ; NOOP

;-----------------------------------------------------------------------------
; Printer output
;-----------------------------------------------------------------------------
L_WRITE:     RET                       ; NOOP

;-----------------------------------------------------------------------------
; Raw IO. A=E=$FF non-blocking read, A=E=$FE chars avail, otherwise print the char
;-----------------------------------------------------------------------------
C_RAWIO:     INC    E                   ; Check for input request ($ff)
             JP     Z,C_RAWIOINP
             INC    E                   ; Check for status request ($fe)
             JP     Z,C_RAWIOSTS
             LD     C,A
             CALL   CONOUT              ; Just emit the char
             RET
C_RAWIOSTS:  LD     A,(SERBUFUSED)      ; Status request - return 0 if nothing avail
             RET
C_RAWIOINP:  LD     A,(SERBUFUSED)      ; Input request - return 0 if nothing avail
             OR     A
             RET    Z                   ; nothing avail
             CALL   CONIN               ; return the avail char
             RET

;-----------------------------------------------------------------------------
; Get current I/O byte value
;-----------------------------------------------------------------------------
S_GETIOB:    LD     A,(IOBYTE)
             RET

;-----------------------------------------------------------------------------
; Set current I/O byte value
;-----------------------------------------------------------------------------
S_SETIOB:    LD     (IOBYTE),A
             RET

;-----------------------------------------------------------------------------
; Display a string of ASCII characters, terminated with the $ character
;-----------------------------------------------------------------------------
C_WRITESTR:  PUSH   AF                 ; Save working registers
             PUSH   DE
C_WRITESTRWR:LD     A,(DE)             ; Get character
             CP     '$'                ; Is it $?
             JR     Z,C_WRITESTREX     ; Then Return on terminator
             CALL   C_WRITE            ; Print it
             INC    DE                 ; Next Character
             JR     C_WRITESTRWR       ; Continue until $
C_WRITESTREX:POP    DE                 ; Restore working registers
             POP    AF
             RET

;-----------------------------------------------------------------------------
; Read characters from the keyboard into a memory buffer until RETURN is pressed
; DE points to a buffer:
;     buffer:   .DB   size
;               .DB   chars_read  (set on return)
;               .DB   * size
;-----------------------------------------------------------------------------
C_READSTR:   PUSH  AF                  ; Save working registers
             PUSH  BC                  ; NOTE: DE is pushed later with an offset of one
             LD    A,(CURPOS)          ; Save curpos for BS of first TAB in line
             LD    (CURSTART),A
             LD    A,(DE)              ; Get buffer size
             LD    C,A
             INC   DE                  ; Point to return space
             LD    (TMP),DE            ;   Save chars_read ptr
             LD    B,$00               ; Count of chars
C_READSTRIN: CALL  CONIN               ; Get a char
             AND   $7f                 ; Force into ASCII range
             CP    CR                  ; End of a line via CR or LF???
             JR    Z,C_READSTREOL
             CP    LF
             JR    Z,C_READSTREOL
             CP    BS                  ; BS?
             JR    Z,C_READSTRBS
             CP    DEL                 ; Delete Key?
             JR    Z,C_READSTRIN       ;   TODO(jguzik) handle this - ignore for now
             CP    CTRLE               ; Ctrl-E
             JR    Z,C_READSTRIN       ;   TODO(jguzik) handle this - ignore for now
             CP    CTRLPRT             ; Ctrl-P (toggle printer)
             JR    Z,C_READSTRIN       ;   TODO(jguzik) handle this - ignore for now
             CP    CTRLR               ; Ctrl-R (Cancel Line)
             JR    Z,C_READSTRIN       ;   TODO(jguzik) handle this - ignore for now
             CP    NAK                 ; Ctrl-U (Erase line)
             JR    Z,C_READSTRERS      ;
             CP    CAN                 ; Ctrl-X (Cancel)
             JR    Z,C_READSTRCAN      ;

             INC   DE                  ; Load the char in the buffer
             LD    (DE),A

             CP    TAB                 ; TAB
             JR    Z,C_READSTRTAB
             CP    ' '                 ; Other CNTRL char
             JR    C,C_READSTRCTL      ;   Yes - print it with caret (^)
             CALL  CNTDOUT             ; Write the char out normally

C_READSTRCNT:INC   B                   ; Adjust the buffer
             DEC   C                   ; If not then end of the buffer
             JR    NZ,C_READSTRIN      ;   get more chars

; TODO(jguzik) handle CR/LF as next chars (buffer is full and the CR/LF should be ignored)

C_READSTREOL:LD    DE,(TMP)            ; Load return length
             LD    A,B
             LD    (DE),A
             LD    A,CR                ; and finish with CR
             CALL  CNTDOUT
             DEC   DE                  ; Restore working registers
             POP   BC
             POP   AF
             RET

C_READSTRTAB:LD    H,A                 ; Use C_WRITE to emit TAB
             CALL  C_WRITETAB
             JR    C_READSTRCNT

C_READSTRCTL:LD    H,A                 ; Save the char
             LD    A,'^'               ; Emit ^
             CALL  CNTDOUT
             LD    A,H
             OR    $40                 ; Now display as alpha equiv
             CALL  CNTDOUT
             LD    A,H
             CP    CTRLC               ; Is it CTRL-C (BRK)
             JR    NZ,C_READSTRCNT

C_READSTRCTC:LD    A,B                 ; Ctrl-c at begin of line
             OR    A
             JR    NZ,C_READSTRCNT     ;   no - continue accepting
             RST   0                   ;   yes - reboot

C_READSTRBS: LD    A,B
             OR    A                   ; ignore BS at begin of line
             CALL  NZ,C_READSTRBCK
             JR    C_READSTRIN

C_READSTRERS:CALL  C_READSTRDER
             JR    C_READSTRIN

C_READSTRCAN:CALL  C_READSTRDER        ; Erase line and cancel
             JR    C_READSTREOL

C_READSTRDER:LD    A,B                 ; Remove all chars to begin of line
             OR    A
             RET   Z
             CALL  C_READSTRBCK
             JR    C_READSTRDER

C_READSTRBCK:LD    A,(DE)              ; Need to check the char for ctrl or tab
             DEC   DE                  ; Backup buffer one char
             INC   C
             DEC   B
             CP    TAB                 ; Tab?
             JR    Z,C_READSTRETB      ;   BS to correct pos
             CP    ' '                 ; Ctrl char?
             JR    NC,C_READSTRBON     ;   No  - Just one BS
             LD    A,BS                ;   Yes - removed the ^ too
             CALL  CNTDOUT
C_READSTRBON:LD    A,BS
             JP    CNTDOUT

C_READSTRETB:PUSH  BC                  ; Save working registers
             PUSH  DE
             LD    A,(CURSTART)        ; Calc pos the cursor should be in
             LD    C,A

             LD    A,B                 ; Already at begining?
             OR    A
             JR    Z,C_RDSTRTBCLC      ;   Yes - Nothing in buffer to count
             LD    DE,(TMP)            ; Get the start of the buffer
C_RDSTRETBNX:INC   DE                  ; Next char
             INC   C                   ; Count it (and maybe more)
             LD    A,(DE)
             CP    TAB                 ; Tab?
             JR    NZ,C_RDSTRETBCT     ;   No  - Just count it once
             LD    A,C                 ;   Yes - Round to next TS
             ADD   A,$08
             AND   $f8
             LD    C,A
C_RDSTRETBCT:DEC   B
             JR    NZ,C_RDSTRETBNX
             LD    B,C

             ; B Now contains where we should be at

C_RDSTRTBCLC:LD    A,(CURPOS)
             CP    B
             JP    Z,C_RDSTRETBDN
             LD    A,BS
             CALL  CNTDOUT
             JR    C_RDSTRTBCLC

C_RDSTRETBDN:POP   DE                  ; Restore working registers
             POP   BC
             RET

;-----------------------------------------------------------------------------
; Console status
; Returns A=0 if no characters are waiting, nonzero if a character is waiting.
;-----------------------------------------------------------------------------
C_STAT:      LD    A,(SERBUFUSED)
             RET

;-----------------------------------------------------------------------------
; Get version number
; Entered with C=0Ch. Returns B=H=system type, A=L=version number.
;-----------------------------------------------------------------------------
S_BDOSVER:   LD     A,$22
             LD     H,$00
             RET

DRV_ALLRESET:JP     CPM_NOTIMP
DRV_SET:     JP     CPM_NOTIMP
F_OPEN:      JP     CPM_NOTIMP
F_CLOSE:     JP     CPM_NOTIMP
F_SFIRST:    JP     CPM_NOTIMP
F_SNEXT:     JP     CPM_NOTIMP
F_DELETE:    JP     CPM_NOTIMP
F_READ:      JP     CPM_NOTIMP
F_WRITE:     JP     CPM_NOTIMP
F_MAKE:      JP     CPM_NOTIMP
F_RENAME:    JP     CPM_NOTIMP

;-----------------------------------------------------------------------------
; Return bitmap of logged-in drives
; AH = bits for drives H[7] = P: and L[0] = A:
;-----------------------------------------------------------------------------
DRV_LOGINVEC:LD     A,$00              ; No drives
             LD     H,$00
             RET

DRV_GET:     JP     CPM_NOTIMP
F_DMAOFF:    JP     CPM_NOTIMP
DRV_ALLOCVEC:JP     CPM_NOTIMP
DRV_SETRO:   JP     CPM_NOTIMP
DRV_ROVEC:   JP     CPM_NOTIMP
F_ATTRIB:    JP     CPM_NOTIMP
DRV_DPB:     JP     CPM_NOTIMP
F_USERNUM:   JP     CPM_NOTIMP
F_READRAND:  JP     CPM_NOTIMP
F_WRITERAND: JP     CPM_NOTIMP
F_SIZE:      JP     CPM_NOTIMP
F_RANDREC:   JP     CPM_NOTIMP
DRV_RESET:   JP     CPM_NOTIMP
DRV_ACCESS:  JP     CPM_NOTIMP
DRV_FREE:    JP     CPM_NOTIMP
F_WRITEZF:   JP     CPM_NOTIMP

;-----------------------------------------------------------------------------
; Function listed here take
;
;   DE      - 16 bit parameter
;   E       -  8 bit parameter
;
; Returns
;
;   A       -  8 bit return
;   AH      - 16 bit return  (NOTE not HL)
;-----------------------------------------------------------------------------
NFUNCTS      .EQU   41
CPMFUNCS:    .DW    P_TERMCPM    ;$00/00
             .DW    C_READ       ;$01/01
             .DW    C_WRITE      ;$02/02
             .DW    A_READ       ;$03/03
             .DW    A_WRITE      ;$04/04
             .DW    L_WRITE      ;$05/05
             .DW    C_RAWIO      ;$06/06
             .DW    S_GETIOB     ;$07/07
             .DW    S_SETIOB     ;$08/08
             .DW    C_WRITESTR   ;$09/09
             .DW    C_READSTR    ;$0A/10
             .DW    C_STAT       ;$0B/11
             .DW    S_BDOSVER    ;$0C/12
             .DW    DRV_ALLRESET ;$0D/13
             .DW    DRV_SET      ;$0E/14
             .DW    F_OPEN       ;$0F/15
             .DW    F_CLOSE      ;$10/16
             .DW    F_SFIRST     ;$11/17
             .DW    F_SNEXT      ;$12/18
             .DW    F_DELETE     ;$13/19
             .DW    F_READ       ;$14/20
             .DW    F_WRITE      ;$15/21
             .DW    F_MAKE       ;$16/22
             .DW    F_RENAME     ;$17/23
             .DW    DRV_LOGINVEC ;$18/24
             .DW    DRV_GET      ;$19/25
             .DW    F_DMAOFF     ;$1A/26
             .DW    DRV_ALLOCVEC ;$1B/27
             .DW    DRV_SETRO    ;$1C/28
             .DW    DRV_ROVEC    ;$1D/29
             .DW    F_ATTRIB     ;$1E/30
             .DW    DRV_DPB      ;$1F/31
             .DW    F_USERNUM    ;$20/32
             .DW    F_READRAND   ;$21/33
             .DW    F_WRITERAND  ;$22/34
             .DW    F_SIZE       ;$23/35
             .DW    F_RANDREC    ;$24/36
             .DW    DRV_RESET    ;$25/37
             .DW    DRV_ACCESS   ;$26/38
             .DW    DRV_FREE     ;$27/39
             .DW    F_WRITEZF    ;$28/40

;-----------------------------------------------------------------------------
; WR0
; 0-2  Register Index (0-5 for write, 0-2 for read) (SIO: 0-7 for write)
; 3-5  Command (0..7)
;       0=No action
;       1=DART: Reserved / SIO: Send Abort (SDLC)
;       2=Reset Ext/Status Interrupts
;       3=Reset Channel
;       4=Enable Interrupt on next Rx character
;       5=Reset Tx Int pending
;       6=Reset Error
;       7=Return from Int (Ch-A only)
; 6-7  CRC Reset Code (DART: Reserved, should be 0) (SIO: 0..3)
;       0=No action
;       1=DART: Reserved, SIO: Reset Receive CRC Checker
;       2=DART: Reserved, SIO: Reset Transmit CRC Generator
;       3=DART: Reserved, SIO: Reset Tx Underrun/End of Message latch
;

;-----------------------------------------------------------------------------
; WR1
; 0   External Interrupts Enable
; 1   Tx Interrupt Enable
; 2   Status Affects Vector (Channel B only) (see WR2/RR2)
; 3-4 Rx Interrupt Mode (0..3)
;      0=None
;      1=On first Rx char
;      2=On all Rx chars (Parity affects vector)
;      3=On all Rx chars (Parity does not affect vector)
; 5   Wait/Ready on Receive/Transmit
; 6   Wait/Ready Function
; 7   Wait/Ready Enable

;-----------------------------------------------------------------------------
; WR2 Interrupt Vector (Channel B only)
;
; 0-7 Interrupt Vector (Bit1-3=no effect when WR1.Bit2=1, see RR2 for details)

;-----------------------------------------------------------------------------
; WR3 - Rx Control
; 0   Rx Enable
; 1   DART: Reserved (must be 0), SIO: Sync Char Load Inhibit
; 2   DART: Reserved (must be 0), SIO: Address Search Mode
; 3   DART: Reserved (must be 0), SIO: Receiver CRC Enable
; 4   DART: Reserved (must be 0), SIO: Enter Hunt Phase
; 5   Enable automatic hardware handshaking using RTS/CTS
; 6-7 Rx data bits          (0..3 = 5bits, 7bits, 6bits, 8bits)

;-----------------------------------------------------------------------------
; WR4 RX/TX Control
; 0   Rx/Tx Parity bit      (0=None, 1=Enable)
; 1   Rx/Tx Parity type     (0=Odd, 1=Even)
; 2-3 Rx/Tx Stop bits (0..3=Reserved, 1bit, 1.5bits, 2bits) (SIO: 0=Sync Modes)
; 4-5 DART: Reserved (must be 0), SIO: Sync Mode (enabled when Bit2-3=zero)
;      0=8bit SYNC Character        (SIO only, not DART)
;      1=16bit SYNC Character       (SIO only, not DART)
;      2=SDLC Mode (0111 1110 Flag) (SIO only, not DART)
;      3=External SYNC Mode         (SIO only, not DART)
; 6-7 Rx/Tx DART clock mode (0..3=X1, X16, X32, X64)

;-----------------------------------------------------------------------------
; WR5 - Tx Control
;
; 0   DART: Reserved (must be 0), SIO: Tx CRC Enable
; 1   RTS enabled/disabled
; 2   DART: Reserved (must be 0), SIO: Rx/Tx CRC Type (0=SDLC, 1=CRC-16)
; 3   Tx Enable
; 4   Tx Send break
; 6-5 Tx data bits          (0..3 = 5bits, 7bits, 6bits, 8bits)
; 7   DTR enabled/disabled

INITSERREG:  .BYTE  0,4,1,2,3,5         ; Order of registers the data is below
INITSERSEQ:  .BYTE  $18                 ; (000)(110)(00) - Reset Channel
             .BYTE  $84                 ; (10)(00)(01)(0)(0) - clock mode x32, 8 bits,  1 stop, no parity
                                        ;    aka 57600 8N1
             .BYTE  $18                 ; (0)(0)(0)(11)(0)(0)(0) -  Interrupt on all chars
             .BYTE  SERIALINTVEC        ; INTERRUPT VECTOR ADDRESS
             .BYTE  $E1                 ; (11)(1)(0)(0)(0)(0)(1) RX 8bites, Hardware handshake, Rx Enable
             .BYTE  RTS_LOW             ; EA (1)(11)(0)(1)(0)(1)(0) - DTR enable, 8 bites, TX enable, RTS eanbled

SIGNON:      .ASCII "Z80 SBC Boot ROM 1.2h by G. Searle/J Guzik"
             .BYTE  CR,LF
             .ASCII "Type ? for options"
             .ASCII "$"

MONPROMPT:   .BYTE  CR,LF
             .ASCII "> "
             .ASCII "$"

HLPTXT:      .BYTE  CR,LF
             .ASCII "R - Reset"
             .BYTE  CR,LF
             .ASCII "D - Dump Memory"
             .BYTE  CR,LF
             .ASCII "E - Edit Memory"
             .BYTE  CR,LF
             .ASCII "F - Fill Memory"
             .BYTE  CR,LF
             .ASCII "G - Jump to Location"
             .BYTE  CR,LF
             .ASCII "L - Load CP/M TPA via XMODEM"
             .BYTE  CR,LF
             .ASCII "T - Exec CP/M TPA in memory"
             .BYTE  CR,LF
             .ASCII "M - Replace Monitor"
             .BYTE  CR,LF
             .ASCII "X - XMODEM"
             .ASCII "$"

XMODEMTXT:   .BYTE  CR,LF
             .ASCII "Waiting for File...."
             .ASCII "$"

BIOSRAMS     .EQU   $
TMP:         .DW    0

;-----------------------------------------------------------------------------
; Serial Pointers and counters
;-----------------------------------------------------------------------------
SERINPTR:    .DW    $0000
SERRDPTR:    .DW    $0000
SERBUFUSED:  .DB    $00

;-----------------------------------------------------------------------------
; Cursor positions used for cooked I/O
;-----------------------------------------------------------------------------
CURPOS:      .DB    $00
CURSTART:    .DB    $00

;-----------------------------------------------------------------------------
; Stack for the TPA a d BIOS (and save TPA)
;-----------------------------------------------------------------------------
TPASTACKSAV: .DW    0
             .DS    $12
TPASTACK     .EQU   $

             .DS    $20
STACK        .EQU   $

;-----------------------------------------------------------------------------
; Selected parameters for drive read/write
;-----------------------------------------------------------------------------
DSKSEL:      .DB    $ff
DSKTRK:      .DW    $0000
DSKSEC:      .DW    $0000
DSKDMA:      .DW    $0000

;-----------------------------------------------------------------------------
; Current parameters loaded in buffer
;-----------------------------------------------------------------------------
CURSEL:      .DB    $ff
CURTRK:      .DW    $0000
CURSEC:      .DB    $00
CURLBA0:     .DW    $0000
CURLBA1:     .DW    $0000

LBA0:        .DW    $0000
LBA1:        .DW    $0000

DIRTYBUFFER: .DB    $00

RDBUFFER:    .DS    $200

;-----------------------------------------------------------------------------
; Disk parameter headers for disk 0 to 15
;-----------------------------------------------------------------------------
DPBASE:      .DW    16 * DRVCOUNT
;            .DW    $0000,$0000,$0000,$0000,FILEBUFF,DPB0,$0000,ALV00
;            .DW    $0000,$0000,$0000,$0000,FILEBUFF,DPB,$0000,ALV01
;            .DW    $0000,$0000,$0000,$0000,FILEBUFF,DPB,$0000,ALV02
;            .DW    $0000,$0000,$0000,$0000,FILEBUFF,DPB,$0000,ALV03
;            .DW    $0000,$0000,$0000,$0000,FILEBUFF,DPB,$0000,ALV04
;            .DW    $0000,$0000,$0000,$0000,FILEBUFF,DPB,$0000,ALV05
;            .DW    $0000,$0000,$0000,$0000,FILEBUFF,DPB,$0000,ALV06
;            .DW    $0000,$0000,$0000,$0000,FILEBUFF,DPB,$0000,ALV07
;            .DW    $0000,$0000,$0000,$0000,FILEBUFF,DPB,$0000,ALV08
;            .DW    $0000,$0000,$0000,$0000,FILEBUFF,DPB,$0000,ALV09
;            .DW    $0000,$0000,$0000,$0000,FILEBUFF,DPB,$0000,ALV10
;            .DW    $0000,$0000,$0000,$0000,FILEBUFF,DPB,$0000,ALV11
;            .DW    $0000,$0000,$0000,$0000,FILEBUFF,DPB,$0000,ALV12
;            .DW    $0000,$0000,$0000,$0000,FILEBUFF,DPB,$0000,ALV13
;            .DW    $0000,$0000,$0000,$0000,FILEBUFF,DPB,$0000,ALV14
;            .DW    $0000,$0000,$0000,$0000,FILEBUFF,DPB,$0000,ALV15

;-----------------------------------------------------------------------------
; First drive has a reserved track for CP/M
;-----------------------------------------------------------------------------
DPB0:        .DS    $0f

;            .DW    128                ; SPT - sectors per track
;            .DB    5                  ; BSH - block shift factor
;            .DB    31                 ; BLM - block mask
;            .DB    1                  ; EXM - Extent mask
;            .DW    2043               ; (2047-4) DSM - Storage size (blocks - 1)
;            .DW    511                ; DRM - Number of directory entries - 1
;            .DB    240                ; AL0 - 1 bit set per directory block
;            .DB    0                  ; AL1 -            "
;            .DW    0                  ; CKS - DIR check vector size (DRM+1)/4 (0=fixed disk)
;            .DW    1                  ; OFF - Reserved tracks

DPB:
;            .DW    128                ; SPT - sectors per track
;            .DB    5                 ; BSH - block shift factor
;            .DB    31                ; BLM - block mask
;            .DB    1                 ; EXM - Extent mask
;            .DW    2047              ; DSM - Storage size (blocks - 1)
;            .DW    511               ; DRM - Number of directory entries - 1
;            .DB    240               ; AL0 - 1 bit set per directory block
;            .DB    0                 ; AL1 -            "
;            .DW    0                 ; CKS - DIR check vector size (DRM+1)/4 (0=fixed disk)
;            .DW    0                 ; OFF - Reserved tracks

;-----------------------------------------------------------------------------
; Data storage
;-----------------------------------------------------------------------------
ALV00:       .DS    AVLSIZE;  * DRVCNT   ; allocation vectors
;ALV00:        .DS    257
;ALV01:        .DS    257
;ALV02:        .DS    257
;ALV03:        .DS    257
;ALV04:        .DS    257
;ALV05:        .DS    257
;ALV06:        .DS    257
;ALV07:        .DS    257
;ALV08:        .DS    257
;ALV09:        .DS    257
;ALV10:        .DS    257
;ALV11:        .DS    257
;ALV12:        .DS    257
;ALV13:        .DS    257
;ALV14:        .DS    257
;ALV15:        .DS    257
BIOSRAMSZ    .EQU    $-BIOSRAMS


             .ORG   $ff80
SERBUF:      .DS    $80
FINIS:       .END
