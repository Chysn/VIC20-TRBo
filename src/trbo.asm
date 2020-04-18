; TRBo: Turtle Rescue Bot
; (c)2020, Jason Justian

* = $1020

; System Resources
ISR    = $0314          ; ISR vector
SYSISR = $EABF          ; System ISR   
VOICEH = $900C
VOICEM = $900B
VOICEL = $900A
NOISE  = $900D  
VOLUME = $900E
BACKGD = $900F
BASRND = $E094          ; Routine for BASIC's RND() function
SCPAGE = $0288          ; Screen location start
RNDNUM = $8C            ; Result storage location for RND()
VIA1DD = $9113          ; Data direction register for joystick
VIA1PA = $9111
VIA2DD = $9122          ; Data direction register for joystick
VIA2PB = $9120
CLSR   = $E55F          ; Clear screen

; Constants
SCRCOM = $08            ; Light yellow with black border
SCRCOT = $3B            ; Cyan with cyan border
SPEED  = $10            ; Game speed, in jiffies of delay

; Characters
CH_PLR = $2A            ; Player character
CH_SPC = $20            ; Space
CH_LAD = $3D            ; Ladder
CH_WAL = $A0            ; Wall
CH_BLD = $2D            ; Building
                  
; Music Player                  
REG_L  = $033C          ; \ Storage for the shift register
REG_H  = $033D          ; /
TEMPO  = $033E          ; Tempo (lower numbers are faster)
MUCD   = $033F          ; Tempo countdown
PLAY   = $0340          ; Music is playing

; Maze Builder
SPOS_L = $01            ; \ Screen position (maze builder)
SPOS_H = $02            ; / Player screen position (play)
FRCD   = $05            ; Frame countdown
SCRPAD = $06            ; Scratchpad for a function
REMAIN = $0344          ; Remaining cells for the current level

; Game Play
SCOR_L = $0346          ; \ Player score
SCOR_H = $0347          ; /
GLEVEL = $0348          ; Game level
UNDER  = $0349          ; Character underneath player
INVEN  = $034A          ; Inventory
JOYDIR = $034B          ; Joystick direction capture
PLR_L  = $01            ; \ Player screen position (play)
PLR_H  = $02            ; / Screen position (maze builder)
CAND_L = $03            ; \ Candidate direction
CAND_H = $04            ; /

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MAIN PROGRAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
INIT:   SEI
        LDA #<CSTISR
        STA ISR
        LDA #>CSTISR
        STA ISR+1
        LDA #$08
        STA TEMPO
        STA MUCD 
        LDA #$32
        STA REG_L
        LDA #$23
        STA REG_H
        JSR M_STOP
        CLI
        
; Initialize the maze        
START:  JSR CLSR        ; Clear screen
        JSR MAZE
        LDA #SCRCOM
        STA BACKGD
        LDA #$58        ; Position the player at the top
        STA PLR_L       ;   of the maze.
        LDA SCPAGE
        STA PLR_H
        LDA #$7F        ; Set DDR to read East
        STA VIA2DD

; Initialize score and game locations
CLEAR:  LDY #$04
L0:     STA SCOR_L,Y
        DEY
        BPL L0
        JSR M_PLAY      ; Start the music
        LDY #$00
        LDA #CH_PLR
        STA (PLR_L),Y   ; Place the player
        LDA #CH_SPC
        STA UNDER       ; Start with a space under player

; Main loop
MAIN:   LDA #$00        ; Reset joystick
        STA JOYDIR      ;   and
        LDA #SPEED      ; Initialize frame
        STA FRCD
FRWAIT: JSR READJS      ; Read joystick
        LDA FRCD
        BNE FRWAIT      ; Wait for the frame counter to hit 0
        JSR PL_MV       ; Process the player's movement
        JSR NPC_MV      ; Process non-player movement
        JMP MAIN
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; INTERRUPT SERVICE ROUTINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CSTISR - The Interrupt Service Routine. Based on the tempo,
; get the next note and play it. Decrement the frame countdown.
CSTISR: LDA #$01
        BIT PLAY
        BEQ ENDISR
        DEC MUCD 
        BNE ENDISR
        LDA TEMPO
        STA MUCD 
        JSR NXNOTE
        LDA REG_H
        ORA #$80
        STA VOICEM
        LDA REG_L
        AND #$0F
        STA VOLUME
ENDISR: DEC FRCD
        JMP SYSISR        
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GAME PLAY SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PL_MV - Move the player, based on the joystick position
PL_MV:  LDA JOYDIR
        BEQ MV_RTS      ; Do nothing if no direction set
        LDA PLR_L       ; Set the candidate direction
        STA CAND_L
        LDA PLR_H
        STA CAND_H
JY_U:   LDA #$04        ; Handle up
        BIT JOYDIR
        BEQ JY_R
        LDA UNDER       ; You can only move up if you're on
        CMP #CH_LAD     ;   a ladder
        BNE JY_R
        JSR MVCD_U      ; Move candidate up
        JMP JY_F
JY_R:   LDA #$80        ; Handle right
        BIT JOYDIR
        BEQ JY_D
        JSR MVCD_R      ; Move candidate right
        JMP JY_F
JY_D:   LDA #$08        ; Handle down
        BIT JOYDIR
        BEQ JY_L
        JSR MVCD_D      ; Move candidate down
        JMP JY_F
JY_L:   LDA #$10        ; Handle left
        BIT JOYDIR
        BEQ JY_F
        JSR MVCD_L      ; Move candidate left
JY_F:   LDA #$20        ; Handle fire
        BIT JOYDIR
        BEQ DOMOVE:
        ; Do something
        
DOMOVE: JSR ISOPEN      ; Is the candidate space open?
        BEQ MV_RTS      ;   If not, don't move
        LDY #$00
        LDA UNDER       ; Restore the previous character
        STA (PLR_L),Y
        LDA (CAND_L),Y  ; Get the current character at candidate
        STA UNDER       ;   and save it for when we move away
        LDA #CH_PLR
        STA (CAND_L),Y  ; Moved character
        LDA CAND_L      ; Update player position
        STA PLR_L
        LDA CAND_H
        STA PLR_H
MV_RTS: RTS
        
NPC_MV: RTS

; READJS - Read the joystick, if it has not yet been read,
; and store a combined direction register in JOYDIR
READJS: LDA VIA1PA      ; Read VIA1 port
        AND #$3C        ; Keep track of bits 2,3,4,5
        STA SCRPAD
        LDA VIA2PB      ; Combine with read of bit 7
        AND #$80        ;   from VIA2-B
        ORA SCRPAD
        EOR #$BC        ; Flip each joystick bit in the
                        ;   combined read byte, so that
                        ;   on = 1
        BEQ READ_R      ; If any directions are selected,
                        ;   set the JOYDIR register
        STA JOYDIR
READ_R: RTS        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MOVEMENT SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MVCD_U - Move candidate direction up
MVCD_U: LDA #$16
        JMP MVCDSB

; MVCD_R - Move candidate direction right
MVCD_R: LDA #$01
        JMP MVCDAD

; MVCD_D - Move candidate direction down
MVCD_D: LDA #$16
        JMP MVCDAD

; MVCD_L - Move candidate direction left
MVCD_L: LDA #$01
        JMP MVCDSB
        
; MVCDAD - Add to direction
MVCDAD: CLC
        ADC CAND_L
        STA CAND_L
        BCC MVAD_R
        INC CAND_H
MVAD_R: RTS

; MVCDSB - Subtract from direction
MVCDSB: SEC
        STA SCRPAD
        LDA CAND_L
        SBC SCRPAD
        STA CAND_L
        BCS MVSB_R        
        DEC CAND_H
MVSB_R: RTS  

; ISOPEN - Is the candidate space open? A character is stopped 
; by a wall. Zero flag is clear if the candidate is open.
;
; Example
;    JSR MVCD_R
;    JSR ISOPEN
;    BEQ not_open
ISOPEN: LDY #$00
        LDA #CH_WAL
        CMP (CAND_L),Y
        RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MUSIC PLAYER SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; M_PLAY - Start the music player
M_PLAY: LDA #$01
        STA PLAY
        RTS
    
; M_STOP - Stop the music player
M_STOP  LDA #$00
        STA VOICEM
        STA PLAY
        RTS
                    
; NXNOTE - Rotates the 16-bit register one bit to the left
NXNOTE: LDX #$00        ; X is the carry bit for REG_H
        ASL REG_L       ; Shift the low byte, which may set C
        ROL REG_H       ; Rotate the high byte, including C
        BCC ROLL        ; Was the high bit of the high byte set?
        LDX #$01        ; If so, add it back to the beginning
ROLL    TXA
        ORA REG_L
        STA REG_L
        RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MAZE SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MAZE - Generate and display an 10x9 maze with the Sidewinder 
; algorithm. The maze is 8x8, but takes up a 16x16 on the screen
MAZE:   LDA #$6D        ; Fill the screen with walls, which
        STA SPOS_L      ;   will be removed to make the
        LDA SCPAGE      ;   maze.
        STA SPOS_H
        LDA #CH_WAL
        LDY #$FF
L1:     STA (SPOS_L),Y
        INC SPOS_H
        STA (SPOS_L),Y
        DEC SPOS_H
        DEY
        BNE L1
        LDA #$58        ; Offset for the maze
        STA SPOS_L
        LDA SCPAGE
        STA SPOS_H
        LDX #$00
        INC SPOS_L      ; Move to the next space to
        BNE LEVEL       ;   accommodate the left-hand
        INC SPOS_H      ;   maze border
LEVEL:  TXA
        PHA
        JSR DRLEV       ; Draw the level
        PLA
        TAX
        INX
        CPX #$09
        BNE LEVEL
        RTS
        
; DRLEV - Generate and draw a level of the sidewinder maze
;
; Preparations
;     X is the level number
DRLEV:  CPX #$00
        BEQ F_COR
        LDA #$2C        ; Drop to the next level by adding
        CLC             ; 44 (2 lines) to the screen position
        ADC SPOS_L
        STA SPOS_L
        BCC F_COR
        INC SPOS_H
F_COR:  LDA #$0A        ; Initialize the current level
        TAY             ; Default level length
NX_COR: STA REMAIN      ; Start a new corridor
        CPX #$00        ; Level 0 has a special case; it always
        BEQ DRAW        ;   has a single full-length corridor
        JSR RAND        ; Y = Length of the next corridor
DRAW:   JSR DRCORR      ; Draw the corridor
        STY SCRPAD      ; Update remaining cells by
        LDA REMAIN      ;   subtracting the size of the
        SEC             ;   current corridor from the
        SBC SCRPAD      ;   number of remaining cells
        BNE NX_COR      ; If any cells are left, keep going
        RTS

; DRCORR - Draw a corridor. The starting cell of the corridor
; is be 8 minus the number of remaining cells.
;
; Preparations
;     X is the level number
;     Y is the length of the corridor
DRCORR: LDA SPOS_L      ; Save the screen position
        PHA
        LDA SPOS_H
        PHA
        TYA             ; Save the Y register for the caller
        PHA
        LDA #$0A        ; Find the starting x-axis of this
        SEC             ;   corridor, which is 8 minus the
        SBC REMAIN      ;   number of remaining cells, and
        ASL             ;   multiplying by 2. Then advance
        CLC             ;   the screen position pointer to
        ADC SPOS_L      ;   the starting location.
        STA SPOS_L
        BCC KNOCK
        INC SPOS_H
KNOCK:  DEY             ; Keep one wall intact
        TYA             ; Double the length. This is how many
        ASL             ; walls are going to be knocked out.
        TAY
        LDA #$20        ; Knock out walls with a space
KNLOOP: STA (SPOS_L),Y  ; Knock out Y walls
        DEY
        BPL KNLOOP
; Select a random cell from the corridor and knock out a wall
; directly above it. This provides access to every other open
; cell in the maze.
        CPX #$00        ; If this is the first level, there's
        BEQ RESET       ; no knocking out the ceiling.
        PLA             ; A is now the passed Y register, the
                        ;   length of the corridor.
        PHA             ; But we still need Y for later
        JSR RAND        ; Y is now a random index within the
                        ;   corridor. 
        DEY               
        TYA
        ASL
        TAY
        LDA #CH_LAD     ; Put a ladder at the chosen position
        STA (SPOS_L),Y
        LDA SPOS_L
        SEC
        SBC #$16        ; Go up one line
        STA SPOS_L
        BCS CKNOCK
        DEC SPOS_H
CKNOCK: LDA #CH_LAD     ; Knock out ceiling with a ladder
        STA (SPOS_L),Y  ; Knock out the ceiling
RESET:  PLA             ; Start restoring things for return
        TAY
        PLA
        STA SPOS_H
        PLA
        STA SPOS_L
        RTS   

; RAND - Get a random number between 1 and 8. A contains the
; maximum value. The random number will be in Y.
RAND:   STA SCRPAD
        DEC SCRPAD      ; Behind the scenes, look for a number
                        ;   between 0 and A - 1. See the INY
                        ;   below, which compensates
        JSR BASRND
        LDA RNDNUM
        AND #$07
        CMP SCRPAD
        BCC E_RAND      ; Get another random number if this one
        BEQ E_RAND      ; is greater than the maximum
        INC SCRPAD
        LDA SCRPAD
        BNE RAND
E_RAND: TAY
        INY
        RTS
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GAME ASSET DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

