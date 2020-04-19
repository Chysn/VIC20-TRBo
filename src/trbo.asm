; TRBo: Turtle RescueBot
; (c)2020, Jason Justian

* = $1020

; System Resources
ISR    = $0314          ; ISR vector
SYSISR = $EABF          ; System ISR   
VICCR5 = $9005          ; Character map register
VOICEH = $900C          ; High sound register
VOICEM = $900B          ; Mid sound register
VOICEL = $900A          ; Low sound register
NOISE  = $900D          ; Noise register
VOLUME = $900E          ; Sound volume register
BACKGD = $900F          ; Background color
BASRND = $E094          ; Routine for BASIC's RND() function
SCPAGE = $0288          ; Screen location start
RNDNUM = $8C            ; Result storage location for RND()
VIA1DD = $9113          ; Data direction register for joystick
VIA1PA = $9111          ; Joystick port (up, down, left, fire)
VIA2DD = $9122          ; Data direction register for joystick
VIA2PB = $9120          ; Joystick port (for right)
CLSR   = $E55F          ; Clear screen
PRTSTR = $CB1E          ; Print from data (Y,A)
CHROM0 = $8000          ; Character ROM
CHROM1 = $8100          ; Character ROM

; Constants
SCRCOM = $3B            ; Cyan with cyan border
SCRCOT = $18            ; white with black border
SPEED  = $10            ; Game speed, in jiffies of delay
CHRAM0 = $1C00          ; Custom Characters
CHRAM1 = $1D00          ; Custom Characters

; Characters
CH_SPC = $20            ; Space
CH_TUR = $21            ; Turtle Right (!)
CH_TUL = $22            ; Turtle Left (")
CH_TUU = $23            ; Turtle Up (#)
CH_PLR = $24            ; TRBo Right ($)
CH_PLL = $25            ; TRBo Left (%)
CH_PLU = $26            ; TRBo Up/Down (&)
CH_PAR = $27            ; Patrol Right (')
CH_PAL = $28            ; Patrol Left ( ( )
CH_PAU = $29            ; Patrol Up/Down ( ) )
CH_BEA = $2A            ; Beam (*)
CH_LAD = $2B            ; Ladder (+)
CH_TER = $2C            ; Location Terminal (,)
CH_SPA = $2D            ; Spaceship (-)
CH_CPY = $2E            ; Copyright (.)
CH_WAL = $2F            ; Wall (/)
                  
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
DATA_L = $09            ; \ Data pointer
DATA_H = $0A            ; /

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MAIN PROGRAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
INIT:   SEI             ; Install the custom ISR
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
        JSR CHRSET      ; Install the custom characters
        
; Initialize the maze        
START:  LDA #$00        ; Make the screen black during init
        STA BACKGD
        JSR CLSR        ; Clear screen
        JSR MAZE        ; Draw Maze
        JSR SCRSET      ; Setup various screen stuff
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
        LDA PLR_L
        LDY PLR_H
        LDX #CH_PLR
        SEC
        JSR PLACE       ; Place and color player
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
PL_MV:  LDY #$00
        LDA (PLR_L),Y
        TAX             ; Set current player character
        LDA JOYDIR
        BNE SETC        ; Do nothing if no direction set
        RTS
SETC:   LDA PLR_L       ; Set the candidate direction
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
        LDX #CH_PLU
        JMP JY_F
JY_R:   LDA #$80        ; Handle right
        BIT JOYDIR
        BEQ JY_D
        JSR MVCD_R      ; Move candidate right
        LDX #CH_PLR
        JMP JY_F
JY_D:   LDA #$08        ; Handle down
        BIT JOYDIR
        BEQ JY_L
        JSR MVCD_D      ; Move candidate down
        LDX #CH_PLU
        JMP JY_F
JY_L:   LDA #$10        ; Handle left
        BIT JOYDIR
        BEQ JY_F
        JSR MVCD_L      ; Move candidate left
        LDX #CH_PLL
        LDA CAND_H      ; If the candidate is off the left
        CMP SCPAGE      ;   side of the screen, then
        BNE JY_F        ;   restore the candidate back
        LDA CAND_L      ;   to the curent player
        CMP #$57        ;   coordinates
        BNE JY_F
        LDA PLR_L
        STA CAND_L
JY_F:   LDA #$20        ; Handle fire
        BIT JOYDIR
        BEQ DOMOVE:
        ; Do something
        
DOMOVE: JSR ISOPEN      ; Is the candidate space open?
        BEQ MV_RTS      ;   If not, don't move
        TXA
        PHA             ; Save the player character
        LDX UNDER       ; Restore the previous character
        LDA PLR_L       ; Prepare for PLACE
        LDY PLR_H
        SEC
        JSR PLACE
        LDY #$00
        LDA (CAND_L),Y  ; Get the current character at candidate
        STA UNDER       ;   and save it for when we move away
        PLA
        TAX             ; Prepare for PLACE
        LDA CAND_L
        LDY CAND_H
        SEC
        JSR PLACE
        LDA CAND_L      ; Update player position
        STA PLR_L
        LDY CAND_H
        STY PLR_H
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
        
; PLACE - Place the character on the screen at the specified
; address.
;
; Preparations
;     A - Low byte of the screen address
;     Y - High byte of the screen address
;     X - Character to place
;     Carry flag - Color if set, hidden if unset
PLACE:  STA DATA_L
        STY DATA_H
        TXA
        LDY #$00
        STA (DATA_L),Y
        LDA DATA_L
        LDY DATA_H      ; Falls through to COLOR
        
; COLOR - Set the color for the specified screen address. The
; screen address is converted to the color character address
; automatically.
;
; Preparations
;     A - Low byte of screen address
;     Y - High byte of screen addess
;     Carry flag - Color if set, hidden if unset
COLOR:  STA DATA_L
        STY DATA_H
        PHA
        PHP
        LDY #$00
        LDA (DATA_L),Y  ; Character at the specified address
        SEC
        SBC #$20        ; Get a color table index
        TAX             ;   and store it in X for later
        LDA DATA_H      ; Subtract the starting page of screen
        SEC             ;   memory from the specified page to
        SBC SCPAGE      ;   get the screen page offset
        CLC
        ADC #$96        ; Add that offset to color memory so
        STA DATA_H      ;   data now points to color location
        PLP
        BCS SETCOL      ; If carry flag is clear, set index to 0
        LDX #$00        ;   to use the space's color in the map
SETCOL: LDA COLMAP,X    ; Get the color for this character
        STA (DATA_L),Y  ; Set the color of the character
        PLA
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
MAZE:   LDA #$6E       ; Fill the screen with walls, which
        STA SPOS_L      ;   will be removed to make the
        LDA SCPAGE      ;   maze.
        STA SPOS_H
        LDY #$00
L1:     LDA #CH_WAL
        STA (SPOS_L),Y
        INC SPOS_H
        STA (SPOS_L),Y
        DEC SPOS_H
        LDA #$00        ; Set the maze to black
        STA $9600,Y
        STA $9700,Y
        INY
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

; RAND - Get a random number between 1 and 16. A contains the
; maximum value. The random number will be in Y.
RAND:   STA SCRPAD
        DEC SCRPAD      ; Behind the scenes, look for a number
                        ;   between 0 and A - 1. See the INY
                        ;   below, which compensates
        JSR BASRND
        LDA RNDNUM
        AND #$0F
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
;;;; SETUP SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CHRSET - Set up custom character assets
CHRSET: LDX #$00        ; Copy character ROM to RAM
CL0:    LDA CHROM0,X
        STA CHRAM0,X
        LDA CHROM1,X
        STA CHRAM1,X
        INX
        BNE CL0
CL1:    LDA CCHSET,X
        STA CHRAM1+8,X
        INX
        CPX #$C8
        BNE CL1
        LDA #$FF        ; Switch over character map
        STA VICCR5
        RTS        

; SCRSET - Set up screen
SCRSET: LDA #SCRCOM     ; Set background color
        STA BACKGD
        LDA #<INTRO     ; Show the intro screen
        LDY #>INTRO
        JSR PRTSTR
        LDX #CH_SPA     ; Place the spaceship (goal)
        LDA #$6D
        LDY SCPAGE
        SEC
        JSR PLACE
        RTS 
                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GAME ASSET DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
INTRO:  .byte "TRBO: TURTLE RESCUEBOT"
        .byte "   . JASON JUSTIAN",$0d
        .byte "!   FIRE TO START    %",$00

; Custom character set        
CCHSET: .byte $00,$00,$30,$7b,$7b,$fc,$48,$6c ; TurtleR
        .byte $00,$00,$0c,$de,$de,$3f,$12,$36 ; TurtleL
        .byte $00,$18,$5a,$42,$3c,$3c,$5a,$81 ; TurtleUp/Down
        .byte $0f,$0d,$07,$3c,$42,$99,$3c,$18 ; RobotR
        .byte $f0,$b0,$e0,$3c,$42,$99,$3c,$18 ; RobotL
        .byte $3c,$3c,$18,$3c,$42,$bd,$24,$24 ; RobotUp/Down
        .byte $00,$3c,$37,$3c,$3c,$00,$66,$66 ; PatrolR
        .byte $00,$3c,$ec,$3c,$3c,$00,$66,$66 ; PatrolL
        .byte $00,$18,$18,$3c,$7e,$00,$24,$24 ; PatrolUp/Down
        .byte $00,$00,$44,$aa,$11,$00,$00,$00 ; Beam
        .byte $24,$3c,$24,$24,$24,$3c,$24,$24 ; Ladder
        .byte $3c,$24,$3c,$00,$3c,$7e,$18,$3c ; LocationTerminal
        .byte $00,$18,$3c,$db,$7e,$18,$24,$42 ; Spaceship
        .byte $3c,$42,$99,$a1,$a1,$99,$42,$3c ; Copyright
        .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; Wall
        .byte $7f,$43,$43,$43,$41,$41,$7f,$00 ; 0
        .byte $03,$03,$03,$03,$01,$01,$01,$00 ; 1
        .byte $7f,$03,$03,$7f,$40,$40,$7f,$00 ; 2
        .byte $7f,$01,$01,$7f,$03,$03,$7f,$00 ; 3
        .byte $43,$43,$43,$7f,$01,$01,$01,$00 ; 4
        .byte $7f,$40,$40,$7f,$03,$03,$7f,$00 ; 5
        .byte $7f,$40,$40,$7f,$61,$61,$7f,$00 ; 6
        .byte $7f,$03,$03,$03,$01,$01,$01,$00 ; 7
        .byte $7f,$41,$41,$7f,$43,$43,$7f,$00 ; 8
        .byte $7f,$43,$43,$7f,$01,$01,$01,$00 ; 9

; Color map for the above characters, indexed from 0
COLMAP: .byte $00,$05,$05,$05,$06,$06,$06,$04
        .byte $04,$04,$07,$00,$02,$04
