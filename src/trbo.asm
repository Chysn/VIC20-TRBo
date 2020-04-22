; TRBo: Turtle RescueBot
; (c)2020, Jason Justian

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BASIC LAUNCHER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This is a tokenization of the following BASIC program, which
; runs the game:
;     1 SYS4110
* = $1001
LAUNCH: .byte $0b,$04,$01,$00,$9e,$34,$31,$31
        .byte $30,$00,$00,$00,$00

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; LABEL DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
CLSR   = $E55F          ; Clear screen/home
HOME   = $E581          ; Home cursor
COLOR  = $0286          ; Text color
PRTSTR = $CB1E          ; Print from data (Y,A)
CHROM0 = $8000          ; Character ROM
CHROM1 = $8100          ; Character ROM
CASECT = $0291          ; Disable Commodore case
PRTFIX = $DDCD          ; Decimal display routine

; Constants
SCRCOM = $08            ; Maze color
SCRCOT = $3B            ; Top color
TXTCOL = $06            ; Text color
WALCOL = $01            ; Wall color

SPEED  = $10            ; Game speed, in jiffies of delay
CHRAM0 = $1C00          ; Custom Characters
CHRAM1 = $1D00          ; Custom Characters

; Characters
CH_SPC = $20            ; Space
CH_TUR = $21            ; Turtle Right (exclamation)
CH_TUL = $22            ; Turtle Left (double quote)
CH_TUU = $23            ; Turtle Up (octothorpe)
CH_PLR = $24            ; TRBo Right (dollar)
CH_PLL = $25            ; TRBo Left (percent)
CH_PLU = $26            ; TRBo Up/Down (ampersand)
CH_PAR = $27            ; Patrol Right (single quote)
CH_PAL = $28            ; Patrol Left (open paren)
CH_PAU = $29            ; Patrol Up/Down (close paren)
CH_BEA = $2A            ; Beam (asterisk)
CH_LAD = $2B            ; Ladder (plus)
CH_TER = $2C            ; Location Terminal (comma)
CH_CPY = $2E            ; Copyright (period)
CH_WAL = $2F            ; Wall (slash)
                  
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
REMAIN = $0344          ; Remaining cells for the current level

; Game Play
GLEVEL = $0345          ; Game level
SCOR_L = $0346          ; \ Player score
SCOR_H = $0347          ; /
UNDER  = $0348          ; Character underneath player
JOYDIR = $0349          ; Joystick direction capture
DIRBLK = $034A          ; Directional block
TURTLS = $034B          ; Turtle count for the level
PATRLS = $034C          ; Patrol count for the level
UNFOLL = $034D          ; Unfollow if fire is pressed

PLR_L  = $01            ; \ Player screen position (play)
PLR_H  = $02            ; / Screen position (maze builder)
CAND_L = $03            ; \ Candidate direction
CAND_H = $04            ; /

; General use registers - Any function may use these, but no
; function may assume that they're safe from other functions
SCRPAD = $06            ; Scratchpad for a function
DATA_L = $09            ; \ General data pointer
DATA_H = $0A            ; /

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MAIN PROGRAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Initialize the game system      
INIT:   JSR SETHW       ; Set up hardware features
        JSR CHRSET      ; Install the custom characters
        
; Intro Screen
GMOVER: JSR CLSR        ; Clear screen
        JSR MAZE        ; Draw Maze
        JSR WELCOM      ; Show intro screen
WAIT:   JSR READJS
        AND #$20        ; Wait for the fire button
        BEQ WAIT

; Initialize score and game locations
STGAME: LDA #$00
        STA SCOR_L
        STA SCOR_H
        STA GLEVEL 

; Start a new level
STLEV:  JSR INITLV

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
        LDA TURTLS      ; Has the level been completed?
        BEQ LVLUP
        JMP MAIN
        
LVLUP:  INC GLEVEL
        JSR INITLV
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
        LDA REG_L       ; This will set the volume and flash
        STA VOLUME      ;   the windows of the spaceship
ENDISR: DEC FRCD
        JMP SYSISR        
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GAME PLAY SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PL_MV - Move the player, based on the joystick position
PL_MV:  LDY #$00
        STY UNFOLL
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
        JSR MVCD_U      ; Move candidate up
        LDA SCPAGE      ; If the candidate is past the
        CMP CAND_H      ;   top of the play area, then
        BNE JY_UC       ;   leave the handler routine
        LDA #$58
        CMP CAND_L
        BCC JY_UC
        JSR MVCD_D
        RTS
JY_UC:  LDA #$01        ; Upward movement may proceed
        STA DIRBLK
        LDX #CH_PLU
        JMP JY_F
JY_R:   LDA #$80        ; Handle right
        BIT JOYDIR
        BEQ JY_D
        JSR MVCD_R      ; Move candidate right
        LDA #$02
        STA DIRBLK
        LDX #CH_PLR
        JMP JY_F
JY_D:   LDA #$08        ; Handle down
        BIT JOYDIR
        BEQ JY_L
        JSR MVCD_D      ; Move candidate down
        LDA #$04
        STA DIRBLK
        LDX #CH_PLU
        JMP JY_F
JY_L:   LDA #$10        ; Handle left
        BIT JOYDIR
        BEQ JY_F
        JSR MVCD_L      ; Move candidate left
        LDA #$08
        STA DIRBLK
        LDX #CH_PLL
        LDA CAND_H      ; If the candidate is off the left
        CMP SCPAGE      ;   side of the screen, then
        BNE JY_F        ;   restore the candidate back
        LDA CAND_L      ;   to the curent player
        CMP #$58        ;   coordinates
        BNE JY_F
        RTS
JY_F:   LDA #$20        ; Handle fire
        BIT JOYDIR
        BEQ DOMOVE
        LDY #$01
        STA UNFOLL      ; If Unfollow is set, the turtles
                        ;   will not follow you
DOMOVE: JSR ISOPEN      ; Is the candidate space open?
        BEQ MV_RTS      ;   If not, don't move
        TXA
        PHA             ; Save the player character
        LDX UNDER       ; Restore the previous character
        LDA PLR_L       ; ..
        LDY PLR_H       ; ..
        SEC             ; ..
        JSR PLACE       ; ..
        
        LDY #$00
        LDA (CAND_L),Y  ; Get the current character at candidate
        STA UNDER       ;   and save it for when we move away
        PLA             ; Place the player
        TAX             ; ..
        LDA CAND_L      ; ..
        LDY CAND_H      ; ..
        SEC             ; ..
        JSR PLACE       ; ..
        
        LDA PLR_H       ; Store the player position temporarily
        PHA             ;   so that we have the previous
        LDA PLR_L       ;   position after the update
        PHA
        
        LDA CAND_L      ; Update player position
        STA PLR_L
        LDA CAND_H
        STA PLR_H
        
        PLA             ; Put the previous position into 
        STA CAND_L      ;   candidate so that we can look around
        PLA             ;   for a turtle chain
        STA CAND_H
        
        LDA UNFOLL      ; If the fire button is down, then a
        BNE MV_RTS      ;   turtle chain can't be started
        LDY #$00        ; If there's a turtle in the previous
        LDA (CAND_L),Y  ;   position, then a turtle chain can't
        CMP #CH_TUR     ;   be started, or else that turle will
        BEQ MV_RTS      ;   be destroyed. Check for all three
        CMP #CH_TUL     ;   turtle characters.
        BEQ MV_RTS
        CMP #CH_TUU
        BEQ MV_RTS
        JSR TURCHN      ; Recursively find turtles for a chain
MV_RTS: RTS
       
; NPC_MV - Move non-player characters (turtles and patrols)        
NPC_MV: LDA #$5A        ; First, look for a turtle near the
        STA CAND_L      ;   spaceship. This turtle will be
        LDA SCPAGE      ;   rescued. Rescue involves
        STA CAND_H      ;   (1) Removing the turtle
        LDY #$00        ;   (2) Adding to the score    
        LDA (CAND_L),Y  ;   (3) Playing a sound
        CMP #CH_TUL     ;   (4) Decrement the turtle count
        BNE PATROL      ;   (5) Chaining other turtles
        LDA #$20        ; Remove the turtle
        STA (CAND_L),Y
        DEC TURTLS      ; Decrement the turtle count
        LDA #$64
        JSR USCORE      ; Add to the score
        LDA #$08
        STA DIRBLK
        JSR TURCHN      ; Chain other turtles
PATROL: RTS
        
        
; TURCHN - Turtle chain! look around the position in candidate
; for a turtle. If there's a turtle there, move it, then
; recursively call TURCHN to keep the chain going.
TURCHN: JSR SDATA       ; DATA will contain the original
                        ;   position, while the candidate
                        ;   may be moved
LOOK_U: LDY DIRBLK      ; Get the directional block, to avoid
                        ;   looking in the direction from which
        CPY #$01        ;   we came     
        BEQ LOOK_R      ; Direction is blocked; look right
        JSR MVCD_U
        JSR CH4TUR      ; Check for a turtle above
        BNE LOOK_R
        LDA #$04
        STA DIRBLK
        LDX #CH_TUU     ; Pulling turtle from above, so use
                        ;   the turtle on a ladder
        JMP MOVETU
LOOK_R: LDY DIRBLK
        CPY #$02
        BEQ LOOK_D
        JSR RSCAND
        JSR MVCD_R
        JSR CH4TUR
        BNE LOOK_D
        LDA #$08
        STA DIRBLK
        LDX #CH_TUL     ; Pulling a turtle from the right, so
                        ;   use the left-facing turtle
        JMP MOVETU
LOOK_D: LDY DIRBLK
        CPY #$04
        BEQ LOOK_L
        JSR RSCAND
        JSR MVCD_D
        JSR CH4TUR
        BNE LOOK_L
        LDA #$01
        STA DIRBLK
        LDX #CH_TUU     ; Pulling a turtle from below, so
                        ;   use the turtle on a ladder
        LDY #$00
        LDA (DATA_L),Y
        CMP #CH_LAD
        BEQ MOVETU
        DEX             ; If the turtle isn't being pulled
                        ;   ONTO a ladder, change to a left-
                        ;   facing turtle. The turtle might
                        ;   look confused for a moment, but
                        ;   that's baby turtles for ya
        JMP MOVETU
LOOK_L: LDY DIRBLK
        CPY #$08
        BEQ CHN_R
        JSR RSCAND
        JSR MVCD_L
        JSR CH4TUR
        BNE CHN_R
        LDA #$02
        STA DIRBLK
        LDX #CH_TUR     ; Pulling a turtle from the left, so
                        ;   use the right-facing turtle

; To move a turtle in the chain, a turtle will be placed in
; the DATA location, with the movement graphic, as set
; above in X. Then, the current position (DATA) needs to be 
; cleared out. It'll be cleared with either a space, or a 
; ladder, depending on the current graphic at the DATA 
; location.  Then, recursively call TURCH to continue the
; chain.
MOVETU: LDY #$00
        LDA (CAND_L),Y  ; What turtle is there now?
        PHA
        LDA (DATA_L),Y  ; Where the turtle is going?
        CMP #CH_LAD     ; To a ladder?
        BNE PL_TUR      ; If not, use the selected graphic
        LDX #CH_TUU     ; Otherwise, switch to the ladder turtle
PL_TUR: LDA DATA_L      ; Place the turtle in the DATA position
        LDY DATA_H      ; ...
        SEC             ; ...
        JSR PLACE       ; ...
        LDX #CH_SPC     ; Default to replacing with space
        PLA
        CMP #CH_TUU     ; But if the turtle is coming off a
        BNE PL_SL       ;   ladder, replace with a ladder
        LDX #CH_LAD
PL_SL : LDA CAND_L      ; Place the space or ladder
        LDY CAND_H      ; ...
        SEC             ; ...
        JSR PLACE       ; ...
        JMP TURCHN
CHN_R:  RTS
        
; MV_TUR - Move a turtle at the DATA register. Turtles don't
; usually move. A turtle will only move if doing so would put
; it next to the player, or to another turtle.
MV_TUR:  

; READJS - Read the joystick, if it has not yet been read,
; and store a combined direction register in JOYDIR, and
; return the same in A
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
MVCDSB: STA SCRPAD
        LDA CAND_L
        SEC
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
        
; CH4TUR - Check for turtle at candidate position. Zero flag
; is set if there's a turtle.
CH4TUR: TYA
        PHA
        LDY #$00
        LDA (CAND_L),Y
        TAX
        PLA
        TYA
        CPX #CH_TUR
        BEQ CH4_R
        CPX #CH_TUL
        BEQ CH4_R
        CPX #CH_TUU
        BEQ CH4_R
CH4_R:  RTS        
        
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
        LDY DATA_H      ; Falls through to CHRCOL
        
; CHRCOL - Set the color for the specified screen address. The
; screen address is converted to the color character address
; automatically.
;
; Preparations
;     A - Low byte of screen address
;     Y - High byte of screen addess
;     Carry flag - Color if set, hidden if unset
CHRCOL: STA DATA_L
        STY DATA_H
        PHA
        TXA
        PHA
        PHP
        LDY #$00
        LDA (DATA_L),Y  ; Character at the specified address
        SEC
        SBC #CH_SPC     ; Get a color table index
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
        TAX
        PLA
        RTS
        
; USCORE - Update the score and draw it on the screen
;
; Preparations
;     A is the amount to add to the current score
USCORE: CLC
        ADC SCOR_L
        STA SCOR_L
        BCC SCDRAW
        INC SCOR_H
SCDRAW: JSR HOME
        LDX SCOR_L
        LDA SCOR_H
        JSR PRTFIX
        RTS       

; SDATA - Set DATA pointer from candidate
SDATA:  LDA CAND_L
        STA DATA_L
        LDA CAND_H
        STA DATA_H
        RTS
       
; RSCAND - Reset candidate from DATA pointer 
RSCAND: LDA DATA_L
        STA CAND_L
        LDA DATA_H
        STA CAND_H
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
MAZE:   LDA #$6E        ; Fill the screen with walls, which
        STA SPOS_L      ;   will be removed to make the
        LDA SCPAGE      ;   maze.
        STA SPOS_H
        LDY #$00
L1:     LDA #CH_WAL
        STA (SPOS_L),Y
        INC SPOS_H
        STA (SPOS_L),Y
        DEC SPOS_H
        LDA #WALCOL     ; Set the maze to wall color
        STA $9600,Y
        STA $9700,Y
        INY
        BNE L1
        LDA #$59        ; Offset for the maze
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
        CPX #$00        ; Level 0 is a special case; it always
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
        LDA #CH_SPC     ; Knock out walls with a space
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
        CPX #$E8
        BNE CL1
        LDA #$FF        ; Switch over character map
        STA VICCR5
        RTS
        
; WELCOM - Set up welcome screen
WELCOM: LDA #SCRCOM     ; Set background color
        STA BACKGD
        LDA #<INTRO     ; Show the intro screen
        LDY #>INTRO
        JSR PRTSTR
        RTS      

; INITLV - Set up screen
INITLV: JSR CLSR
        JSR MAZE
        LDA #$3A        ; Place the spaceship
        STA SCRPAD
        LDA #$00
        STA DATA_L
        LDA SCPAGE
        STA DATA_H        
        LDX #$03
INITSH: LDY SHOFF,X
        LDA SCRPAD
        STA (DATA_L),Y
        LDA #$0D
        STA $9600,Y
        INC SCRPAD
        DEX
        BPL INITSH
        LDA #$5A        ; Position the player at the top
        STA PLR_L       ;   of the maze.
        LDY SCPAGE
        STY PLR_H
        LDX #CH_PLR
        SEC
        JSR PLACE       ; Place and color player
        LDA #CH_SPC
        STA UNDER       ; Start with a space under player
        LDY #$01        ; Populate the location terminal
        LDX #CH_TER     ; ...
        JSR POPULA      ; ...
        LDY #$0A        ; Populate some turtles
        STY TURTLS      ; .. Set the turtle count
        LDX #CH_TUR     ; ...
        JSR POPULA      ; ...
        LDA GLEVEL      ; Populate some patrols
        TAY             ; ...
        INY             ; ...
        INY             ; ...
        LDX #CH_PAL     ; ...
        JSR POPULA      ; ...
        LDA #$08
        STA TEMPO       ; Set the music tempo
        STA MUCD
        LDA GLEVEL
        ASL             ; Multiply level by 2 for score index
        TAX
        LDA SCORES,X    ; Set the musical score
        STA REG_L
        LDA SCORES+1,X
        STA REG_H
        LDA #$00
        JSR USCORE      ; Display current score
        JSR M_PLAY      ; Start the music
        RTS 

; SETHW - Some hardware setup. This only needs to be done
; once.
SETHW:  LDA #$08        ; Make the screen black during init
        STA BACKGD
        LDA #$7F        ; Set DDR to read East
        STA VIA2DD
        LDA CASECT      ; Disable Commodore-Shift
        ORA #$80
        STA CASECT
        JSR M_STOP      ; Turn off music playing
        LDA #TXTCOL
        STA COLOR
        SEI             ; Install the custom ISR
        LDA #<CSTISR
        STA ISR
        LDA #>CSTISR
        STA ISR+1
        CLI
        RTS

; POPULA - Populate the maze with some characters
;
; Preparations
;     X is the character
;     Y is the number of that character to put in the maze        
POPULA: TYA
        PHA
        TXA
        PHA
        LDA #$27
        STA DATA_L
        LDA SCPAGE
        STA DATA_H
        CPX #CH_TER     ; If the character is a computer
        BNE RNDY        ;   terminal, its Y position is not
        LDY GLEVEL      ;   random, but based on the current
        INY             ;   level
        CPY #$08
        BCC PL1
        LDY #$08
        BCS PL1
RNDY:   LDA #$08        ; Get a random Y-axis
        JSR RAND
PL1:    LDA #$2C        ; Drop down that number of lines
        CLC             ;   in the maze
        ADC DATA_L
        STA DATA_L
        BCC RY
        INC DATA_H
RY:     DEY
        BPL PL1
        LDA #$0A        ; Get a random X-axis
        JSR RAND
PL2:    LDA #$02        ; Move over that number of columns
        CLC             ;   in the maze
        ADC DATA_L
        STA DATA_L
        BCC RX
        INC DATA_H
RX:     DEY
        BPL PL2
        LDY #$00
        LDA (DATA_L),Y
        CMP #CH_SPC     ; Is the randomly-determined space
        BEQ PLCNEW      ;   occupied already?
        PLA
        TAX
        PLA
        TAY
        JMP POPULA      ; If so, then retry
PLCNEW  PLA
        TAX
        LDA DATA_L
        LDY DATA_H
        SEC             ; Hide all the populated objects
        JSR PLACE       ; Place the character in X, and
        PLA             ; Decrement the character number
        TAY             ;   counter. 
        DEY             ; Any more characters to place?
        BNE POPULA
        RTS
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GAME ASSET DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
INTRO:  .asc "TRBO- TURTLE RESCUEBOT"
        .asc "   . JASON JUSTIAN",$0d
        .asc "!   FIRE TO START    %",$00
        
ENDTXT: .asc "GAME OVER",$00

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
        .byte $00,$18,$18,$00,$00,$18,$18,$00 ; Colon
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
        .byte $00,$00,$00,$00,$00,$80,$80,$a8 ; Ship1
        .byte $00,$00,$00,$00,$00,$00,$00,$0a ; Ship2
        .byte $2b,$2f,$0b,$02,$00,$00,$02,$08 ; Ship3
        .byte $ba,$be,$b8,$a0,$80,$80,$20,$08 ; Ship4

; Color map for the above characters, indexed from SPACE
COLMAP: .byte $00,$05,$05,$05,$07,$07,$07,$04
        .byte $04,$04,$07,$01,$02,$01

; Spaceship part offsets        
SHOFF:  .byte $43,$42,$2C,$2D       

; Curated musical scores for the shift register player     
SCORES: .byte $32,$23
        .byte $12,$54
        .byte $d2,$2b
        .byte $ff,$2f
        .byte $54,$56
        .byte $18,$1f
        .byte $b3,$2a
        .byte $c6,$78
        .byte $54,$53
        .byte $19,$84
        .byte $19,$29
