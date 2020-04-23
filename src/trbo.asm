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
SCRCOM = $6E            ; Maze color
SCRCOT = $3B            ; Top color
TXTCOL = $01            ; Text color
SPEED  = $0E            ; Game speed, in jiffies of delay
CHRAM0 = $1C00          ; Custom Characters, Page 0
CHRAM1 = $1D00          ; Custom Characters, Page 1

; Sound Effects Library
FX_STA = $00            ; Start the game
FX_FIR = $01            ; Beam sound
FX_RES = $02            ; Rescue sound
FX_TER = $03            ; Computer terminal activated
FX_UNF = $04            ; Unfollow sound
FX_DMG = $05            ; The player was damaged

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
CH_WAL = $2D            ; Wall (minus)
                  
; Music Player                  
REG_L  = $033C          ; \ Music shift register
REG_H  = $033D          ; /
TEMPO  = $033E          ; Tempo (lower numbers are faster)
MUCD   = $033F          ; Tempo countdown
PLAY   = $0340          ; Music is playing

; Sound Effects Player
REG_FX = $034E          ; Sound effects register
FXLEN  = $034F          ; Sound effects length
FXCD   = $0350          ; Sound effects countdown
FXCDRS = $0351          ; Countdown reset value

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
HUNTER = $0352          ; Hunters will attack turtles
FIRED  = $0353          ; A beam was fired
HEALTH = $0354          ; Player health
PLR_L  = $01            ; \ Player screen position (play)
PLR_H  = $02            ; / Screen position (maze builder)
CAND_L = $03            ; \ Candidate direction
CAND_H = $04            ; /

; Start of Patrol Table
; Each entry in the Patrol Table contains four bytes. Indexed
; from the start of each entry, these are
;   0 - Patrol position (low byte)
;   1 - Patrol position (high byte)
;   2 - Multiple data points
;       bits 0-2 - Fire refresh countdown
;       bit 7    - Vertical travel (up=1, down=0)
;   3 - Character under the patrol (ladder or space)
PATTAB = $03C0          ; Start of Patrol table

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
        LDA #<INTRO     ; Show the intro screen
        LDY #>INTRO     ; ..
        JSR PRTSTR      ; ..
        LDY #$06        ; Populate some turtles
        LDX #CH_TUR     ; ...
        JSR POPULA      ; ...
        JSR REVEAL      ; Reveal the board
        JSR SPSHIP
WAIT:   JSR READJS
        AND #$20        ; Wait for the fire button
        BEQ WAIT

; Initialize score and game locations
STGAME: LDA #$00
        STA SCOR_L
        STA SCOR_H
        STA GLEVEL
        JSR SOUND
        LDA #$04
        STA HEALTH

; Start a new level
STLEV:  JSR INITLV
        JSR REVEAL      ; Diagnostic - remove when done

; Main loop
MAIN:   LDA #$00        ; Reset joystick
        STA JOYDIR      ;   and
        LDA #SPEED      ; Initialize frame
        STA FRCD
FRWAIT: JSR READJS      ; Read joystick
        LDA FRCD
        BNE FRWAIT      ; Wait for the frame counter to hit 0
        JSR CLBEAM      ; Clear the beam, if fired
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
; Custom ISR
; Play music, sound effects, and update frame countdown
CSTISR: JSR NXNOTE
        JSR NXFX
ENDISR: DEC FRCD
        JMP SYSISR        
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GAME PLAY SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Move the player
PL_MV:  LDY #$00
        STY UNFOLL
        STY DIRBLK
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
        LDA #FX_UNF
        JSR SOUND       ; Launch the alert sound
        LDA DIRBLK
        BEQ MV_R  
DOMOVE: JSR ISBLOC      ; Is the candidate space open?
        BEQ MV_R        ;   If not, don't move
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
        CMP #CH_TER     ; Has the player encountered the
        BNE PL_PL       ;   computer terminal?
        LDA #$20
        STA UNDER       ; Goes away after use
        LDA #FX_TER
        JSR SOUND       ; Launch the terminal sound
        JSR REVEAL      ; Reveal the board
PL_PL:  PLA             ; Place the player
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
        BNE MV_R        ;   turtle chain can't be started
        LDY #$00        ; If there's a turtle in the previous
        LDA (CAND_L),Y  ;   position, then a turtle chain can't
        CMP #CH_TUR     ;   be started, or else that turle will
        BEQ MV_R        ;   be destroyed. Check for all three
        CMP #CH_TUL     ;   turtle characters.
        BEQ MV_R  
        CMP #CH_TUU
        BEQ MV_R
        JSR TURCHN      ; Recursively find turtles for a chain
MV_R:   RTS
       
; NPC Move
; Move non-player characters (turtles and patrols)        
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
        LDA #FX_RES
        JSR SOUND       ; Launch the rescue sound
        LDA #$64
        JSR USCORE      ; Add to the score
        LDA #$08
        STA DIRBLK
        JSR TURCHN      ; Chain other turtles
PATROL: LDX PATRLS      ; Move each patrol
FORPAT: DEX             ; Patrols are zero-indexed
        JSR PAT_AI      ; Call patrol AI routine
        CPX #$00
        BNE FORPAT
        RTS
        
; Turtle Chain! 
; Look around the position in candidate for a turtle. 
; If there's a turtle there, move it, then recursively call
; TURCHN to keep the chain going.
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
        
; Patrol AI
; 
; Preparations
;     X is the patrol index
PAT_AI: TXA
        PHA
        ASL
        ASL             ; Index four bytes per patrol
        TAX             ; Actual table index
        LDA PATTAB,X
        STA CAND_L
        LDA PATTAB+1,X
        STA CAND_H
        JSR SDATA
        JSR LOS         ; Check line of sight
        LDY #$00        
IA_U:   JSR MVCD_U
        LDA SCPAGE      ; If the candidate is past the
        CMP CAND_H      ;   top of the play area, then
        BNE IA_UC       ;   go to the next option
        LDA #$58
        CMP CAND_L
        BCS IA_L
IA_UC:  JSR OPEN2P
        BNE IA_L
        LDA #CH_PAU
        STA (DATA_L),Y
        LDA (CAND_L),Y  ; Moving up to a space?
        CMP #CH_SPC
        BNE MOVPAT      ; If it's a ladder, just move
NEWDIR: LDA CAND_L      ; Once off the ladder, choose
        CMP PLR_L       ;   a new direction. This is also
        LDA #CH_PAR     ;   used below, when going down
        ADC #$00        ;   the ladder.
        STA (DATA_L),Y
        JMP MOVPAT
IA_L:   JSR RSCAND
        LDA (CAND_L),Y  ; Get the character here
        CMP #CH_PAL     ; Left patrol
        BNE AI_R
        JSR MVCD_L
        JSR OPEN2P
        BEQ MOVPAT
        LDA #CH_PAR     ; Turn around
        STA (DATA_L),Y
        JMP P_AI_R
AI_R:   JSR RSCAND
        LDA (CAND_L),Y
        CMP #CH_PAR     ; Right patrol
        JSR MVCD_R
        JSR OPEN2P
        BEQ MOVPAT
        LDA #CH_PAL     ; Turn around
        STA (DATA_L),Y
        JMP P_AI_R
MOVPAT: LDY #$00
        LDA (DATA_L),Y  ; Get the current character
        PHA             ;   and stash it for redraw
        LDA PATTAB+3,X  ; Restore the character under
        STA (DATA_L),Y  ;   and set the correct color
        LDA DATA_L      ; ..
        LDY DATA_H      ; ..
        SEC
        JSR CHRCOL      ; ..
        LDA CAND_L      ; Update the patrol data table
        STA PATTAB,X    ; ..
        LDA CAND_H      ; ..
        STA PATTAB+1,X  ; ..
        LDY #$00
        LDA (CAND_L),Y  ; Record the new under character
        STA PATTAB+3,X  ;   in the data table
        PLA             ; Get the character for redraw
        TAX             ; Prepare for placement
        LDA CAND_L      ; ..
        LDY CAND_H      ; ..
        SEC             ; ..
        JSR PLACE       ; ..
P_AI_R: PLA
        TAX
        RTS        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MOVEMENT SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Move Candidate Up
MVCD_U: LDA #$16
        JMP MVCDSB

; Move Candidate Right
MVCD_R: LDA #$01
        JMP MVCDAD

; Move Candidate Down
MVCD_D: LDA #$16
        JMP MVCDAD

; Move Candidate Left
MVCD_L: LDA #$01
        JMP MVCDSB
        
; Add to Candidate Direction
MVCDAD: CLC
        ADC CAND_L
        STA CAND_L
        BCC MVAD_R
        INC CAND_H
MVAD_R: RTS

; Subtract from Candidate Direction
MVCDSB: STA SCRPAD
        LDA CAND_L
        SEC
        SBC SCRPAD
        STA CAND_L
        BCS MVSB_R        
        DEC CAND_H
MVSB_R: RTS  

; Is Blocked to Player
; Is the candidate space open? A character is stopped by a wall
; or a patrol.  Zero flag is set if the candidate is blocked.
ISBLOC: LDY #$00
        LDA (CAND_L),Y
        CMP #CH_WAL
        BEQ OP_R
        CMP #CH_PAR
        BEQ OP_R
        CMP #CH_PAL
        BEQ OP_R
        CMP #CH_PAU
OP_R:   RTS
        
; Is Open to Patrol
; Patrols can move along corridors and ladders only.
; Anything else causes them to turn around. Zero flag
; is set if the candidate is open.
OPEN2P: LDY #$00
        LDA (CAND_L),Y
        JSR IS_COR
O2P_R:  RTS  

; Is Player
IS_PLR: CMP #CH_PLR     ; Is it a right-facing player?
        BEQ IS_P_R
        CMP #CH_PLL     ; Is it a left-facing player?
        BEQ IS_P_R
        CMP #CH_PLU     ; Is it a climbing player?
IS_P_R: RTS

; Is Turtle
IS_TUR: CMP #CH_TUR     ; Is it a right-facing turtle?
        BEQ IS_T_R
        CMP #CH_TUL     ; Is it a left-facing turtle?
        BEQ IS_T_R
        CMP #CH_TUU     ; Is it a climbing turtle?
IS_T_R: RTS

; Is Corridor
IS_COR: CMP #CH_SPC     ; Is it a space?
        BEQ IS_C_R
        CMP #CH_LAD     ; Is it a ladder?
IS_C_R: RTS
        
; Check for Turtle
; Check for turtle at candidate position. Zero flag
; is set if there's a turtle.
CH4TUR: LDY #$00
        LDA (CAND_L),Y
        JSR IS_TUR
        RTS        
        
; Place a Character
; Place the character on the screen at the specified address.
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

; Set Color at Address        
; Set the color for the specified screen address. The screen
; address is converted to the color address automatically.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GAME ACTION SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Clear Beam
CLBEAM: LDA FIRED
        BEQ CLB_R
        LDY #$00
        STY FIRED
        STY CAND_L
        LDA SCPAGE
        STA CAND_H
CLB1:   JSR CHKBM
        INC CAND_H
        JSR CHKBM
        DEC CAND_H
        INY
        BNE CLB1
CLB_R:  RTS
CHKBM:  LDA (CAND_L),Y
        CMP #CH_BEA
        BNE CHKB_R
        LDA #CH_SPC
        STA (CAND_L),Y
CHKB_R: RTS
        
; Update Score        
; Update the score and draw it on the screen
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
        
; Reveal the Board
; Usally a benefit of activating the terminal
REVEAL: LDX #$00
RL0:    TXA             ; Set up two character color calls...
        CMP #$6D
        BCC RP1         ; Ignore the top part of the screen
        PHA
        LDY SCPAGE
        SEC
        JSR CHRCOL      ; One for the first page of screen,
        PLA
RP1:    PHA
        TXA
        LDY SCPAGE
        INY
        SEC
        JSR CHRCOL      ; and one for the other
        PLA
        TAX
        INX
        BNE RL0
        RTS
        
; Read the Joystick
; Return the direction byte in A. If the joystick has not
; been moved this frame, also store the direction in the
; JOYDIR register.
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
;;;; MUSIC AND EFFECT PLAYER SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Start the music player
M_PLAY: LDA #$01
        STA PLAY
        RTS
    
; Stop the music player
M_STOP  LDA #$00
        STA VOICEM
        STA PLAY
        RTS
                    
; Play Next Note
; Rotates the 16-bit register one bit to the left
; and plays the note
NXNOTE: LDA #$01
        BIT PLAY
        BEQ NOTE_R
        DEC MUCD 
        BNE NOTE_R
        LDA TEMPO
        STA MUCD
        LDX #$00        ; X is the carry bit for REG_H
        ASL REG_L       ; Shift the low byte, which may set C
        ROL REG_H       ; Rotate the high byte, including C
        BCC NROLL       ; Was the high bit of the high byte set?
        LDX #$01        ; If so, add it back to the beginning
NROLL:  TXA
        ORA REG_L
        STA REG_L
        ORA #$80
        STA VOICEM
        LDA REG_H       ; This will set the volume and flash
        STA VOLUME      ;   the windows of the spaceship
NOTE_R: RTS

; Play Next Sound Effect
; Rotates the 8-bit sound effect register and
; plays the pitch      
NXFX:   LDA FXLEN       ; Has the sound been launched?
        BEQ ENDFX       ; If unlaunched, kill voice and return
        DEC FXLEN
        DEC FXCD
        BNE FX_R
        LDA FXCDRS      ; Reset the countdown
        STA FXCD        ; ..
        LDX #$00
        ROL REG_FX      ; Rotate the register left
        BCC EROLL
        LDX #$01
EROLL:  TXA
        ORA REG_FX
        STA REG_FX
        ORA #$80
ENDFX:  STA VOICEH
FX_R:   RTS      
        
; Launch Sound Effect
; Preparations
;     A - The sound effect index
SOUND:  SEI             ; Don't play anything while setting up
        STX SCRPAD
        ASL             ; Each effect has two parameters in the
                        ;   table, register and length (in
                        ;   jiffies.
        TAX
        LDA FXTYPE,X    ; Get the register
        STA REG_FX      ;   and activate it
        INX
        LDA FXTYPE,X    ; Get the length
        AND #$F0
        STA FXLEN       ;   and set it
        LDA FXTYPE,X
        AND #$0F
        STA FXCDRS      ; Record the reset value
        STA FXCD        ; Set the countdown
        LDX SCRPAD
        CLI             ; Go! 
        RTS
        
; Check Line-of-Sight for Patrol  
; The patrol is looking in the direction it is facing,
; for the player. If it sees the player, it will fire on 
; the player if its beam is charged.
;
; Preparations:
;     X contains the patrol table index
;     Candidate contains the current patrol character
LOS:    LDA PATTAB+2,X  ; Check on the charge of the
        BEQ CHRGED      ;   patrol's beam. After a shot,
        DEC PATTAB+2,X  ;   the patrol must wait a while
        RTS             ;   before shooting again.
CHRGED: LDA CAND_H
        PHA
        LDA CAND_L
        PHA
        LDY #$00
        LDA (CAND_L),Y
        STA SCRPAD
LOSNX:  LDA SCRPAD      ; A is the character
        CMP #CH_PAL     ; Determine facing direction
        BNE LO_R
        JSR MVCD_L
        JMP LOS_CH
LO_R:   CMP #CH_PAR
        BNE LOS_R
        JSR MVCD_R
LOS_CH: LDY #$00
        LDA (CAND_L),Y  ; A is now the next cell
        CMP #CH_WAL     ; Is it a wall?
        BEQ LOS_R       ; No line-of-sight found
        JSR IS_PLR
        BEQ FIBEAM
        LDY HUNTER      ; Are the patrols in hunt mode?
        BEQ LOSNX
        JSR IS_TUR
        BEQ FIBEAM
        BNE LOSNX       ; Keep going until something is hit
LOS_R:  PLA
        STA CAND_L
        PLA
        STA CAND_H
        RTS 
FIBEAM: LDA #$10        ; Discharge the beam
        SEC             ; ..
        SBC GLEVEL      ; ..
        STA PATTAB+2,X  ; ..
        INC HUNTER      ; Activate Hunter mode
        INC FIRED       ; Fire happened
        LDA #$07
        STA TEMPO       ; Make the music faster
        LDA #FX_FIR
        JSR SOUND       ; Launch the fire sound
        PLA             ; Firing in the direction the patrol
        STA CAND_L      ;   is facing. Reset the candidate
        PLA
        STA CAND_H
        PHA
        LDA CAND_L
        PHA
NXBEAM: LDA SCRPAD      ; Scratchpad is the character, for
        CMP #CH_PAL     ;   determining the direction
        BNE FIRE_R
        JSR MVCD_L
        JMP DRBEAM
FIRE_R: JSR MVCD_R
DRBEAM: LDY #$00
        LDA (CAND_L),Y
        PHA
        LDX #CH_BEA     ; Draw the beam
        LDA CAND_L      ; ..
        LDY CAND_H      ; ..
        SEC             ; ..
        JSR PLACE       ; ..
        PLA             ; Okay, what did we hit?
        JSR IS_COR      ; Is it a corridor?
        BEQ NXBEAM
        JSR IS_PLR      ; Is it the player
        BNE HITTUR
        JSR DAMAGE
        JMP LOS_R
HITTUR: JSR IS_TUR      ; Is it a turtle?
        BNE LOS_R
        DEC TURTLS      ; A turtle was killed; reduce the count
        JMP LOS_R       ; Anything else stops the beam
      
; Damage!
; The player or a turtle have taken a hit
; Falls through to DRAWPL
DAMAGE: DEC HEALTH
        LDA FX_DMG
        JSR SOUND       ; Falls through to PLPLR

; Place Player        
PLPLR:  LDA PLR_L
        LDY PLR_H
        LDX #CH_PLR
        SEC
        JMP PLACE
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MAZE SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Generate Maze
; Generates and displays an 10x9 maze with the Sidewinder 
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
        LDA #$00        ; Set the maze to be hidden
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
        
; Draw Level
; Generates and draw a level of the sidewinder maze
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
        AND #$0F        ; Limit corridors to eight cells
DRAW:   JSR DRCORR      ; Draw the corridor
        STY SCRPAD      ; Update remaining cells by
        LDA REMAIN      ;   subtracting the size of the
        SEC             ;   current corridor from the
        SBC SCRPAD      ;   number of remaining cells
        BNE NX_COR      ; If any cells are left, keep going
        RTS

; Draw Corridor
; The starting cell of the corridor is 10 minus the number 
; of remaining cells.
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

; Random Number
; Gets a random number between 1 and 16. A contains the
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
; Install Character Set
; Set up custom character assets
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
        
; Initialize Level
INITLV: JSR CLSR
        JSR M_STOP
        JSR MAZE
        JSR SPSHIP
        LDA #$5A        ; Position the player at the top
        STA PLR_L       ;   of the maze.
        LDY SCPAGE
        STY PLR_H
        JSR PLPLR       ; Place the player
        LDA #CH_SPC
        STA UNDER       ; Start with a space under player
        LDY #$01        ; Populate the location terminal
        LDX #CH_TER     ; ...
        JSR POPULA      ; ...
        LDY #$0A        ; Populate some turtles
        STY TURTLS      ; .. Set the turtle count
        LDX #CH_TUR     ; ...
        JSR POPULA      ; ...
        LDA #$00
        STA PATRLS      ; Initialize patrol data table
        LDA GLEVEL      ; Populate some patrols
        AND #$07        ; Limit patrols to eight (including INY)
        TAY             ; ...
        INY             ; ...
        LDX #CH_PAL     ; ...
        JSR POPULA      ; ...
        LDA #$08
        STA TEMPO       ; Set the music tempo
        STA MUCD
        LDA GLEVEL
        AND #$0F        ; Limit to 16 musical scores
        ASL             ; Multiply level by 2 for score index
        TAX
        LDA SCORES,X    ; Set the musical score
        STA REG_L
        LDA SCORES+1,X
        STA REG_H
        LDA #$00
        STA HUNTER      ; Reset Hunter flag
        JSR USCORE      ; Display current score
        JSR M_PLAY      ; Start the music
        LDA GLEVEL      ; After so many levels, start
        CMP #$08        ;   all patrols in Hunter mode
        BCC INIT_R      ;   to raise the difficulty
        INC HUNTER
        DEC TEMPO       ; And also make the music faster
INIT_R: RTS 

; Setup Hardware
SETHW:  LDA #SCRCOM     ; Set background color
        STA BACKGD
        LDA #$4F        ; Set volume and spaceship port color
        STA VOLUME
        LDA #$00        ; Initialize sound registers
        STA VOICEL      ; ..
        STA VOICEM      ; ..
        STA VOICEH      ; ..
        STA NOISE       ; ..
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
        LDA #SPEED      ; Initialize frame countdown before
        STA FRCD        ;   ISR is started
        CLI
        RTS

; Populate Maze
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
        CPX #CH_PAL     ; If placing a patrol,
        BNE PLL0        ;
        JSR ADDPAT      ;   add it to the patrol data table
PLL0:   CLC             ; Hide all the populated objects
        JSR PLACE       ; Place the character in X, and
        PLA             ; Decrement the character number
        TAY             ;   counter. 
        DEY             ; Any more characters to place?
        BNE POPULA
        RTS

; Draw the Spaceship        
SPSHIP: LDA #$3A
        STA SCRPAD
        LDA #$00
        STA DATA_L
        LDA SCPAGE
        STA DATA_H        
        LDX #$03
SSL0:   LDY SHOFF,X
        LDA SCRPAD
        STA (DATA_L),Y
        LDA #$0F
        STA $9600,Y
        INC SCRPAD
        DEX
        BPL SSL0
        RTS
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GAME DATA MANAGEMENT SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Add Patrol
ADDPAT: PHA
        TYA
        PHA
        TXA
        PHA
        LDA PATRLS
        ASL
        ASL             ; Allocate four bytes per Patrol
        TAX             ; Actual table index
        LDA DATA_L
        STA PATTAB,X
        LDA DATA_H
        STA PATTAB+1,X
        LDA #$0F        
        STA PATTAB+2,X  ; Set recharge time to 15 cycles
        INC PATRLS
        LDA #CH_SPC
        STA PATTAB+3,X  ; Set space under the patrol
        PLA
        TAX
        PLA
        TAY
        PLA
        RTS
  
; Set DATA Pointer from Candidate
SDATA:  LDA CAND_L
        STA DATA_L
        LDA CAND_H
        STA DATA_H
        RTS
       
; Reset Candidate from DATA pointer 
RSCAND: LDA DATA_L
        STA CAND_L
        LDA DATA_H
        STA CAND_H
        RTS
                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GAME ASSET DATA AND TABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
INTRO:  .asc "TRBO. TURTLE RESCUEBOT"
        .asc "   / JASON JUSTIAN",$0d
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
        .byte $ff,$cc,$88,$ff,$33,$22,$ff,$00 ; Wall
        .byte $00,$14,$14,$00,$00,$14,$14,$00 ; Colon
        .byte $3c,$42,$99,$a1,$a1,$99,$42,$3c ; Copyright
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
COLMAP: .byte $06,$05,$05,$05,$07,$07,$07,$04
        .byte $04,$04,$07,$01,$02,$01

; Spaceship part offsets        
SHOFF:  .byte $59,$58,$42,$43      

; Curated musical scores for the shift register player, in
; high byte/low byte order... so, backwards.   
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

; Sound effects for the sound effects player
; Each effect has three parameters
;   (1) First byte is the starting shift register value
;   (2) High nybble of second byte is the length in jiffies x 16
;   (3) Low nybble of second byte is refresh rate in jiffies
FXTYPE: .byte $2F,$34                       ; Start the Game
        .byte $55,$63                       ; Fire Sound
        .byte $03,$24                       ; Turtle rescue
        .byte $23,$62                       ; Terminal Activated
        .byte $4A,$11                       ; Unfollow
        .byte $55,$F8                       ; Damaged
