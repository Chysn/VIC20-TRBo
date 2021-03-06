;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                    TRBo: Turtle RescueBot
;                    (c)2020, Jason Justian
;                  
; Beta Release - April 26, 2020
; Final Release - May 3, 2020
; Director's Cut - May 7, 2020
; Assembled with XA
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This software is released under the Creative Commons
; Attribution-NonCommercial 4.0 International
; License. The license should be included with this file.
; If not, please see: 
;
; https://creativecommons.org/licenses/by-nc/4.0/legalcode.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BASIC LAUNCHER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This is the tokenization of the following BASIC program, which
; runs this game
;     42 SYS4110
* = $1001
BASIC:  .byte $0b,$04,$2a,$00,$9e,$34,$31,$31
        .byte $30,$00,$00,$00,$00

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; LABEL DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; System Resources
CINV   = $0314          ; ISR vector
NMINV  = $0318          ; NMI vector
SCREEN = $1E00          ; Screen character memory (unexpanded)
COLOR  = $9600          ; Screen color memory (unexpanded)
IRQ    = $EABF          ; System ISR   
BASRND = $E094          ; Routine for BASIC's RND() function
RNDNUM = $8C            ; Result storage location for RND()
VICCR5 = $9005          ; Character map register
VOICEH = $900C          ; High sound register
VOICEM = $900B          ; Mid sound register
VOICEL = $900A          ; Low sound register (unused)
NOISE  = $900D          ; Noise register
VOLUME = $900E          ; Sound volume register/aux color
BACKGD = $900F          ; Background color
VIA1DD = $9113          ; Data direction register for joystick
VIA1PA = $9111          ; Joystick port (up, down, left, fire)
VIA2DD = $9122          ; Data direction register for joystick
VIA2PB = $9120          ; Joystick port (for right)
CLSR   = $E55F          ; Clear screen/home
HOME   = $E581          ; Home text
TCOLOR = $0286          ; Text color
PRTSTR = $CB1E          ; Print from data (Y,A)
CASECT = $0291          ; Disable Commodore case
PRTFIX = $DDCD          ; Decimal display routine
CHROUT = $FFD2          ; Output one character
TIME_L = $A2            ; Jiffy counter low
TIME_M = $A1            ; Jiffy counter middle

; Constants - Game Configuration
ST_HLT = $08            ; Initial health level per game
MAXPAT = $05            ; Maximum number of patrols per level
MAXTUR = $08            ; Maximum number of turtles per level
CORLIM = $08            ; Maximum corridor size
SPEED  = $0E            ; Game speed, in jiffies of delay
SCRCOM = $08            ; Maze color
TXTCOL = $01            ; Text color

; Constants - Direction Blocking
UP     = $10            ; Block - Up
RIGHT  = $20            ; Block - Right
DOWN   = $40            ; Block - Down
LEFT   = $80            ; Block - Left

; Constants - Point Values
PT_RES = $64            ; Rescuing a turtle 100 pts
PT_TER = $FA            ; Finding Terminal  250 pts
PT_BON = $32            ; Bonus Health       50 pts
PT_HLT = $19            ; Found Health       25 pts

; Sound Effects Library
; The number is the index to effects parameters in the
; FXTYPE table
FX_STA = $00            ; Start the game
FX_FIR = $01            ; Beam sound
FX_RES = $02            ; Rescue sound
FX_TER = $03            ; Computer terminal activated
FX_DIG = $04            ; Dig sound
FX_DMG = $05            ; The player was damaged
FX_BON = $06            ; Bonus points!
FX_HLT = $07            ; Found health

; Characters
; Screen codes for custom characters
CH_SPC = $20            ; Space
CH_TUR = $21            ; Turtle Right (exclamation)
CH_TUL = $22            ; Turtle Left (double quote)
CH_TUC = $23            ; Turtle Climbing (octothorpe)
CH_PLR = $24            ; TRBo Right (dollar)
CH_PLL = $25            ; TRBo Left (percent)
CH_PLC = $26            ; TRBo Climbing (ampersand)
CH_PAR = $27            ; Patrol Right (single quote)
CH_PAL = $28            ; Patrol Left (open paren)
CH_PAC = $29            ; Patrol Climbing (close paren)
CH_BEA = $2A            ; Beam (asterisk)
CH_LAD = $2B            ; Ladder (plus)
CH_TER = $2C            ; Location Terminal (comma)
CH_WAL = $2D            ; Wall (minus)
CH_HLT = $2E            ; Health (period)
CH_FWA = $2F            ; False Wall (slash)
CH_BWV = $1C            ; Broken Wall Vert (GBP)
CH_BWH = $1D            ; Broken Wall Horiz (close bracket)
CH_PLD = $1E            ; TRBo Destroyed (up arrow)
   
; Music Player Memory                 
THEME  = $033C          ; \ Music shift register theme
THM_H  = $033D          ; /
TEMPO  = $033E          ; Tempo (lower numbers are faster)
MUCD   = $033F          ; Tempo countdown
PLAY   = $0340          ; Music is playing
FADE   = $0341          ; Fadeout volume

; Sound Effects Player Memory
REG_FX = $034E          ; Sound effects register
FXLEN  = $034F          ; Sound effects length
FXCD   = $0350          ; Sound effects countdown
FXCDRS = $0351          ; Countdown reset value

; Maze Builder Memory
REMAIN = $0344          ; Remaining cells for the current level

; Game Play Memory
FRCD   = $00            ; Frame countdown
GLEVEL = $05            ; Game level
SCORE  = $0346          ; \ Player score
SCOR_H = $0347          ; /
UNDER  = $0348          ; Character underneath player
MVDOWN = $FE            ; Patrol move-down indicator
JOYDIR = $F9            ; Joystick direction capture
DIRBLK = $FA            ; Block direction (UP,RIGHT, RIGHT)
TURTLS = $FB            ; Turtle count for the level
PATRLS = $FC            ; Patrol count for the level
HUNTER = $9C            ; Hunters will attack turtles
FIRED  = $0353          ; Any patrol fired this round
DIDFIR = $0354          ; Current patrol fired this round
HEALTH = $FD            ; Player health
LOSDIR = $0356          ; Line-of-sight direction
TABIDX = $0357          ; Current patrol's real table index
HISCOR = $0358          ; \ High score
HISC_H = $0359          ; /
PLAYER = $01            ; \ Player screen position
PLR_H  = $02            ; /
CURSOR = $03            ; \ CURSOR position
CUR_H  = $04            ; /

; Start of Patrol Table
; Each entry in the Patrol Table contains eight bytes
; From the start of each entry, these are
PATTAB = $03C0
PATROL = PATTAB+0       ; Patrol location low byte
PATL_H = PATTAB+1       ; Patrol location high byte
PAT_BR = PATTAB+2       ; Beam refresh (0 if ready to fire)
PAT_DI = PATTAB+3       ; Direction (UP, RIGHT, DOWN, LEFT)
PAT_TR = PATTAB+4       ; Vertical travel (UP, DOWN)
PAT_UN = PATTAB+5       ; Character under (CH_SPC, CH_LAD)
PAT_BF = PATTAB+6       ; Bump flag
PAT_CP = PATTAB+7       ; Ladder checkpoint

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
        LDA #$00        ; Initialize high score; this should
        STA HISCOR      ;   survive a RESTORE
        STA HISC_H      ;   ,,
        
; Welcome Screen
WELCOM: JSR CLSR        ; Clear screen
        JSR M_STOP      ; Stop music player, in case of RESTORE
        LDA #<INTRO     ; Show the intro screen
        LDY #>INTRO     ; ,,
        JSR PRTSTR      ; ,,
        JSR LAIR        ; Draw Maze
        LDY #$05        ; Populate some turtles
        LDX #CH_TUR     ; ,,
        JSR POPULA      ; ,,
        LDY #$02        ; Populate some patrols
        LDX #CH_PAL     ; ,,
        JSR POPULA      ; ,,
        LDY #$01        ; Add one TRBo
        LDX #CH_PLR     ; ,,
        JSR POPULA      ; ,,
        JSR REVEAL      ; Reveal the board
        JSR WAIT        ; Wait for the fire button

; Initialize score and game locations
MANUAL: JSR CLSR
        LDA #<MANTXT    ; Show the game manual
        LDY #>MANTXT    ; ,,
        JSR PRTSTR      ; ,,

; Start a new game        
START:  JSR WAIT        ; Wait for the fire button again
        LDA #$00        ; Initialize to zero
        STA GLEVEL      ; * The level
        STA SCORE       ; * The game score
        STA SCOR_H      ;   ,,
        STA FADE        ; * Disable music fade-out
        JSR SOUND       ; * Launch the game start sound
        LDA #ST_HLT     ; Initialize health
        STA HEALTH      ; ,,

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
        JSR CLBEAM      ; Clear the beam, if fired
        JSR PL_MV       ; Process the player's movement
        JSR NPC_MV      ; Process non-player movement
        LDA HEALTH      ; Is the player still alive?
        BEQ GAMOVR      ; ,,
        LDA TURTLS      ; Has the level been completed?
        BEQ LVLUP       ; ,,
        JMP MAIN

; Level Up
; When the turtles are either rescued or killed off
LVLUP:  LDA PLAYER      ; Is the player home?
        CMP #$59        ; If not, keep playing.
        BNE MAIN        ; ,,
        LDA PLR_H       ; ,,
        CMP #>SCREEN    ; ,,
        BNE MAIN        ; ,,
        JSR REVEAL      ; Reveal the board
        LDA #$0F        ; Fade out the music
        STA FADE        ; ,,
        LDA #$A0        ; Wait a few seconds to let the music
        JSR DELAY       ;   fade out
        LDA #$0E        ; Set the DATA pointer to the start
        STA DATA_L      ;   of the health gears. They will be
        LDA #>COLOR     ;   changed to white as the score
        STA DATA_H      ;   is increased for each gear remaining
        JSR M_STOP      ; Stop the music
        LDA #$08        ; Ensure that the volume is up, but
        STA VOLUME      ;   not too high
        LDA HEALTH      ; Store health for bonus countdown
        PHA             ; ,,
BONUS:  LDA #FX_BON     ; Launch the bonus sound effect
        JSR SOUND       ;   for each remaining health gear
        LDA #PT_BON     ; Increase the score for each gear
        JSR USCORE      ; ,,
        LDA #$01        ; Set the color of each remaining
        LDY #$00        ;   gear to white
        STA (DATA_L),Y  ;   ,,
        INC DATA_L      ;   ,,
        LDA #$06        ; A delay for each health count so that
        JSR DELAY       ;   ,, the player may count along!
        DEC HEALTH      ; Do this BONUS loop until the health
        BNE BONUS       ;   has been counted
        PLA             ; Restore health afterward, as promised
        STA HEALTH      ; ,,
        INC GLEVEL      ; Advance the level
        LDA #$80        ; One more little delay so the player
        JSR DELAY       ;   can catch his or her breath
        JMP STLEV       ; Start next level. LVLUP was branched
                        ;   into, so need need to worry about
                        ;   a return address on the stack.
        
; Game Over, Juggalos and Juggalettes!        
GAMOVR: JSR REVEAL      ; Reveal the board
        LDA #CH_PLD     ; Show the player
        LDY #$00        ; ,,
        STA (PLAYER),Y  ; ,,
        LDA #$06        ; Make the board blue, so that it
BLUE:   STA COLOR,Y     ;   looks more ominous
        STA COLOR+$0100,Y
        INY
        BNE BLUE
        JSR HOME        ; Show the Game Over text
        LDA #<ENDTXT    ; ,,
        LDY #>ENDTXT    ; ,,
        JSR PRTSTR      ; ,,
        LDA #$00        ; Show the final score
        JSR USCORE      ; ,,
        JSR HSCORE      ; Display high score
        LDA #$0F        ; Slow the music down, so that it
        STA TEMPO       ;   sounds more ominous. I love ominous!
        STA FADE        ; Fade out music
        JMP START       ; Back to waiting for fire button to
                        ;   start a new game

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; INTERRUPT SERVICE ROUTINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Custom ISR
; Play music, sound effects, and update frame countdown
CSTISR: JSR NXNOTE      ; Play next note of musical theme
        JSR NXFX        ; Play the sound effect
ENDISR: DEC FRCD        ; Update frame countdown
        JMP IRQ         ; Back to the default ISR
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GAME PLAY SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Move the player
PL_MV:  LDY #$00        ; Initialize the direction block; it
        STY DIRBLK      ;   tells which direction was chosen,
                        ;   and also which way we shouldn't
                        ;   look for turtles in the turtle
                        ;   train
        LDA (PLAYER),Y
        TAX             ; Set current player character
        LDA JOYDIR      ; Read the joystick and
        BNE SETC        ;   do nothing if no direction is set
        RTS
SETC:   JSR PLR2C       ; Move player position to CURSOR      
JY_U:   LDA #$04        ; Handle up
        BIT JOYDIR
        BEQ JY_R
        JSR MCUR_U      ; Move CURSOR up
        LDA #>SCREEN    ; If the CURSOR is past the
        CMP CUR_H       ;   top of the play area, then
        BNE JY_UC       ;   leave the handler routine
        LDA #$58
        CMP CURSOR
        BCC JY_UC
        JSR MCUR_D
        RTS
JY_UC:  LDA #UP         ; Upward movement may proceed
        STA DIRBLK
        LDX #CH_PLC
        JMP JY_F
JY_R:   LDA #$80        ; Handle right
        BIT JOYDIR
        BEQ JY_D
        JSR MCUR_R      ; Move CURSOR right
        LDA #RIGHT
        STA DIRBLK
        LDX #CH_PLR
        JMP JY_F
JY_D:   LDA #$08        ; Handle down
        BIT JOYDIR
        BEQ JY_L
        JSR MCUR_D      ; Move CURSOR down
        LDA #DOWN
        STA DIRBLK
        LDX #CH_PLC
        JMP JY_F
JY_L:   LDA #$10        ; Handle left
        BIT JOYDIR
        BEQ JY_F
        JSR MCUR_L      ; Move CURSOR left
        LDA #LEFT
        STA DIRBLK
        LDX #CH_PLL
        LDA CUR_H       ; If the CURSOR is off the left
        CMP #>SCREEN    ;   side of the screen, then
        BNE JY_F        ;   restore the CURSOR back
        LDA CURSOR      ;   to the curent player
        CMP #$58        ;   coordinates
        BNE JY_F
        RTS
JY_F:   LDA #$20        ; Handle fire button, whose function is
        BIT JOYDIR      ;   do dig a hole in the direction
        BEQ DOMOVE      ;   you're facing
        LDA DIRBLK      ; Can't dig a hole unless choosing
        BEQ MV_R        ;   a direction
        JSR DIG         ; Dig a hole at the cost of damage
        RTS             ; Don't 
DOMOVE: JSR ISBLOC      ; Is the CURSOR space open?
        BEQ MV_R        ;   If not, don't move
        TXA
        PHA             ; Save the player character
        LDX UNDER       ; Restore the previous character
        LDA PLAYER      ; ,,
        LDY PLR_H       ; ,,
        SEC             ; ,,
        JSR PLACE       ; ,,
        LDY #$00        ; Get the current character at CURSOR
        LDA (CURSOR),Y  ;   and save it for when we move away
        STA UNDER       ;   ,,
ENCTER: CMP #CH_TER     ; Has the player encountered the
        BNE ENCHLT      ;   computer terminal?
        JSR FNDTER      ; If so, handle it
        JMP PL_PL
ENCHLT: CMP #CH_HLT     ; Has the player encountered a health
        BNE PL_PL       ;   boost?
        JSR FNDHLT      ; If so, handle it
PL_PL:  PLA             ; Place the player
        TAX             ; ,,
        LDA CURSOR      ; ,,
        LDY CUR_H       ; ,,
        SEC             ; ,,
        JSR PLACE       ; ,,
        LDA PLR_H       ; Store the player position temporarily
        PHA             ;   so that we have the previous
        LDA PLAYER      ;   position after the update
        PHA             ;   ,,
        LDA CURSOR      ; Update player position
        STA PLAYER      ; ,,
        LDA CUR_H       ; ,,
        STA PLR_H       ; ,,
        PLA             ; Put the previous position into 
        STA CURSOR      ;   CURSOR so that we can look around
        PLA             ;   for a turtle train
        STA CUR_H       ; ,,
        LDY #$00        ; If there's a turtle in the previous
        LDA (CURSOR),Y  ;   position, then a turtle train can't
        JSR IS_TUR      ;   be started, or else that turle will
        BEQ MV_R        ;   be destroyed. Check for all three
                        ;   turtle characters.
        JSR TURTRN      ; Recursively find turtles for a train
MV_R:   JSR EXPLOR      ; Explore surroundings
        RTS
       
; NPC Move
; Move non-player characters (turtles and patrols)        
NPC_MV: LDA SCREEN+$5A  ; First, look for a turtle near the
        JSR IS_TUR      ;   safecraft. This turtle will be
        BNE MVPATS      ;   rescued. Rescue involves:
        LDA #CH_SPC     ;   (1) Removing the turtle
        STA SCREEN+$5A  ;   ,,
        JSR DECTUR      ;   (2) Decrementing the turtle count
        LDA #FX_RES     ;   (3) Launching a rescue sound
        JSR SOUND       ;   ,,
        LDA #PT_RES     ;   (4) Adding to the score
        JSR USCORE      ;   ,,
        LDA #$5A        ; Set the CURSOR to pull in
        STA CURSOR      ;   additional turtles in the train
        LDA #>SCREEN    ;   ,,
        STA CUR_H       ;   ,,
        LDA #LEFT
        STA DIRBLK
        JSR TURTRN
MVPATS: LDX PATRLS      ; Move each patrol
FORPAT: DEX             ; Patrols are zero-indexed
        JSR PAT_AI      ; Call patrol AI routine
        CPX #$00
        BNE FORPAT
        RTS
        
; Turtle Train! 
; Look around the position in CURSOR for a turtle. 
; If there's a turtle there, move it, then recursively call
; TURTRN to keep the train going.
TURTRN: JSR SDATA       ; DATA will contain the original
                        ;   position, while the CURSOR
                        ;   may be moved
LOOK_U: LDY DIRBLK      ; Get the directional block, to avoid
                        ;   looking in the direction from which
        CPY #UP         ;   we came     
        BEQ LOOK_R      ; Direction is blocked; look right
        JSR MCUR_U
        JSR CH4TUR      ; Check for a turtle above
        BNE LOOK_R
        LDA #DOWN
        STA DIRBLK
        LDX #CH_TUC     ; Pulling turtle from above, so use
                        ;   the turtle on a ladder
        JMP MOVETU
LOOK_R: LDY DIRBLK      ; All these other LOOK blocks do
        CPY #RIGHT      ;   do more or less the same thing,
        BEQ LOOK_D      ;   so I'm not going to comment each
        JSR RS_CUR
        JSR MCUR_R
        JSR CH4TUR
        BNE LOOK_D
        LDA #LEFT
        STA DIRBLK
        LDX #CH_TUL     ; Pulling a turtle from the right, so
                        ;   use the left-facing turtle
        JMP MOVETU
LOOK_D: LDY DIRBLK
        CPY #DOWN
        BEQ LOOK_L
        JSR RS_CUR
        JSR MCUR_D
        JSR CH4TUR
        BNE LOOK_L
        LDA #UP
        STA DIRBLK
        LDX #CH_TUC     ; Pulling a turtle from below, so
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
        CPY #LEFT
        BEQ CHN_R
        JSR RS_CUR
        JSR MCUR_L
        JSR CH4TUR
        BNE CHN_R
        LDA #RIGHT
        STA DIRBLK
        LDX #CH_TUR     ; Pulling a turtle from the left, so
                        ;   use the right-facing turtle

; To move a turtle in the train, a turtle will be placed in
; the DATA location, with the movement graphic, as set
; above in X. Then, the current position (DATA) needs to be 
; cleared out. It'll be cleared with either a space, or a 
; ladder, depending on the current graphic at the DATA 
; location.  Then, recursively call TURCH to continue the
; train.
MOVETU: LDY #$00
        LDA (CURSOR),Y  ; What turtle is there now?
        PHA
        LDA (DATA_L),Y  ; Where the turtle is going?
        CMP #CH_LAD     ; To a ladder?
        BNE PL_TUR      ; If not, use the selected graphic
        LDX #CH_TUC     ; Otherwise, switch to the ladder turtle
PL_TUR: LDA DATA_L      ; Place the turtle in the DATA position
        LDY DATA_H      ; ,,
        SEC             ; ,,
        JSR PLACE       ; ,,
        LDX #CH_SPC     ; Default to replacing with space
        PLA
        CMP #CH_TUC     ; But if the turtle is coming off a
        BNE PL_SL       ;   ladder, replace with a ladder
        LDX #CH_LAD
PL_SL : LDA CURSOR      ; Place the space or ladder
        LDY CUR_H       ; ,,
        SEC             ; ,,
        JSR PLACE       ; ,,
        JMP TURTRN
CHN_R:  RTS
        
; Patrol AI
; Fire if the player is in the line of sight. If the patrol did
; not fire, then determine a new position and direction.
;
; Preparations
;     X is the patrol index
PAT_AI: TXA
        ASL             ; Index eight bytes per patrol so that
        ASL             ;   X is the real table index
        ASL             ;   ,,
        TAX             ;   ,,
        STX TABIDX      ; Save the current real table index
        LDA PATROL,X    ; Set the CURSOR to the patrol's current
        STA CURSOR      ;   position
        LDA PATL_H,X    ;   ,,
        STA CUR_H       ;   ,,
AI_I:   LDA #$00        ; Reset whether this patrol has fired
        STA DIDFIR      ;   to check after line-of-sight (LOS)                        
        JSR LOS         ; Check line of sight
        JSR SDATA       ; Set DATA from CURSOR
        LDA DIDFIR      ; If the patrol fired during the check,
        BNE AI_R        ;   do nothing else
AI_II:  LDX TABIDX
        LDA PAT_DI,X    ; Get current direction
        AND #$50        ; UP + DOWN
        BEQ AI_III      ; If not on ladder, go to next step
        JSR ONLAD       ; If so, do the On Ladder routine
        JMP PAT_DR      ; Draw the patrol
AI_III: JSR OFFLAD      ; Do the Off Ladder routine
PAT_DR: LDY #CH_PAC     ; Determine the character based
        LDA PAT_DI,X    ;   on the new direction
        AND #$50        ; Is the direction up/down?
        BNE GOTCHR      ; If so, climbing is already in Y
        LDY #CH_PAL
        LDA PAT_DI,X
        CMP #RIGHT
        BNE GOTCHR      ; If not, left is already in Y
        LDY #CH_PAR     ; Switch to right
GOTCHR: TYA             ; Stash the new character
        PHA             ; ,,
        LDA PAT_UN,X    ; Get the character under the patrol
        TAX             ; Place the old character
        LDA DATA_L      ; ,,
        LDY DATA_H      ; ,,
        SEC             ; ,, 
        JSR PLACE       ; ,,
        JSR OPEN2P      ; Update the patrol's UNDER value
        BNE UP_PAT      ; ,,
        LDX TABIDX      ; ,,
        STA PAT_UN,X    ; ,,
UP_PAT: LDA CURSOR      ; Update the patrol table entry
        STA PATROL,X    ; ,,
        LDA CUR_H       ; ,,
        STA PATL_H,X    ; ,,
        PLA             ; Put the new character to draw in X
        TAX             ; Place the new patrol
        LDA CURSOR      ; ,,
        LDY CUR_H       ; ,,
        SEC             ; ,,
        JSR PLACE       ; ,,
AI_R:   LDA TABIDX      ; Restore X from Real Table Index back
        LSR             ;   to the patrol index, because the
        LSR             ;   caller uses X as an iterator
        LSR             ;   ,,
        TAX             ;   ,,
        RTS  

; On Ladder Movement
; Includes conditions for getting off the ladder
;
; X is the real table index
ONLAD:  LDA PAT_DI,X    ; Split off the Down travel case and
        CMP #DOWN       ;   the Up travel case
        BEQ ONDOWN
ONUP:   JSR MCUR_U      ; Move CURSOR upward and check
        JSR UPOPEN      ;   for availability
        BNE UP_LK
        RTS             ; Up is available, so return to draw
UP_LK:  JSR LOOKLR      ; No upward movement, so check the sides
        BCC STUCK       ; Sides not available, so stuck
        JSR MVOFFL      ; Side is available, so move off the
        JMP OFFLAD      ;   ladder
ONDOWN: JSR LOOKLR      ; When moving down, search sides first
        BCC CH_DN       ;   ,,
        JSR MVOFFL      ; Left or right are available, so move
        JMP OFFLAD      ;   off the ladder
CH_DN:  JSR RS_CUR      ; Can't move left or right, so try down
        JSR MCUR_D      ;   ,,
        JSR OPEN2P      ; If down is open, then return to draw;
        BNE STUCK       ;   otherwise, patrol is stuck
        RTS             ;   ,,
STUCK:  JSR RS_CUR      ; The patrol is vertically stuck
        JSR REV_TR      ; Reverse travel
        STA PAT_DI,X    ; ,,
        RTS             ; But end without moving

; Look Left and Right
; Carry flag is set if either direction is open
LOOKLR: JSR RS_CUR      ; Reset CURSOR
        JSR MCUR_R      ; Can patrol move right?
        JSR OPEN2P      ;   ,,
        BEQ L_YES       ;   ,,
        JSR RS_CUR      ; Or left?
        JSR MCUR_L      ;   ,,
        JSR OPEN2P      ;   ,,
        BNE L_NO        ;   ,,
L_YES:  SEC             ; Set or
        RTS
L_NO:   CLC             ; Clear carry flag
        RTS

; Off-Ladder Movement
; Includes conditions for getting onto a ladder
; 
; X is the real table index
OFFLAD: LDA PAT_BF,X    ; If the Bump flag is set, then consider
        BEQ OFF_R       ;   the travel direction first
CHKPT:  LDA PAT_CP,X    ; Has the patrol reached its checkpoint?
        CMP CURSOR      ;    ,,
        BNE OFF_D       ;    ,,
        LDA #$40        ; At the checkpoint, the patrol has a
        CMP THEME       ;   25% chance of taking the ladder
        BCC OFF_D       ;   and reversing direction of travel
        JSR REV_TR      ;   ,,
        STA PAT_DI,X    ;   ,, (direction comes from REV_TR)
        JMP ONLAD       ;   ,,
OFF_D:  LDA PAT_TR,X    ; The direction of travel is down
        CMP #DOWN       ; ,,
        BNE OFF_U
        JSR MCUR_D      ; If the patrol can move down from
        JSR OPEN2P      ;   here, then do it most of the time
        BNE OFF_R2      ;   and return for draw
        LDA THEME       ; There's a light probability component
        CMP #$40        ;   here, based on the current musical
        BCC OFF_R2      ;   register value
        LDA #DOWN       ;   ,,
        STA PAT_DI,X    ;   ,,
        RTS             ;   ,,
OFF_U:  JSR MCUR_U      ; If the patrol can move up from
        JSR UPOPEN      ;   here, then do it.
        BNE OFF_R       ;   ,,
        LDA #UP         ;   ,,
        STA PAT_DI,X    ;   ,,
        RTS             ;   ,,
OFF_R:  LDA MVDOWN      ;
        BEQ OFF_R2      ; If the MVDOWN flag was set by UPOPEN,
        LDA #DOWN       ;   called above, then set the travel to
        STA PAT_TR,X    ;   Down
OFF_R2: JSR RS_CUR
        LDA PAT_DI,X
        CMP #RIGHT
        BNE OFF_L
        JSR MCUR_R      ; If the patrol can move right
        JSR OPEN2P      ;   from here, then do it
        BNE BUMP        ;   ,,
        RTS             ;   ,,
OFF_L:  JSR MCUR_L      ; No need to check direction here
        JSR OPEN2P      ;   If it's open, then move and draw
        BNE BUMP        ; Otherwise, bump
        RTS
BUMP:   JSR RS_CUR      ; Cannot move; restore the CURSOR
        INC PAT_BF,X    ; Set the Bump flag
        LDA #RIGHT      ; If the direction is not already
        CMP PAT_DI,X    ;   to the right, then set it
        BEQ BUMP_L
OFFL_R: STA PAT_DI,X
        RTS             ; Return to draw routine        
BUMP_L: LDA #LEFT
        BNE OFFL_R
                
; Move Off Ladder
; Sets the direction to a pseudo-random one. But, more
; importantly, this sets the ladder checkpoint. If the patrol 
; reaches this point again, it will change its travel direction
; some of the time. Only the low byte is used for this operation
; since the point is to keep the patrol from getting stuck
; locally.
;
; X is the real table index
MVOFFL: LDA DATA_L      ; Set the checkpoint  
        STA PAT_CP,X    ;   ,,
        LDY #LEFT       ; Choose left or right with equal
        CPY THEME       ;   probability, based on the musical
        BCC SETDIR      ;   theme register (LEFT is nice and
        LDY #RIGHT      ;   $80)
SETDIR: LDA #$00        ; Reset the Bump flag for off-ladder
        STA PAT_BF,X    ;   movement
        TYA             ; Set the chosen direction, which will
        STA PAT_DI,X    ;   be used next time the patrol moves
        RTS     
       
; Reverse Vertical Travel 
;
; X is the real table index
REV_TR: LDY #DOWN
        LDA PAT_TR,X
        CMP #UP
        BEQ CHG_TR
        LDY #UP
CHG_TR: LDA #$00        ; Clear the checkpoint first, so that
        STA PAT_CP,X    ;   the Accumulator can be returned as
        TYA             ;   the newly-selected direction
        STA PAT_TR,X    ;   ,,
        RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MOVEMENT SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Move CURSOR Up
MCUR_U: LDA #$16
        JMP CUR_SB

; Move CURSOR Right
MCUR_R: LDA #$01
        JMP CUR_AD

; Move CURSOR Down
MCUR_D: LDA #$16
        JMP CUR_AD

; Move CURSOR Left
MCUR_L: LDA #$01
        JMP CUR_SB
        
; Add to CURSOR Direction
CUR_AD: CLC
        ADC CURSOR
        STA CURSOR
        BCC CADD_R
        INC CUR_H 
CADD_R: RTS

; Subtract from CURSOR Direction
CUR_SB: STA SCRPAD
        LDA CURSOR
        SEC
        SBC SCRPAD
        STA CURSOR
        BCS CSUB_R        
        DEC CUR_H 
CSUB_R: RTS  

; Is Blocked to Player
; Is the CURSOR space open? A character is stopped by a wall
; or a patrol.  Zero flag is set if the CURSOR is blocked.
ISBLOC: LDY #$00
        LDA (CURSOR),Y
        CMP #CH_WAL     ; Is it a wall?
        BEQ OP_R
        CMP #CH_FWA     ; Is it a false wall?
        BEQ OP_R
        JSR IS_PAT
OP_R:   RTS
        
; Is Up Open    
; Checks whether the Up direction is open to a patrol, taking
; into account the top of screen. Passed through to OPEN2P,
; below, which will finish the jorb. Zero flag is clear
; if the CURSOR is not open
UPOPEN: LDA #$00
        STA MVDOWN
        LDA CUR_H
        CMP #>SCREEN 
        BNE OPEN2P 
        LDA CURSOR 
        CMP #$58 
        BCS OPEN2P
        INC MVDOWN
        RTS
        
; Is Open to Patrol
; Patrols can move along corridors and ladders, and they can
; pass over bonus items. Anything else causes them to turn 
; around. Zero flag is set if the CURSOR is open
OPEN2P: LDY #$00
        LDA (CURSOR),Y
        JSR IS_COR
        BEQ O2P_R
        CMP #CH_HLT
        BEQ O2P_R
        CMP #CH_TER
O2P_R:  RTS  

; Is Player
IS_PLR: CMP #CH_PLR     ; Is it a right-facing player?
        BEQ IS_P_R
        CMP #CH_PLL     ; Is it a left-facing player?
        BEQ IS_P_R
        CMP #CH_PLC     ; Is it a climbing player?
IS_P_R: RTS

; Is Turtle
IS_TUR: CMP #CH_TUR     ; Is it a right-facing turtle?
        BEQ IS_T_R
        CMP #CH_TUL     ; Is it a left-facing turtle?
        BEQ IS_T_R
        CMP #CH_TUC     ; Is it a climbing turtle?
IS_T_R: RTS

; Is Patrol
IS_PAT: CMP #CH_PAR     ; Is it a right-facing patrol?
        BEQ ISPA_R
        CMP #CH_PAL     ; Is it a left-facing patrol?
        BEQ ISPA_R
        CMP #CH_PAC     ; Is it a climbing patrol?
ISPA_R: RTS

; Is Corridor
IS_COR: CMP #CH_SPC     ; Is it a space?
        BEQ IS_C_R
        CMP #CH_LAD     ; Is it a ladder?
IS_C_R: RTS
        
; Check for Turtle
; Check for turtle at CURSOR position. Zero flag
; is set if there's a turtle.
CH4TUR: LDY #$00
        LDA (CURSOR),Y
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
        SBC #$1C        ; Get a color table index
        TAX             ;   and store it in X for later
        LDA DATA_H      ; Subtract the starting page of screen
        SEC             ;   memory from the specified page to
        SBC #>SCREEN    ;   get the screen page offset
        CLC
        ADC #>COLOR     ; Add that offset to color memory so
        STA DATA_H      ;   data now points to color location
        PLP
        BCS SETCOL      ; If carry flag is clear, set index to 0
        LDX #$04        ;   to use the space's color in the map
SETCOL: LDA COLMAP,X    ; Get the color for this character
        STA (DATA_L),Y  ; Set the color of the character
        PLA
        TAX
        PLA
        RTS
        
; Reveal character at the CURSOR       
CUR_RV: LDA CURSOR
        LDY CUR_H 
        SEC
        JSR CHRCOL
        RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GAME ACTION SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Clear Beam
CLBEAM: LDA FIRED
        BEQ CLB_R
        LDY #$00
        STY FIRED
        STY CURSOR
        LDA #>SCREEN
        STA CUR_H 
CLB1:   JSR CHKBM
        INC CUR_H 
        JSR CHKBM
        DEC CUR_H 
        INY
        BNE CLB1
CLB_R:  RTS
CHKBM:  LDA (CURSOR),Y
        CMP #CH_BEA
        BNE CHKB_R
        LDA #CH_SPC
        STA (CURSOR),Y
CHKB_R: RTS
        
; Update Score        
; Update the score and display it on the screen
; Also update the high score, in case someone presses RESTORE
;
; Preparations
;     A is the amount to add to the current score
USCORE: CLC
        ADC SCORE
        STA SCORE
        BCC UHSC
        INC SCOR_H
UHSC:   LDA HISC_H      ; Is the last score greater than
        CMP SCOR_H      ;   the high score?
        BCC NEWHS       ;   ,,
        BNE SCDRAW      ;   ,,
        LDA SCORE       ;   ,,
        CMP HISCOR      ;   ,,
        BCC SCDRAW      ;   ,,
NEWHS:  LDA SCORE       ; A new high score has been
        STA HISCOR      ; achived; update high score
        LDA SCOR_H      ; ,,
        STA HISC_H      ; ,,        
SCDRAW: JSR HOME        ; Draw the level indicator and
        LDA #"L"        ;   score at the top of the screen,
        JSR CHROUT      ;   but there's no room for the
        LDX GLEVEL      ;   high score. The high score is
        INX             ;   displayed after the game, using
        LDA #$00        ;   HSCORE, below.
        JSR PRTFIX      ;   ,,
        LDA #CH_SPC     ;   ,,
        JSR CHROUT      ;   ,,
        JSR CHROUT      ;   ,,
        LDX SCORE       ;   ,,
        LDA SCOR_H      ;   ,,
        JSR PRTFIX      ;   ,,
        RTS             ;   ,,
        
; Draw High Score
; Display the high score
HSCORE: LDA #<HSTXT     ; Show the high score text
        LDY #>HSTXT     ; ,,
        JSR PRTSTR      ; ,,
        LDX HISCOR      ; Show the high score number
        LDA HISC_H      ;
        JSR PRTFIX      ;
        RTS

; Show Health Status        
SHOWHL: LDY #$00
NXHRT:  LDA #CH_HLT
        CPY HEALTH
        BCC HLPOS
        LDA #CH_SPC
HLPOS:  STA SCREEN+$0E,Y
        LDA #$06
        STA COLOR+$0E,Y
        INY
        CPY #$0A
        BNE NXHRT
        RTS
        
; Explore
; Reveal the area around the player
EXPLOR: JSR PLR2C       ; Set the CURSOR position
        JSR REV_UD
EX_R:   JSR MCUR_R      ; First, explore to the right
        JSR REV_UD      ; Reveal up and down here
        LDY #$00
        LDA (CURSOR),Y
        CMP #CH_SPC
        BEQ EX_R        ; Explore until something is hit
        JSR PLR2C
EX_L:   JSR MCUR_L
        JSR REV_UD      ; Reveal up and down from here
        LDY #$00
        LDA (CURSOR),Y
        CMP #CH_SPC
        BEQ EX_L
        JSR SCRAFT
        RTS
                
; Reveal Up/Down
; Reveal the CURSOR, and up and down from the CURSOR
REV_UD: JSR CUR_RV      ; Show the current CURSOR
        JSR MCUR_U
        JSR CUR_RV      ; Then one space up
        JSR MCUR_D
        JSR MCUR_D
        JSR CUR_RV      ; Then one space down
        JSR MCUR_U      ; And restore
        RTS
                
; Reveal the Board
; Usually a benefit of activating the terminal
REVEAL: LDX #$00
RL0:    TXA 
        CMP #$6D        ; Ignore the top part of the screen
        BCC RP1         ; ,,   
        PHA
        LDY #>SCREEN
        SEC
        JSR CHRCOL
        PLA
RP1:    PHA
        TXA
        LDY #>SCREEN+$0100
        SEC
        JSR CHRCOL
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

; Dig a Hole
; At the host of one point of damage
DIG:    LDA HEALTH
        CMP #$02        ; Can you even afford to dig? We won't
        BCC DIG_R       ;   let it kill you
        LDA CUR_H 
        PHA
        LDA CURSOR
        PHA
        JSR PLR2C       ; Set the CURSOR
        LDA DIRBLK      ; Which direction is blocked?
D_U:    CMP #UP
        BNE D_R
        JSR MCUR_U
        JMP DO_DIG
D_R:    CMP #RIGHT
        BNE D_D
        JSR MCUR_R
        JMP DO_DIG
D_D     CMP #DOWN
        BNE D_L
        JSR MCUR_D
        JMP DO_DIG
D_L     JSR MCUR_L
DO_DIG: LDY #$00
        LDA (CURSOR),Y
        CMP #CH_WAL     ; Can only dig walls
        BNE DIG_R
        LDX #CH_BWV     ; Dig with broken wall
        LDA DIRBLK      ; Which direction did you dig?
        AND #$50        ; Up or down?
        BNE SH_DIG      ; If not, use the horiz wall
        LDX #CH_BWH     ;   ..
SH_DIG: TXA
        STA (CURSOR),Y
        LDA #FX_DIG     ; Launch the digging sound
        JSR SOUND       ; ,,
        JSR DAMAGE      ; Take one point of damage
DIG_R:  PLA
        STA CURSOR
        PLA
        STA CUR_H 
        RTS     

; Found the Terminal        
FNDTER: LDA #CH_SPC     ; Goes away after use
        STA UNDER       ; ,,   
        LDA #FX_TER     ; Launch the terminal sound
        JSR SOUND       ; ,,
        JSR REVEAL      ; Reveal the board
        LDA #$00        ; Quiesce the patrols
        STA HUNTER      ; ,,  
        LDA #$08        ; Bring the tempo to normal
        STA TEMPO       ; ,,
        LDA #PT_TER     ; Add to the score
        JSR USCORE      ; ,,
        RTS 

; Found a Health Boost
FNDHLT: LDA #CH_SPC     ; Goes away after use
        STA UNDER       ; ,,   
        LDA HEALTH
        CMP #$08        ; Already maxed out
        BCS HLT_R       ; ,,
        INC HEALTH      ; Increase and display
        JSR SHOWHL      ; ,,
HLT_R:  LDA #FX_HLT     ; Launch the bonus sound
        JSR SOUND       ; ,,
        LDA #PT_HLT     ; Add to the score
        JSR USCORE      ; ,,
        LDA GLEVEL      ; Reset the music to the level
        AND #$0F        ;   theme, in case health was
        JSR MUSIC       ;   previously at 1
        RTS
        
; Hunt
; All patrols enter Hunter mode, which causes them to shoot
; turtles on sight. Also, the music gets faster.
HUNT:   LDA #$07
        STA HUNTER      ; Only needs to be non-zero 
        STA TEMPO
        RTS
       
; Decrement Turtle Count
; If it hits 0, change the musical theme to notify the player        
DECTUR: DEC TURTLS
        BNE DECT_R
        LDA #$09        ; The "turtles are gone" alert
        JSR MUSIC
DECT_R: RTS
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MUSIC AND EFFECT PLAYER SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Start the music player
M_PLAY: LDA #$01
        STA PLAY
        LDA #$00
        STA FADE
        RTS
    
; Stop the music player
M_STOP: LDA #$00
        STA VOICEM
        STA PLAY
        RTS

; Select Music
; Set a musical theme 
;
; Preparations
;     A is the theme index
MUSIC:  ASL             ; Multiply level by 2 for theme index
        TAX
        LDA THEMES,X    ; Set the musical theme
        STA THEME 
        LDA THEMES+1,X
        STA THM_H
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
        LDA #$00        ; Shift the register left
        ASL THEME       ; ,,
        ROL THM_H       ; ,,
        ADC THEME       ; ,,
        STA THEME       ; ,,
        ORA #$80        ; Gate the middle voice
        STA VOICEM      ; ,,
        LDA FADE        ; Fade is a volume override. If fade is
        BEQ VOLREG      ;   set, it will decrease every note,
        DEC FADE        ;   and the music will stop when it
        BNE VOL         ;   reaches zero
        JMP M_STOP
VOLREG: LDA THM_H       ; Set the music volume and flash
VOL:    STA VOLUME      ;   the windows of the safecraft
        LDA HUNTER      ; If the patrols are in hunter mode,
        BEQ NOTE_R      ;   play a low counterpoint
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
        STA FXCD        ; ,,
        LDA #$00        ; Rotate the register left
        ASL REG_FX      ; ,,
        ADC REG_FX      ; ,,
        STA REG_FX      ; ,,
        ORA #$80        ; Gate the high voice
ENDFX:  STA VOICEH      ; ,,
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
        
; Line-of-Sight 
; The patrol is looking in the direction it is facing,
; for the player. If it sees the player, it will fire on 
; the player if its beam is charged.
;
; Preparations:
;     X contains the patrol table index
;     CURSOR contains the current patrol character
LOS:    LDA PAT_BR,X    ; Check on the charge of the
        BEQ CHRGED      ;   patrol's beam. After a shot,
        DEC PAT_BR,X    ;   the patrol must wait a while
        RTS             ;   before shooting again.
CHRGED: LDA CUR_H 
        PHA
        LDA CURSOR
        PHA
        LDY #$00
        LDA (CURSOR),Y
        STA LOSDIR
LOSNX:  LDA LOSDIR      ; A is the character
        CMP #CH_PAL     ; Determine facing direction
        BNE LO_R
        JSR MCUR_L
        JMP LOS_CH
LO_R:   CMP #CH_PAR
        BNE LOS_R
        JSR MCUR_R
LOS_CH: LDY #$00
        LDA (CURSOR),Y  ; A is now the next cell
        CMP #CH_WAL     ; Is it a wall?
        BEQ LOS_R       ;   No line-of-sight found
        CMP #CH_FWA     ; Is it a false wall?
        BEQ LOS_R       ;   No line-of-sight found
        CMP #$3D        ; Right corner of craft?
        BEQ LOS_R       ;   No line-of-sight found
        JSR IS_PAT      ; Is it a patrol?
        BEQ LOS_R       ;   They don't fire on each other
        JSR IS_PLR      ; Is it the player?
        BEQ FIBEAM      ;   Yeah, shoot that!
        LDY HUNTER      ; Are the patrols in hunt mode?
        BEQ LOSNX
        JSR IS_TUR      ; If so, fire at turtles
        BEQ FIBEAM
        BNE LOSNX       ; Keep going until something is hit
LOS_R:  PLA
        STA CURSOR
        PLA
        STA CUR_H 
        RTS 
FIBEAM: LDA #$0F        ; Discharge the beam
        STA DIDFIR      ; Mark beam as fired for this patrol
        SEC             ; ,,
        SBC GLEVEL      ; ,,
        STA PAT_BR,X  ; ,,
        INC FIRED       ; Fire happened
        JSR HUNT        ; Turn on Hunter mode
        LDA #FX_FIR
        JSR SOUND       ; Launch the fire sound
        PLA             ; Firing in the direction the patrol
        STA CURSOR      ;   is facing. Reset the CURSOR
        PLA
        STA CUR_H 
        PHA
        LDA CURSOR
        PHA
NXBEAM: LDA LOSDIR      ; LOSDIR is the character, for
        CMP #CH_PAL     ;   determining the direction
        BNE FIRE_R
        JSR MCUR_L
        JMP DRBEAM
FIRE_R: JSR MCUR_R
DRBEAM: LDY #$00
        LDA (CURSOR),Y
        CMP #CH_WAL
        BEQ LOS_R       ; Stop at a wall
        JSR IS_PLR
        BNE OTHER
        JSR DAMAGE
        JMP LOS_R
OTHER:  PHA
        LDX #CH_BEA     ; Draw the beam
        LDA CURSOR      ; ,,
        LDY CUR_H       ; ,,
        SEC             ; ,,
        JSR PLACE       ; ,,
        PLA             ; Okay, what did we hit?
        JSR IS_COR      ; Is it a corridor?
        BEQ NXBEAM
HITTUR: JSR IS_TUR      ; Is it a turtle?
        BNE LOS_R
        JSR DECTUR      ; A turtle was killed; reduce the count
        JMP LOS_R       ; Anything else stops the beam
      
; Damage!
; The player has taken a hit
DAMAGE: LDA HEALTH
        BEQ DAMA_R
        DEC HEALTH
        JSR SHOWHL      ; Show health
        LDA #FX_DMG
        JSR SOUND
        LDA #$01        ; If health gets to 1, change
        CMP HEALTH      ;   the theme to an alarm tone
        BNE DAMA_R      ;   ,,
        LDA #$08        ;   ,,
        JSR MUSIC       ;   ,,
DAMA_R: RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; LAIR SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Generate Lair
; Generates and displays an 10x9 lair with the Sidewinder 
; algorithm.
LAIR:   LDY #$00
L1:     LDA #CH_WAL
        STA SCREEN+$6E,Y
        STA SCREEN+$016E,Y
        LDA #$00
        STA COLOR+$6E,Y
        STA COLOR+$016E,Y
        INY
        BNE L1
        LDA #$58        ; Draw a "false wall."
        STA CURSOR      ;   The false wall is a strip along the
        LDA #>SCREEN    ;   left-hand side of the playing field
        STA CUR_H       ;   that uses the same graphic as a wall,
        LDX #$12        ;   but is not a wall. Its purpose is to
FWAL:   LDY #$00        ;   prevent the player from digging from
        JSR MCUR_D      ;   one side of the board to the other,
        LDA #CH_FWA     ;   which is something that should be
        STA (CURSOR),Y  ;   physically impossible.
        INY             ;   ,,
        STA (CURSOR),Y  ;   ,,
        DEX             ;   ,,
        BNE FWAL        ;   ,,
        LDA #$5A        ; Offset for the lair
        STA CURSOR
        LDA #>SCREEN
        STA CUR_H 
        LDX #$00
LEVEL:  TXA
        PHA
        JSR DRLEV       ; Draw the level
        PLA
        TAX
        INX
        CPX #$09
        BNE LEVEL
        JSR SCRAFT
        RTS
        
; Draw Level
; Generates and draw a level of the sidewinder lair
;
; Preparations
;     X is the level number
DRLEV:  CPX #$00
        BEQ F_COR
        LDA #$2C        ; Drop to the next level by adding
        CLC             ; 44 (2 lines) to the screen position
        ADC CURSOR
        STA CURSOR
        BCC F_COR
        INC CUR_H 
F_COR:  LDA #$0A        ; Initialize the current level
        TAY             ; Default level length
NX_COR: STA REMAIN      ; Start a new corridor
        CPX #$00        ; Level 0 is a special case; it always
        BEQ DRAW        ;   has a single full-length corridor
        CMP #CORLIM     ; Limit the size of a single corridor
        BCC G_LEN       ; ,,
        LDA #CORLIM     ; ,,
G_LEN:  JSR PSRAND      ; Y = Length of the next corridor
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
DRCORR: LDA CUR_H      ; Save the screen position
        PHA
        LDA CURSOR 
        PHA
        TYA             ; Save the Y register for the caller
        PHA
        LDA #$0A        ; Find the starting x-axis of this
        SEC             ;   corridor, which is 8 minus the
        SBC REMAIN      ;   number of remaining cells, and
        ASL             ;   multiplying by 2. Then advance
        CLC             ;   the screen position pointer to
        ADC CURSOR      ;   the starting location.
        STA CURSOR
        BCC KNOCK
        INC CUR_H 
KNOCK:  DEY             ; Keep one wall intact
        TYA             ; Double the length. This is how many
        ASL             ; walls are going to be knocked out.
        TAY
        LDA #CH_SPC     ; Knock out walls with a space
KNLOOP: STA (CURSOR),Y  ; Knock out Y walls
        DEY
        BPL KNLOOP
; Select a random cell from the corridor and knock out a wall
; directly above it. This provides access to every other open
; cell in the lair.
        CPX #$00        ; If this is the first level, there's
        BEQ RESET       ; no knocking out the ceiling.
        PLA             ; A is now the passed Y register, the
                        ;   length of the corridor.
        PHA             ; But we still need Y for later
        JSR PSRAND      ; Y is now a random index within the
                        ;   corridor. 
        DEY               
        TYA
        ASL
        TAY
        LDA #CH_LAD     ; Put a ladder at the chosen position
        STA (CURSOR),Y  ; ,,
        LDA CURSOR
        SEC
        SBC #$16        ; Go up one line
        STA CURSOR
        BCS CKNOCK
        DEC CUR_H 
CKNOCK: LDA #CH_LAD     ; Knock out ceiling with a ladder
        STA (CURSOR),Y  ; ,,
RESET:  PLA             ; Start restoring things for return
        TAY             ; ,,
        PLA             ; ,,
        STA CURSOR      ; ,,
        PLA             ; ,,
        STA CUR_H       ; ,,
        RTS   
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SETUP SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Initialize Level
INITLV: JSR CLSR
        LDA #$00
        STA HUNTER      ; Disable Hunter mode
        STA PATRLS      ; Initialize patrol table data
        JSR USCORE      ; Display current score
        JSR SHOWHL      ; Display current health
        JSR LAIR        ; Create and draw the lair and safecraft
        LDA #$5A        ; Position the player at the top
        STA PLAYER      ;   of the lair, and then place
        LDY #>SCREEN    ;   the player
        STY PLR_H       ;   ,,
        LDX #CH_PLR     ;   ,,
        SEC             ;   ,,
        JSR PLACE       ;   ,,
        LDA #CH_SPC     ; Start with a space under player
        STA UNDER       ; ,,
        LDA GLEVEL      ; Patrols start in Hunter mode after
        CMP #$08        ;   level 9
        BCC POPTER      ;   ,,
        JSR HUNT        ;   ,,
POPTER: LDA #$07        ; The location terminal is not used
        CMP GLEVEL      ;   after level 9
        BCC POPTUR
        LDY #$01        ; Populate the location terminal
        LDX #CH_TER     ; ,,
        JSR POPULA      ; ,,
POPTUR: LDA GLEVEL      ; Populate some turtles
        ASL             ; ,,
        ADC #$02        ; ,,
        CMP #MAXTUR     ; ,, With a limit
        BCC PPT1        ; ,,
        LDA #MAXTUR     ; ,,
PPT1:   TAY             ; ,,
        STY TURTLS      ; ,,
        LDX #CH_TUR     ; ,,
        JSR POPULA      ; ,,
        LDA GLEVEL      ; Populate some patrols
        CMP #MAXPAT-1   ; ,, With a limit
        BCC POPPAT      ; ,,
        LDA #MAXPAT-1   ; ,,
POPPAT: TAY             ; ,,
        INY             ; ,,
        LDX #CH_PAL     ; ,,
        JSR POPULA      ; ,,
        LDX #CH_HLT     ; Populate a couple health boosts
        LDY #$02        ; ,,
        JSR POPULA      ; ,,
        STA MUCD        ; ,,
        LDA #$08        ; Initialize music tempo
        STA TEMPO       ; ,,
        LDA GLEVEL      ; Get level number for theme selection
        AND #$07        ; Limit to 8 musical themes
        JSR MUSIC       ; Select the theme
        JSR M_PLAY      ; Start the music
        JSR EXPLOR      ; Explore the top level
INIT_R: RTS 

; Setup Hardware
SETHW:  LDA TIME_L      ; Seed random number generator
        STA RNDNUM      ; ,,
        LDA #SCRCOM     ; Set background color
        STA BACKGD      ; ,,
        LDA #$FF        ; Set color register
        STA VICCR5      ; ,,
        LDA #$4F        ; Set volume and safecraft port color
        STA VOLUME      ; ,,
        LDA #$00        ; Initialize sound registers
        STA VOICEM      ; ,,
        STA VOICEH      ; ,,
        STA NOISE       ; ,,
        LDA #$7F        ; Set DDR to read East
        STA VIA2DD      ; ,,
        LDA #$80        ; Disabled Commodore-Shift
        STA CASECT      ; ,,
        JSR M_STOP      ; Turn off music playing
        LDA #TXTCOL     ; Set color of screen text, like
        STA TCOLOR      ;   intro, game over, score, etc.
        SEI             ; Install the custom ISR
        LDA #<CSTISR    ; ,,
        STA CINV        ; ,,
        LDA #>CSTISR    ; ,,
        STA CINV+1      ; ,,
        LDA #<WELCOM    ; Install the custom NMI (restart)
        STA NMINV       ; ,, 
        LDA #>WELCOM    ; ,,
        STA NMINV+1     ; ,,
        LDA #SPEED      ; Initialize frame countdown before
        STA FRCD        ;   ISR is started
        CLI
        RTS

; Populate Maze
;
; Preparations
;     X is the character
;     Y is the number of that character to put in the lair        
POPULA: TYA             ; X & Y are put on the stack for use
        PHA             ;   later in this routine, not because
        TXA             ;   they're expected to be preserved,
        PHA             ;   in case you wonder in the future
        LDA #$27        
        STA DATA_L
        LDA #>SCREEN
        STA DATA_H
        CPX #CH_TER     ; If the character is a computer
        BNE RNDY        ;   terminal, its Y position is not
        LDY GLEVEL      ;   random, but based on the current
        INY             ;   level
        BNE PL1         ;   ,,
RNDY:   LDA #$08        ; Get a random Y-axis
        JSR PSRAND
PL1:    LDA #$2C        ; Drop down 2Y lines in the lair
        JSR DATAAD
RY:     DEY
        BPL PL1
        LDA #$07        ; Get a random X-axis
        JSR PSRAND
PL2:    LDA #$03        ; Move over 3Y lines in the lair
        JSR DATAAD
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
        CPX #CH_PAL     ; If placing a patrol, add it
        BNE PLL0        ;   to the patrol data table
        PHA             ;   ,,
        TXA             ;   ,,
        PHA             ;   ,,
        JSR ADDPAT      ;   ,,
        PLA             ;   ,,
        TAX             ;   ,,
        PLA             ;   ,,
PLL0:   CLC             ; Hide all the populated objects
        JSR PLACE       ; Place the character in X, and
        PLA             ; Decrement the character number
        TAY             ;   counter. 
        DEY             ; Any more characters to place?
        BNE POPULA
        RTS

; Draw the safecraft, which is the home base of TRBo.       
SCRAFT: LDA #$3A        ; Starting character for the
        STA SCRPAD      ;   four-piece craft
        LDX #$03        ; Each character has an offset in
SSL0:   LDY SC_OFF,X    ;   the SC_OFF table, so iterate
        LDA SCRPAD      ;   through all four pieces and
        STA SCREEN,Y    ;   place them at their proper
        LDA #$0F        ;   offsets. Set a color for each
        STA COLOR,Y     ;   one as well
        INC SCRPAD      ;   ,,
        DEX             ;   ,,
        BPL SSL0        ;   ,,
        RTS
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GAME DATA MANAGEMENT SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Add Patrol
ADDPAT: LDA PATRLS
        ASL             ; Allocate eight bytes per patrol
        ASL             ; ,,
        ASL             ; ,,
        TAX             ; Real Table Index
        LDA DATA_L      ; Set location low
        STA PATROL,X    ; ,,
        LDA DATA_H      ; Set location high
        STA PATL_H,X    ; ,,
        LDA #$0A        ; Set recharge time to 10 frames
        STA PAT_BR,X    ; ,,
        LDA #LEFT       ; Set direction left
        STA PAT_DI,X    ; ,,
        LDA #UP         ; Set initial vertical travel upward
        STA PAT_TR,X    ; ,,
        LDA #$01        ; Set Bump flag
        STA PAT_BF,X    ; ,,
        LDA #CH_SPC     ; Set space under the patrol
        STA PAT_UN,X    ; ,,
        INC PATRLS
        RTS
  
; Set DATA Pointer from CURSOR
SDATA:  LDA CURSOR
        STA DATA_L
        LDA CUR_H 
        STA DATA_H
        RTS
       
; Reset CURSOR from DATA pointer 
RS_CUR: LDA DATA_L
        STA CURSOR
        LDA DATA_H
        STA CUR_H 
        RTS
        
; Set CURSOR from Player position
PLR2C:  LDA PLAYER
        STA CURSOR
        LDA PLR_H
        STA CUR_H 
        RTS
        
; Add the accumulator to DATA        
DATAAD: CLC
        ADC DATA_L
        STA DATA_L
        BCC DADD_R
        INC DATA_H
DADD_R: RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; UTILITY SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Random Number
; Gets a random number between 1 and 8. A contains the
; maximum value. The random number will be in Y.
PSRAND: STA SCRPAD
        TXA
        PHA
        DEC SCRPAD
AGAIN:  JSR BASRND
        LDA RNDNUM
        AND #$07
        CMP SCRPAD
        BCC E_RAND      ; Get another random number if this one
        BNE AGAIN       ; is greater than the maximum
E_RAND: TAY
        INY
        PLA
        TAX
        RTS
                        
; Delay
; Waits the number of jiffies specified in A
DELAY:  LDY #$00
        STY TIME_L
DWAIT:  CMP TIME_L
        BNE DWAIT
        RTS

; Wait for Fire
WAIT:   JSR READJS      ; Wait for fire to be released
        AND #$20        ; ,,
        BNE WAIT        ; ,,
WAIT_F: JSR READJS      ; Wait for the fire button
        AND #$20        ; ,,
        BEQ WAIT_F      ; ,,
        RTS
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GAME ASSET DATA AND TABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
INTRO:  .asc "TRBO?>TURTLE>RESCUEBOT"
        .asc "   BY>JASON>JUSTIAN",$0d,$0d
        .asc "     FIRE>TO>START",$00
        
ENDTXT: .asc $0d,$0d,$0d,"     MISSION>OVER",$00

HSTXT:  .asc "  HI?",$00

; Instructional manual text
MANTXT: .asc "TRBO?",$0d,$0d,$0d
        .asc $9e,"$",$05,">YOUR MISSION?",$0d,$0d
        .asc $1e,"!",$05,">LEAD BABY TURTLES",$0d,$0d
        .asc $9e,$5b,$05,">TO SAFECRAFT",$0d,$0d
        .asc $9f,"(",$05,">AVOID PATROLS",$0d,$0d
        .asc $5f,">TERMINALS GIVE INTEL",$0d
        .asc $1f,".",$05,">GEARS FIX DAMAGE",$0d,$0d
        .asc "  POINT @ FIRE TO",$0d
        .asc "  DIG? LOSE A ",$1f,".",$05,$0d,$0d,$0d
        .asc ">AGENT ANZU",$00
        
; Partial color map for some characters indexed from $1C
COLMAP: .byte $02,$02,$01,$01,$00,$05,$05,$05
        .byte $07,$07,$07,$03,$03,$03,$0F,$04
        .byte $09,$02,$06,$02
        
; Safecraft part offsets        
SC_OFF: .byte $59,$58,$42,$43      

; Curated musical themes for the shift register player.  
THEMES: .word $5412
        .word $2ab3
        .word $1113
        .word $4214
        .word $6446
        .word $c633
        .word $2919
        .word $3223
        .word $3333     ; Low health alarm tone
        .word $2fff     ; Turtles are gone alarm done

; Sound effects for the sound effects player
; Each effect has three parameters
;   (1) First byte is the starting shift register value
;   (2) High nybble of second byte is the length in jiffies x 16
;   (3) Low nybble of second byte is refresh rate in jiffies
FXTYPE: .byte $2f,$34                       ; Start the Game
        .byte $b8,$12                       ; Fire Sound
        .byte $03,$24                       ; Turtle rescue
        .byte $d1,$45                       ; Terminal Activated
        .byte $4a,$41                       ; Dig
        .byte $31,$21                       ; Damaged
        .byte $2f,$12                       ; Bonus
        .byte $2f,$35                       ; Found Health

; The character set must start at $1C00. If you change anything
; anywhere, you must account for this. The easiest way is to use
; padding bytes immediately before this character data.
;
; The easiest way to tell if you've done this right is to
; make sure that the object code is exactly 3583 bytes. This is
; a reliable method as long as you don't add anything AFTER this
; character data.
;
CHDATA: .byte $f8,$80,$84,$fe,$c4,$c4,$fc,$00 ; &
        .byte $fe,$02,$02,$fe,$86,$86,$fe,$00 ; A
        .byte $fc,$84,$84,$fe,$c2,$c2,$fe,$00 ; B
        .byte $fe,$80,$80,$c0,$c0,$c0,$fe,$00 ; C
        .byte $fc,$84,$84,$c6,$c2,$c2,$fe,$00 ; D
        .byte $fe,$80,$80,$f8,$c0,$c0,$fe,$00 ; E
        .byte $fe,$c0,$c0,$f8,$80,$80,$80,$00 ; F
        .byte $fe,$c0,$c0,$ce,$82,$82,$fe,$00 ; G
        .byte $82,$82,$82,$fe,$c2,$c2,$c2,$00 ; H
        .byte $10,$10,$10,$18,$18,$18,$18,$00 ; I
        .byte $3e,$02,$02,$c2,$c2,$c2,$fe,$00 ; J
        .byte $88,$88,$88,$fe,$c2,$c2,$c2,$00 ; K
        .byte $80,$80,$80,$c0,$c0,$c0,$fe,$00 ; L
        .byte $fe,$92,$92,$92,$c2,$c2,$c2,$00 ; M
        .byte $fe,$c2,$c2,$c2,$82,$82,$82,$00 ; N
        .byte $00,$00,$fe,$86,$86,$86,$fe,$00 ; O
        .byte $fe,$c2,$c2,$fe,$80,$80,$80,$00 ; P
        .byte $fe,$c2,$c2,$c2,$8e,$88,$fa,$00 ; Q
        .byte $fe,$82,$82,$fe,$d8,$cc,$c6,$00 ; R
        .byte $fe,$80,$80,$fe,$06,$06,$fe,$00 ; S
        .byte $fe,$18,$18,$18,$10,$10,$10,$00 ; T
        .byte $c2,$c2,$c2,$82,$82,$82,$fe,$00 ; U
        .byte $c2,$c2,$c2,$c6,$44,$28,$38,$00 ; V
        .byte $86,$86,$86,$92,$92,$92,$fe,$00 ; W
        .byte $c2,$c2,$ee,$38,$ee,$82,$82,$00 ; X
        .byte $82,$82,$82,$fe,$06,$06,$fe,$00 ; Y
        .byte $fe,$02,$0e,$38,$e0,$8e,$fe,$00 ; Z
        .byte $00,$18,$7e,$99,$7e,$18,$24,$42 ; SC Safecraft
        .byte $83,$c0,$80,$c3,$21,$11,$c3,$00 ; Broken Wall V
        .byte $ef,$c4,$08,$00,$02,$11,$dd,$00 ; Broken Wall H
        .byte $00,$00,$00,$08,$d2,$d7,$f7,$da ; Destroyed TRBo
        .byte $00,$00,$ff,$c3,$ff,$3c,$c3,$c3 ; SC Terminal
        .byte $00,$00,$00,$00,$00,$00,$00,$00 ; Space
        .byte $00,$00,$30,$7b,$7b,$fc,$48,$6c ; Turtle R
        .byte $00,$00,$0c,$de,$de,$3f,$12,$36 ; Turtle L
        .byte $00,$18,$5a,$42,$3c,$3c,$5a,$81 ; Turtle C
        .byte $0f,$0d,$07,$3c,$42,$99,$3c,$18 ; TRBo R
        .byte $f0,$b0,$e0,$3c,$42,$99,$3c,$18 ; TRBo L
        .byte $3c,$3c,$18,$3c,$42,$bd,$24,$24 ; TRBo C
        .byte $40,$3c,$37,$3c,$3c,$00,$66,$66 ; Patrol R
        .byte $02,$3c,$ec,$3c,$3c,$00,$66,$66 ; Patrol L
        .byte $04,$18,$7e,$7e,$3c,$00,$7e,$66 ; Patrol C
        .byte $00,$30,$cc,$c0,$03,$33,$0c,$00 ; Beam
        .byte $24,$3c,$24,$24,$24,$3c,$24,$24 ; Ladder
        .byte $00,$00,$aa,$be,$aa,$28,$82,$82 ; MC Terminal
        .byte $ff,$cc,$88,$ff,$33,$22,$ff,$00 ; Wall
        .byte $10,$54,$38,$c6,$38,$54,$10,$00 ; Health
        .byte $ff,$cc,$88,$ff,$33,$22,$ff,$00 ; False Wall
        .byte $fe,$86,$86,$86,$82,$82,$fe,$00 ; 0
        .byte $06,$06,$06,$06,$02,$02,$02,$00 ; 1
        .byte $fe,$06,$06,$fe,$80,$80,$fe,$00 ; 2
        .byte $fe,$02,$02,$fe,$06,$06,$fe,$00 ; 3
        .byte $86,$86,$86,$fe,$02,$02,$02,$00 ; 4
        .byte $fe,$80,$80,$fe,$06,$06,$fe,$00 ; 5
        .byte $fe,$80,$80,$fe,$c2,$c2,$fe,$00 ; 6
        .byte $fe,$06,$06,$06,$02,$02,$02,$00 ; 7
        .byte $fe,$82,$82,$fe,$86,$86,$fe,$00 ; 8
        .byte $fe,$86,$86,$fe,$02,$02,$02,$00 ; 9
        .byte $00,$00,$00,$00,$00,$80,$80,$a8 ; Safecraft 1
        .byte $00,$00,$00,$00,$00,$00,$00,$0a ; Safecraft 2
        .byte $2b,$2f,$0b,$02,$00,$00,$02,$08 ; Safecraft 3
        .byte $ba,$be,$b8,$a0,$80,$80,$20,$08 ; Safecraft 4
        .byte $00,$00,$00,$00,$00,$7c,$70,$00 ; Text Space
        .byte $00,$00,$20,$30,$00,$20,$30,$00 ; Colon
        