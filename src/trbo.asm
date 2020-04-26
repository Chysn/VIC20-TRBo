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
NMI    = $FFFE          ; NMI vector (0318)
SCREEN = $1E00          ; Screen character memory (unexpanded)
COLOR  = $9600          ; Screen color memory (unexpanded)
SYSISR = $EABF          ; System ISR   
VICCR5 = $9005          ; Character map register
VOICEH = $900C          ; High sound register
VOICEM = $900B          ; Mid sound register
VOICEL = $900A          ; Low sound register (unused)
NOISE  = $900D          ; Noise register
VOLUME = $900E          ; Sound volume register
BACKGD = $900F          ; Background color
BASRND = $E094          ; Routine for BASIC's RND() function
RNDNUM = $8C            ; Result storage location for RND()
VIA1DD = $9113          ; Data direction register for joystick
VIA1PA = $9111          ; Joystick port (up, down, left, fire)
VIA2DD = $9122          ; Data direction register for joystick
VIA2PB = $9120          ; Joystick port (for right)
CLSR   = $E55F          ; Clear screen/home
HOME   = $E581          ; Home text
TCOLOR = $0286          ; Text color
PRTSTR = $CB1E          ; Print from data (Y,A)
CHROM0 = $8000          ; Character ROM
CHROM1 = $8100          ; Character ROM
CASECT = $0291          ; Disable Commodore case
PRTFIX = $DDCD          ; Decimal display routine
CHROUT = $FFD2          ; Output one character
TIMER  = $A2            ; Jiffy counter

; Constants
SCRCOM = $08            ; Maze color
TXTCOL = $01            ; Text color
SPEED  = $0E            ; Game speed, in jiffies of delay
CHRAM0 = $1C00          ; Custom Characters, Page 0
CHRAM1 = $1D00          ; Custom Characters, Page 1
; These are constants used for direction blocking
UP     = $01            ; Block - Up
RIGHT  = $02            ; Block - Right
DOWN   = $04            ; Block - Down
LEFT   = $08            ; Block - Left
; These contants are point values
PT_RES = $64            ; Rescuing a turtle 100 pts
PT_TER = $FA            ; Finding Terminal  250 pts
PT_BON = $32            ; Bonus Health       50 pts
PT_HLT = $19            ; Found Health       25 pts

; Sound Effects Library
FX_STA = $00            ; Start the game
FX_FIR = $01            ; Beam sound
FX_RES = $02            ; Rescue sound
FX_TER = $03            ; Computer terminal activated
FX_DIG = $04            ; Dig sound
FX_DMG = $05            ; The player was damaged
FX_BON = $06            ; Bonus points!
FX_HLT = $07            ; Found health

; Characters
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
CH_LEV = $3E            ; Level Number (greater)
                  
; Music Player                  
PATTRN = $033C          ; \ Music shift register pattern
PAT_H  = $033D          ; /
TEMPO  = $033E          ; Tempo (lower numbers are faster)
MUCD   = $033F          ; Tempo countdown
PLAY   = $0340          ; Music is playing
FADE   = $0341          ; Fadeout volume

; Sound Effects Player
REG_FX = $034E          ; Sound effects register
FXLEN  = $034F          ; Sound effects length
FXCD   = $0350          ; Sound effects countdown
FXCDRS = $0351          ; Countdown reset value

; Maze Builder
FRCD   = $05            ; Frame countdown
REMAIN = $0344          ; Remaining cells for the current level

; Game Play
GLEVEL = $0345          ; Game level
SCORE  = $0346          ; \ Player score
SCOR_H = $0347          ; /
UNDER  = $0348          ; Character underneath player
JOYDIR = $0349          ; Joystick direction capture
DIRBLK = $034A          ; Block direction (UP,RIGHT, RIGHT)
TURTLS = $034B          ; Turtle count for the level
PATRLS = $034C          ; Patrol count for the level
HUNTER = $0352          ; Hunters will attack turtles
FIRED  = $0353          ; Any patrol fired this round
DIDFIR = $0354          ; Current patrol fired this round
HEALTH = $0355          ; Player health
LOSDIR = $0356          ; Line-of-sight direction
TABIDX = $0357          ; Current patrol's real table index
PLAYER = $01            ; \ Player screen position (play)
PLR_H  = $02            ; /
CURSOR = $03            ; \ CURSOR direction
CUR_H  = $04            ; /

; Start of Patrol Table
; Each entry in the Patrol Table contains eight bytes. Six
; of these are used. From the start of each entry, these are:
PATROL = $03C0          ; Patrol location low byte
PATL_H = $03C1          ; Patrol location high byte
PAT_BR = $03C2          ; Beam refresh (0 if ready to fire)
PAT_DI = $03C3          ; Direction (UP, RIGHT, DOWN, LEFT)
PAT_TR = $03C4          ; Vertical travel (UP, DOWN)
PAT_UN = $03C5          ; Character under (CH_SPC, CH_LAD)
PAT_BF = $03C6          ; Bump flag
PAT_LL = $03C7          ; Ladder exit location low byte
PAT_LH = $03C8          ; Ladder exit location high byte

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
        
; Welcome Screen
WELCOM: JSR CLSR        ; Clear screen
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
        STA SCORE
        STA SCOR_H
        STA GLEVEL
        STA FADE
        JSR SOUND
        LDA #$08
        STA HEALTH

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
        LDA TURTLS      ; Has the level been completed?
        BEQ LVLUP
        LDA HEALTH      ; Is the player still alive?
        BEQ GAMOVR
        JMP MAIN

; Level Up
LVLUP:  LDA #$0F        ; Fade out the music
        STA FADE        ; ..
        STA TIMER       ; Set the jiffy counter
        LDA #$AF
DELAY:  CMP TIMER
        BNE DELAY
        LDA #$0E
        STA DATA_L      ; Store the #$0E from above
        LDA #>COLOR
        STA DATA_H
        LDA #$08
        STA VOLUME
        JSR M_STOP
        LDA HEALTH      ; Store health for bonus countdown
        PHA
BONUS:  LDA FX_BON
        JSR SOUND       ; Play the bonus sound
        LDA #PT_BON
        JSR USCORE
        LDA #$01
        LDY #$00
        STA (DATA_L),Y
        INC DATA_L
        STY TIMER
        LDA #$10
BDEL:   CMP TIMER
        BNE BDEL
        DEC HEALTH
        BNE BONUS
        PLA             ; Restore health after bonus
        STA HEALTH
        INC GLEVEL      ; Advance the level
        JMP STLEV

; Game Over, Juggalos and Juggalettes!        
GAMOVR: JSR REVEAL      ; Reveal the board
        LDA #CH_PLR     ; Show the player
        LDY #$00        ; ..
        STA (PLAYER),Y  ; ..
        LDA #$06
BLUE:   STA COLOR,Y
        STA COLOR+$0100,Y
        INY
        BNE BLUE
        JSR HOME        ; ..
        LDA #<ENDTXT    ; Show the intro screen
        LDY #>ENDTXT    ; ..
        JSR PRTSTR      ; ..
        LDA #$00
        JSR USCORE      ; Show the final score
        LDA #$10
        STA TEMPO       ; Slow the music down
        STA FADE        ; Fade out music
        JMP WAIT
        
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
PL_MV:  LDY #$00        ; Initialize the direction block; it
        STY DIRBLK      ;   tells which direction was chosen,
                        ;   and also which way we shouldn't
                        ;   look for turtles in the turtle
                        ;   chain
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
JY_F:   LDA #$20        ; Handle fire
        BIT JOYDIR
        BEQ DOMOVE
        LDA DIRBLK      ; Can't dig a hole unless choosing
        BEQ MV_R        ;   a direction
        JSR DIG         ; Dig a hole at the cost of damage
DOMOVE: JSR ISBLOC      ; Is the CURSOR space open?
        BEQ MV_R        ;   If not, don't move
        TXA
        PHA             ; Save the player character
        LDX UNDER       ; Restore the previous character
        LDA PLAYER      ; ..
        LDY PLR_H       ; ..
        SEC             ; ..
        JSR PLACE       ; ..
        
        LDY #$00
        LDA (CURSOR),Y  ; Get the current character at CURSOR
        STA UNDER       ;   and save it for when we move away
ENCTER: CMP #CH_TER     ; Has the player encountered the
        BNE ENCHLT      ;   computer terminal?
        JSR FNDTER
        JMP PL_PL
ENCHLT: CMP #CH_HLT     ; Has the player encountered a health
        BNE PL_PL       ;   boost?
        JSR FNDHLT
PL_PL:  PLA             ; Place the player
        TAX             ; ..
        LDA CURSOR      ; ..
        LDY CUR_H       ; ..
        SEC             ; ..
        JSR PLACE       ; ..
        LDA PLR_H       ; Store the player position temporarily
        PHA             ;   so that we have the previous
        LDA PLAYER      ;   position after the update
        PHA
        LDA CURSOR      ; Update player position
        STA PLAYER
        LDA CUR_H 
        STA PLR_H
        PLA             ; Put the previous position into 
        STA CURSOR      ;   CURSOR so that we can look around
        PLA             ;   for a turtle chain
        STA CUR_H 
        LDY #$00        ; If there's a turtle in the previous
        LDA (CURSOR),Y  ;   position, then a turtle chain can't
        CMP #CH_TUR     ;   be started, or else that turle will
        BEQ MV_R        ;   be destroyed. Check for all three
        CMP #CH_TUL     ;   turtle characters.
        BEQ MV_R  
        CMP #CH_TUC
        BEQ MV_R
        JSR TURCHN      ; Recursively find turtles for a chain
MV_R:   JSR EXPLOR      ; Explore surroundings
        RTS
       
; NPC Move
; Move non-player characters (turtles and patrols)        
NPC_MV: LDA SCREEN+$5A  ; First, look for a turtle near the
        JSR IS_TUR      ;   spaceship. This turtle will be
        BNE MVPATS      ;   rescued. Rescue involves:
        LDA #CH_SPC     ;   (1) Removing the turtle
        STA SCREEN+$5A  ;   ..
        DEC TURTLS      ;   (2) Decrementing the turtle count
        LDA #FX_RES     ;   (3) Launching a rescue sound
        JSR SOUND       ;   ..
        LDA #PT_RES     ;   (4) Adding to the score
        JSR USCORE      ;   ..
        LDA #$5A        ; Set the CURSOR to pull in
        STA CURSOR      ;   additional turtles in the chain
        LDA #>SCREEN    ;   ..
        STA CUR_H       ;   ..
        LDA #LEFT
        STA DIRBLK
        JSR TURCHN
MVPATS: LDX PATRLS      ; Move each patrol
FORPAT: LDA #$01        ; The first patrol is an express to the
        STA PAT_BF      ;   top. It never requires a bump to go
        STA PAT_TR      ;   up, and it never leaves the surface
                        ;   once it gets there. This increases
                        ;   the chance of the surface being
                        ;   defended.
        DEX             ; Patrols are zero-indexed
        JSR PAT_AI      ; Call patrol AI routine
        CPX #$00
        BNE FORPAT
        RTS
        
; Turtle Chain! 
; Look around the position in CURSOR for a turtle. 
; If there's a turtle there, move it, then recursively call
; TURCHN to keep the chain going.
TURCHN: JSR SDATA       ; DATA will contain the original
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
LOOK_R: LDY DIRBLK
        CPY #RIGHT
        BEQ LOOK_D
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

; To move a turtle in the chain, a turtle will be placed in
; the DATA location, with the movement graphic, as set
; above in X. Then, the current position (DATA) needs to be 
; cleared out. It'll be cleared with either a space, or a 
; ladder, depending on the current graphic at the DATA 
; location.  Then, recursively call TURCH to continue the
; chain.
MOVETU: LDY #$00
        LDA (CURSOR),Y  ; What turtle is there now?
        PHA
        LDA (DATA_L),Y  ; Where the turtle is going?
        CMP #CH_LAD     ; To a ladder?
        BNE PL_TUR      ; If not, use the selected graphic
        LDX #CH_TUC     ; Otherwise, switch to the ladder turtle
PL_TUR: LDA DATA_L      ; Place the turtle in the DATA position
        LDY DATA_H      ; ...
        SEC             ; ...
        JSR PLACE       ; ...
        LDX #CH_SPC     ; Default to replacing with space
        PLA
        CMP #CH_TUC     ; But if the turtle is coming off a
        BNE PL_SL       ;   ladder, replace with a ladder
        LDX #CH_LAD
PL_SL : LDA CURSOR      ; Place the space or ladder
        LDY CUR_H       ; ...
        SEC             ; ...
        JSR PLACE       ; ...
        JMP TURCHN
CHN_R:  RTS
        
; Patrol AI
; The patrol movement works like this:
;  I)    If the player is in the patrol's line of sight,
;        and the beam is recharged, then fire. If the patrol
;        fired the beam, then END
;  (ref AI_I)
;
;  II)   If the patrol is on a ladder (Ladder flag set)
;        (A) If the patrol can leave the ladder
;            Set a Horiz direction, clear the Bump flag, and 
;            the Ladder flag. END
;        (B) If the patrol cannot leave the ladder, check
;            the Vertical flag. Check that direction.
;            (1) If that direction is open, MOVE there
;            (2) Otherwise, END
;  (ref AI_II)
;
;  III)  If the patrol is not on a ladder (Ladder flag clear);
;        (A) If the Vert direction is open
;            (1) If the Bump flag is set, set the Ladder flag.
;            MOVE in the Vert direction
;        (B) If the Horiz direction is open, MOVE in the
;            Horiz directon
;        (C) If the Horiz direction is not open, reverse (EOR)
;            the Horiz flag and set the Bump flag. END
;  ref (AI_III)
;  
; Preparations
;     X is the patrol index
PAT_AI: TXA
        ASL             ; Index eight bytes per patrol so that
        ASL             ;   X is the real table index
        ASL             ;   ..
        TAX             ;   ..
        STX TABIDX      ; Save the current real table index
        LDA PATROL,X    ; Set the CURSOR to the patrol's current
        STA CURSOR      ;   position
        LDA PATL_H,X    ;   ..
        STA CUR_H       ;   ..
AI_I:   LDA #$00        ; Reset whether this patrol has fired
        STA DIDFIR      ;   to check after line-of-sight (LOS)                        
        JSR LOS         ; Check line of sight
        JSR SDATA       ; Set DATA from CURSOR
        LDA DIDFIR      ; If the patrol fired during the check,
        BNE AI_R        ;   do nothing else
AI_II:  LDX TABIDX
        LDA PAT_DI,X    ; Get current direction
        AND #$05        ; UP + DOWN
        BEQ AI_III      ; If not on ladder, go to next step
        JSR ONLAD       ; If so, do the On Ladder routine
        JMP PAT_DR      ; Draw the patrol
AI_III: JSR OFFLAD      ; Do the Off Ladder routine
PAT_DR: LDY #CH_PAC     ; Determine the character based
        LDX TABIDX      ;   on the new direction
        LDA PAT_DI,X    ;
        AND #$05        ; Is the direction up or down?
        BNE GOTCHR      ; If so, climbing is already in Y
        LDY #CH_PAL
        LDA PAT_DI,X
        CMP #RIGHT
        BNE GOTCHR      ; If not, left is already in Y
        LDY #CH_PAR     ; Switch to right
GOTCHR: TYA             ; Stash the new character
        PHA             ; ..
        LDA PAT_UN,X    ; Get the character under the patrol
        TAX             ; Place the old character
        LDA DATA_L      ; ..
        LDY DATA_H      ; ..
        SEC             ; .. 
        JSR PLACE       ; ..
        LDY #$00
        LDA (CURSOR),Y  ; Current character at the destination
        JSR IS_COR      ; Update the patrol's UNDER entry only
        BNE UP_PAT      ;   if it is a wall or ladder
        LDX TABIDX      ;   ..
        STA PAT_UN,X    ;   ..
UP_PAT: LDA CURSOR      ; Update the patrol table entry
        STA PATROL,X    ; ..
        LDA CUR_H       ; ..
        STA PATL_H,X    ; ..
        PLA             ; Put the new character to draw in X
        TAX             ; Place the new patrol
        LDA CURSOR      ; ..
        LDY CUR_H       ; ..
        SEC             ; ..
        JSR PLACE       ; ..
AI_R:   LDA TABIDX      ; Restore X from Real Table Index back
        LSR             ;   to the patrol index, because the
        LSR             ;   caller uses X as an iterator
        LSR             ;   ..
        TAX             ;   ..
        RTS  

; Patrol On Ladder        
ONLAD:  JSR MCUR_L
        JSR OPEN2P      ; Is there a left corridor?
        BNE CL_R        ; No, then check right
        LDA #LEFT       ; Change direction to left
        JSR MVOFFL      ; Move off ladder
        RTS
CL_R:   JSR RS_CUR
        JSR MCUR_R
        JSR OPEN2P      ; Is there a right corridor?
        BNE CL_U
        LDA #RIGHT      ; Change direction to right
        JSR MVOFFL
        RTS
CL_U:   JSR RS_CUR
        LDA PAT_DI,X    ; Is the patrol on its way up?
        CMP #UP     
        BNE CL_D
        JSR MCUR_U
        JSR OPEN2P      ; Is up open?
        BNE ON_CR
        LDA CUR_H       ; If the patrol is at the top level,
        CMP #>SCREEN    ;   change its direction of travel
        BNE ON_R        ;   to Down
        LDA CURSOR      ;   ..
        CMP #$6E        ;   ..
        BCS ON_R        ;   ..
        LDA #DOWN       ;   ..
        STA PAT_TR,X    ;   ..
        BNE ON_R
CL_D:   JSR RS_CUR
        JSR MCUR_D      ; No need to check stuff here, because        
                        ;   it's the only option left
        JSR OPEN2P
        BNE STUCK
        BEQ ON_R
STUCK:  LDA #UP         ; Set the travel direction to Up to
        STA PAT_TR,X    ;   avoid getting the hell stuck
        STA PAT_DI,X    ;   ..
ON_CR:  JSR RS_CUR
ON_R:   RTS

; Patrol Off Ladder
OFFLAD: LDA PAT_TR,X    ; If the travel direction is Down, see
        CMP #DOWN       ;   if the patrol came off the ladder
        BNE CO_CHK      ;   at this location. If this is the
        LDA DATA_L      ;   case, give the patrol a chance
        CMP PAT_LL,X    ;   of changing its direction of
        BNE CO_CHK      ;   travel. This helps the patrol to
        LDA DATA_H      ;   avoid getting stuck in a room and
        CMP PAT_LH,X    ;   away from the action.
        BNE CO_CHK      ;   ..
        LDA TIMER       ;   .. 
        CMP #$30        ; The preference is to keep going down,
        BCC CO_CHK      ;   though, if possible
        LDA #UP         ; Set the travel direction to Up
        STA PAT_TR,X    ; ..
CO_CHK: LDA PAT_BF,X    ; If the Bump flag is unset, then don't
        BEQ CO_R        ;   check any vertical directions yet
        LDA PAT_TR,X    ; Is the Travel direction Down?
        CMP #DOWN
        BNE CO_U        ; No, then check Up
        JSR MCUR_D
        JSR OPEN2P      ; Can the patrol move down?
        BNE CO_R        ; If not open, then move left or right
        LDA #DOWN       ; Move onto the ladder going down
        STA PAT_DI,X    ; ..
        RTS
CO_U:   JSR RS_CUR
        JSR MCUR_U
        LDY #$00
        LDA (CURSOR),Y
        CMP #CH_LAD     ; Checking specifically for a ladder, so
        BNE CO_R        ;   patrol doesn't climb off the screen
        LDA #UP         ; Move onto the ladder going up
        STA PAT_DI,X    ; ..
        RTS
CO_R:   JSR RS_CUR
        LDA PAT_DI,X    ; Is the patrol moving right?
        CMP #RIGHT
        BNE CO_L
        JSR MCUR_R
        JSR OPEN2P      ; Can the patrol move right?
        BNE BUMP        ; Can't move this round
        RTS             ; Return to draw routine
CO_L:   JSR RS_CUR      ; No need to check direction flag here
        JSR MCUR_L
        JSR OPEN2P      ; Can the patrol move left?
        BNE BUMP
        RTS        
BUMP:   JSR RS_CUR      ; Cannot move; restore the CURSOR
        LDA #$01        ; Set the Bump flag
        STA PAT_BF,X    ; ..
        LDA #RIGHT      ; If the direction is not already
        CMP PAT_DI,X    ;   to the right, then set it
        BEQ D2LEFT
        STA PAT_DI,X
        RTS             ; Return to draw routine
D2LEFT: LDA #LEFT
        STA PAT_DI,X
        RTS             ; Return to draw routine
        
; Move Off Ladder
; Sets the direction to the one specifed in A. But, more
; importantly, this sets the bottom ladder location if the
; patrol has a downward travel. If the patrol reaches this
; point again, it will change its travel to upward most
; of the time.        
MVOFFL: PHA
        LDA PAT_DI,X
        CMP #DOWN
        BNE CH_DIR
        LDA DATA_L
        STA PAT_LL,X
        LDA DATA_H
        STA PAT_LH,X
CH_DIR  PLA
        STA PAT_DI,X
        LDA #$00        ; Clear Bump flag
        STA PAT_BF,X    ; ..
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
        CMP #CH_WAL
        BEQ OP_R
        CMP #CH_FWA     ; Also check for the secret "false wall"
        BEQ OP_R
        JSR IS_PAT
OP_R:   RTS
        
; Is Open to Patrol
; Patrols can move along corridors and ladders only.
; Anything else causes them to turn around. Zero flag
; is set if the CURSOR is open.
OPEN2P: LDY #$00
        LDA (CURSOR),Y
        JSR IS_COR
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
        SBC #CH_SPC     ; Get a color table index
        TAX             ;   and store it in X for later
        LDA DATA_H      ; Subtract the starting page of screen
        SEC             ;   memory from the specified page to
        SBC #>SCREEN    ;   get the screen page offset
        CLC
        ADC #>COLOR     ; Add that offset to color memory so
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
; Update the score and draw it on the screen
;
; Preparations
;     A is the amount to add to the current score
USCORE: CLC
        ADC SCORE
        STA SCORE
        BCC SCDRAW
        INC SCOR_H
SCDRAW: JSR HOME
        LDA #CH_LEV
        JSR CHROUT
        LDX GLEVEL
        INX
        LDA #$00
        JSR PRTFIX
        LDA #CH_SPC
        JSR CHROUT
        JSR CHROUT
        LDX SCORE
        LDA SCOR_H
        JSR PRTFIX
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
        JSR SPSHIP
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
; Usally a benefit of activating the terminal
REVEAL: LDX #$00
RL0:    TXA             ; Set up two character color calls...
        CMP #$6D
        BCC RP1         ; Ignore the top part of the screen
        PHA
        LDY #>SCREEN
        SEC
        JSR CHRCOL      ; One for the first page of screen,
        PLA
RP1:    PHA
        TXA
        LDY #>SCREEN+$0100
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
D_U:    CMP #$01
        BNE D_R
        JSR MCUR_U
        JMP DO_DIG
D_R:    CMP #$02
        BNE D_D
        JSR MCUR_R
        JMP DO_DIG
D_D     CMP #$04
        BNE D_L
        JSR MCUR_D
        JMP DO_DIG
D_L     JSR MCUR_L
DO_DIG: LDY #$00
        LDA (CURSOR),Y
        CMP #CH_WAL     ; Can only dig walls
        BNE DIG_R
        LDA #CH_SPC
        STA (CURSOR),Y
        LDA #FX_DIG     ; Launch the digging sound
        JSR SOUND       ; ..
        JSR DAMAGE      ; Take one point of damage
DIG_R:  PLA
        STA CURSOR
        PLA
        STA CUR_H 
        RTS     

; Found the Terminal        
FNDTER: LDA #CH_SPC
        STA UNDER       ; Goes away after use
        LDA #FX_TER
        JSR SOUND       ; Launch the terminal sound
        JSR REVEAL      ; Reveal the board
        LDA #$00
        STA HUNTER      ; Quiesce the patrols
        LDA #$08
        STA TEMPO
        LDA #PT_TER
        JSR USCORE
        RTS 

; Found a Health Boost
FNDHLT: LDA #CH_SPC
        STA UNDER       ; Goes away after use
        LDA HEALTH
        CMP #$08        ; Already maxed out
        BCS HLT_R       ; ..
        INC HEALTH      ; Increase and display
        JSR SHOWHL      ; ..
HLT_R:  LDA #FX_HLT     ; Launch the bonus sound
        JSR SOUND       ; ..
        LDA #PT_HLT
        JSR USCORE
        RTS
        
        
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
; Set a musical pattern 
;
; Preparations
;     A is the score index
MUSIC:  ASL             ; Multiply level by 2 for score index
        TAX
        LDA PATRNS,X    ; Set the musical pattern
        STA PATTRN
        LDA PATRNS+1,X
        STA PAT_H
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
        LDX #$00        ; X is the carry bit for PAT_H
        ASL PATTRN      ; Shift the low byte, which may set C
        ROL PAT_H       ; Rotate the high byte, including C
        BCC NROLL       ; Was the high bit of the high byte set?
        LDX #$01        ; If so, add it back to the beginning
NROLL:  TXA
        ORA PATTRN
        STA PATTRN
        ORA #$80
        STA VOICEM
        LDA FADE        ; Fade is a volume override. If fade is
        BEQ VOLREG      ;   set, it will decrease every note,
        DEC FADE        ;   and the music will stop when it
        BNE VOL         ;   reaches zero
        JMP M_STOP
VOLREG: LDA PAT_H       ; Set the music volume and flash
VOL:    STA VOLUME      ;   the windows of the spaceship
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
        CMP #$3D        ; Right corner of ship?
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
        SEC             ; ..
        SBC GLEVEL      ; ..
        STA PAT_BR,X  ; ..
        INC HUNTER      ; Activate Hunter mode
        INC FIRED       ; Fire happened
        LDA #$07
        STA TEMPO       ; Make the music faster
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
        LDA CURSOR      ; ..
        LDY CUR_H       ; ..
        SEC             ; ..
        JSR PLACE       ; ..
        PLA             ; Okay, what did we hit?
        JSR IS_COR      ; Is it a corridor?
        BEQ NXBEAM
HITTUR: JSR IS_TUR      ; Is it a turtle?
        BNE LOS_R
        DEC TURTLS      ; A turtle was killed; reduce the count
        JMP LOS_R       ; Anything else stops the beam
      
; Damage!
; The player or a turtle have taken a hit
; Falls through to DRAWPL
DAMAGE: LDA HEALTH
        BEQ DAMA_R
        DEC HEALTH
        JSR SHOWHL      ; Show health
        LDA FX_DMG
        JSR SOUND
DAMA_R: RTS

; Place Player        
PLPLR:  LDA PLAYER
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
MAZE:   LDY #$00
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
        LDY #$00        ;   prevent the player from digging from
FWAL:   JSR MCUR_D      ;   one side of the board to the other,
        LDA #CH_FWA     ;   which is something that should be
        STA (CURSOR),Y  ;   physically impossible.
        DEX             ;   ..
        BNE FWAL        ;   ..
        LDA #$5A        ; Offset for the maze
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
        ADC CURSOR
        STA CURSOR
        BCC F_COR
        INC CUR_H 
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

; Draw Corridor
; The starting cell of the corridor is 10 minus the number 
; of remaining cells.
;
; Preparations
;     X is the level number
;     Y is the length of the corridor
DRCORR: LDA CURSOR      ; Save the screen position
        PHA
        LDA CUR_H 
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
        STA (CURSOR),Y
        LDA CURSOR
        SEC
        SBC #$16        ; Go up one line
        STA CURSOR
        BCS CKNOCK
        DEC CUR_H 
CKNOCK: LDA #CH_LAD     ; Knock out ceiling with a ladder
        STA (CURSOR),Y  ; Knock out the ceiling
RESET:  PLA             ; Start restoring things for return
        TAY
        PLA
        STA CUR_H 
        PLA
        STA CURSOR
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
        CPX #$F8
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
        STA PLAYER      ;   of the maze.
        LDY #>SCREEN
        STY PLR_H
        JSR PLPLR       ; Place the player
        LDA #CH_SPC
        STA UNDER       ; Start with a space under player
        LDY #$01        ; Populate the location terminal
        LDX #CH_TER     ; ..
        JSR POPULA      ; ..
        LDA GLEVEL      ; Populate some turtles
        ASL             ; ..
        ADC #$02        ; ..
        AND #$0F        ; ..
        TAY             ; ..
        STY TURTLS      ; ..
        LDX #CH_TUR     ; ..
        JSR POPULA      ; ..
        LDA #$00        ; Initialize patrol data table
        STA PATRLS      ; ..
        LDA GLEVEL      ; Populate some patrols
        AND #$03        ; Limit to 4
        TAY             ; ..
        INY             ; ..
        LDX #CH_PAL     ; ..
        JSR POPULA      ; ..
        LDX #CH_HLT     ; Populate a couple health boosts
        LDY #$02        ; ..
        JSR POPULA      ; ..
        LDA #$08        ; Set the music tempo
        STA TEMPO       ; ..
        STA MUCD        ; ..
        LDA GLEVEL
        AND #$07        ; Limit to 8 musical patterns
        JSR MUSIC       ; Select the pattern
        LDA #$00
        STA HUNTER      ; Reset Hunter flag
        JSR USCORE      ; Display current score
        JSR SHOWHL      ; Display current health
        JSR M_PLAY      ; Start the music
        JSR EXPLOR      ; Explore the top level
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
        STA VOICEM      ; ..
        STA VOICEH      ; ..
        STA NOISE       ; ..
        LDA #$7F        ; Set DDR to read East
        STA VIA2DD      ; ..
        LDA TIMER       ; Set the random number seed
        STA RNDNUM      ; ..
        LDA #$80        ; Disabled Commodore-Shift
        STA CASECT      ; ..
        JSR M_STOP      ; Turn off music playing
        LDA #TXTCOL
        STA TCOLOR
        SEI             ; Install the custom ISR
        LDA #<CSTISR    ; ..
        STA ISR         ; ..
        LDA #>CSTISR    ; ..
        STA ISR+1       ; ..
        LDA #<WELCOM    ; Install the custom NMI (restart)
        STA NMI         ; .. 
        LDA #>WELCOM    ; ..
        STA NMI+1       ; ..
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
        LDA #>SCREEN
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

; Draw the f        
SPSHIP: LDA #$3A
        STA SCRPAD
        LDX #$03
SSL0:   LDY SHOFF,X
        LDA SCRPAD
        STA SCREEN,Y
        LDA #$0F
        STA COLOR,Y
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
        ASL             ; Allocate eight bytes per patrol
        ASL             ; ..
        ASL             ; ..
        TAX             ; Real Table Index
        LDA DATA_L      ; Set location low
        STA PATROL,X    ; ..
        LDA DATA_H      ; Set location high
        STA PATL_H,X    ; ..
        LDA #$0F        ; Set recharge time to 15 frames
        STA PAT_BR,X    ; ..
        LDA #LEFT       ; Set direction left
        STA PAT_DI,X    ; ..
        LDA #UP         ; Set initial vertical travel upward
        BIT PATRLS      ;   or downward, alternating
        BNE SET_TR      ;   ..
        LDA #DOWN       ;   ..
SET_TR: STA PAT_TR,X    ; ..
        LDA #$01        ; Set Bump flag
        STA PAT_BF,X    ; ..
        LDA #CH_SPC     ; Set space under the patrol
        STA PAT_UN,X    ; ..
        INC PATRLS
        PLA
        TAX
        PLA
        TAY
        PLA
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
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GAME ASSET DATA AND TABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
INTRO:  .asc "TRBO  TURTLE RESCUEBOT"
        .asc "   ? JASON JUSTIAN",$0d
        .asc " !  FIRE TO START   %",$00
        
ENDTXT: .asc $0d,$0d,"   ' MISSION OVER (   ",$00

; Custom character set        
CCHSET: .byte $00,$00,$30,$7b,$7b,$fc,$48,$6c ; Turtle R
        .byte $00,$00,$0c,$de,$de,$3f,$12,$36 ; Turtle L
        .byte $00,$18,$5a,$42,$3c,$3c,$5a,$81 ; Turtle C
        .byte $0f,$0d,$07,$3c,$42,$99,$3c,$18 ; Robot R
        .byte $f0,$b0,$e0,$3c,$42,$99,$3c,$18 ; Robot L
        .byte $3c,$3c,$18,$3c,$42,$bd,$24,$24 ; Robot C
        .byte $40,$3c,$37,$3c,$3c,$00,$66,$66 ; Patrol R
        .byte $02,$3c,$ec,$3c,$3c,$00,$66,$66 ; Patrol L
        .byte $04,$18,$7e,$7e,$3c,$00,$7e,$66 ; Patrol C
        .byte $00,$30,$cc,$c0,$03,$33,$0c,$00 ; Beam
        .byte $24,$3c,$24,$24,$24,$3c,$24,$24 ; Ladder
        .byte $00,$00,$aa,$be,$aa,$28,$82,$82 ; Terminal
        .byte $ff,$cc,$88,$ff,$33,$22,$ff,$00 ; Wall
        .byte $10,$54,$38,$c6,$38,$54,$10,$00 ; Health
        .byte $ff,$cc,$88,$ff,$33,$22,$ff,$00 ; False Wall
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
        .byte $00,$00,$00,$00,$00,$80,$80,$a8 ; Ship 1
        .byte $00,$00,$00,$00,$00,$00,$00,$0a ; Ship 2
        .byte $2b,$2f,$0b,$02,$00,$00,$02,$08 ; Ship 3
        .byte $ba,$be,$b8,$a0,$80,$80,$20,$08 ; Ship 4
        .byte $c0,$c0,$c0,$80,$80,$80,$f8,$00 ; Level#
        .byte $3c,$42,$99,$a1,$a1,$99,$42,$3c ; Copyright

; Color map for the above characters, indexed from SPACE
COLMAP: .byte $00,$05,$05,$05,$07,$07,$07,$03
        .byte $03,$03,$0F,$02,$09,$04,$06,$04

; Spaceship part offsets        
SHOFF:  .byte $59,$58,$42,$43      

; Curated musical patterns for the shift register player.  
PATRNS: .word $5412
        .word $2ab3
        .word $2fff
        .word $4214
        .word $1331
        .word $6446
        .word $c633
        .word $2919

; Sound effects for the sound effects player
; Each effect has three parameters
;   (1) First byte is the starting shift register value
;   (2) High nybble of second byte is the length in jiffies x 16
;   (3) Low nybble of second byte is refresh rate in jiffies
FXTYPE: .byte $2f,$34                       ; Start the Game
        .byte $55,$63                       ; Fire Sound
        .byte $03,$24                       ; Turtle rescue
        .byte $23,$62                       ; Terminal Activated
        .byte $4a,$41                       ; Dig
        .byte $fb,$72                       ; Damaged
        .byte $44,$1F                       ; Bonus
        .byte $2f,$64                       ; Found Health
        