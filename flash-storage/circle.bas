'============================================================
' Circle-One (adaptado a Spotpear Pico 1.54" + Gamepad 12 botones)
' Base: Circle-One for Picomite LCD (c) 2023 @Volhout
' Adaptado por Marcos LM 2025 - WIP
'
' Requisitos:
'     OPTION LCDPANEL ST7789, LANDSCAPE, GP8, GP12, GP9, GP7
'     OPTION SYSTEM SPI GP10,GP11,GP28
'
' Controles (LCD/gamepad):
' - D-Pad: mover
' - A: sprint
' - B: salir
'============================================================

' padlib-min
' Botones accion (pines fisicos)
CONST PAD_PIN_X      = 4    ' GP2
CONST PAD_PIN_A      = 5    ' GP3
CONST PAD_PIN_Y      = 6    ' GP4
CONST PAD_PIN_B      = 9    ' GP6
' D-Pad
CONST PAD_PIN_LEFT   = 17   ' GP13
CONST PAD_PIN_DOWN   = 19   ' GP14
CONST PAD_PIN_UP     = 21   ' GP16
CONST PAD_PIN_RIGHT  = 22   ' GP17
' Gatillos
CONST PAD_PIN_L      = 24   ' GP18
CONST PAD_PIN_R      = 2    ' GP1
' Start/Select
CONST PAD_PIN_SELECT = 31   ' GP26
CONST PAD_PIN_START  = 32   ' GP27
' Bits del estado (mask)
CONST PAD_UP     = &H0001
CONST PAD_DOWN   = &H0002
CONST PAD_LEFT   = &H0004
CONST PAD_RIGHT  = &H0008
CONST PAD_A      = &H0010
CONST PAD_B      = &H0020
CONST PAD_X      = &H0040
CONST PAD_Y      = &H0080
CONST PAD_L      = &H0100
CONST PAD_R      = &H0200
CONST PAD_SELECT = &H0400
CONST PAD_START  = &H0800

DIM INTEGER gPadInited%

'------------------------------ setup PicoMite ---------------------------

' init del pad
PadInit

CLS

'initial values for the players and the target food
'target (green) index 0, player (keyboard - blue) index 1
'right player (AI - red) index 2
sz=MM.HRES/40

'x,y are coordinates, r=radius, c=color, s=speed
x0=MM.HRES/2:y0=MM.VRES/3:r0=sz:c0=RGB(GREEN)
x1=MM.HRES/3:y1=2*MM.VRES/3:r1=sz:c1=RGB(BLUE):   s1=5  'player speed, tweak
x2=2*MM.HRES/3:y2=2*MM.VRES/3:r2=sz:c2=RGB(RED):  s2=3  'AI speed, tweak
cw=RGB(WHITE)

'intro and start score reset
introscreen
u1=0:u2=0  'scores player and AI

'the game uses framebuffer to prevent screen drawing artefacts
FRAMEBUFFER CREATE
FRAMEBUFFER WRITE f

'initial target
food
counter=0
game_end=0

'---------------------- this is the main game loop --------------------------

DO

  'read keyboard / gamepad
  now% = PadRead%()

  ' Mantener direccion si no se pulsa nada (como el original)
  d%=0
  IF (now% AND PAD_RIGHT) THEN d% = d% OR 1
  IF (now% AND PAD_LEFT)  THEN d% = d% OR 2
  IF (now% AND PAD_UP)    THEN d% = d% OR 8
  IF (now% AND PAD_DOWN)  THEN d% = d% OR 4
  IF d%<>0 THEN p1=d%

  IF (now% AND PAD_A) THEN s1=12          'sprint
  IF (now% AND PAD_B) THEN game_end=1     'salir

  IF s1>5 THEN s1=s1-1

  'read AI
  moveAI

  'wipe old positions players
  eraseplayers

  'move players
  v1=0:v2=0:dx1=0:dx2=0:dy1=0:dy2=0
  IF (p1 AND 2) THEN INC v1,1:INC x1,-s1:dx1=-1
  IF (p1 AND 1) THEN INC v1,1:INC x1,s1:dx1=1
  IF (p1 AND 8) THEN INC v1,1:INC y1,-s1:dy1=-1
  IF (p1 AND 4) THEN INC v1,1:INC y1,s1:dy1=1
  IF (p2 AND 2) THEN INC v2,1:INC x2,-s2:dx2=-1
  IF (p2 AND 1) THEN INC v2,1:INC x2,s2:dx2=1
  IF (p2 AND 8) THEN INC v2,1:INC y2,-s2:dy2=-1
  IF (p2 AND 4) THEN INC v2,1:INC y2,s2:dy2=1

  'check collision with borders
  IF x1<r1 THEN x1=r1
  IF x1>MM.HRES-r1 THEN x1=MM.HRES-r1
  IF y1<r1 THEN y1=r1
  IF y1>MM.VRES-r1 THEN y1=MM.VRES-r1

  IF x2<r2 THEN x2=r2
  IF x2>MM.HRES-r2 THEN x2=MM.HRES-r2
  IF y2<r2 THEN y2=r2
  IF y2>MM.VRES-r2 THEN y2=MM.VRES-r2

  'collision between player 1 and player 2
  IF (x1-x2)^2 + (y1-y2)^2 < (r1+r2)^2 THEN
    TEXT MM.HRES/2,MM.VRES/2,"Collision !!","CM",1,1,RGB(YELLOW)
    prestart
  ENDIF

  'eat food?
  IF (x1-x0)^2 + (y1-y0)^2 < (r1+r0)^2 THEN
    INC r1,2
    newfood
  ENDIF
  IF (x2-x0)^2 + (y2-y0)^2 < (r2+r0)^2 THEN
    INC r2,2
    newfood
  ENDIF

  'draw new positions
  drawplayers

  'win conditions
  IF r1>MM.VRES/2 THEN
    TEXT MM.HRES/2,MM.VRES/2,"Blue Wins","CM",1,1,RGB(YELLOW)
    u1=u1+1
    prestart
  ENDIF
  IF r2>MM.VRES/2 THEN
    TEXT MM.HRES/2,MM.VRES/2,"Red Wins","CM",1,1,RGB(YELLOW)
    u2=u2+1
    prestart
  ENDIF

  'score update (ajuste para no ir tan al borde)
  TEXT 0,0,STR$(u1),"LT",1,2,RGB(BLUE)
  TEXT MM.HRES-1,0,STR$(u2),"RT",1,2,RGB(RED)

  'update screen
  FRAMEBUFFER COPY f,n,b
  PAUSE 50

LOOP UNTIL game_end=1

Salir
'End

'------------------------------- subs ------------------------------

SUB moveAI
  AIx=INT((x0-x2)/2)
  AIy=INT((y0-y2)/2)
  p2=0
  'decide preferred direction
  IF ABS(AIx)>=ABS(AIy) THEN
    'X is preferred direction
    movAIx
  ELSE
    'Y is preferred direction
    movAIy
  ENDIF
END SUB

SUB movAIx
  IF AIx>0 THEN p2=1 ELSE p2=2
  IF INT(RND*10)=0 THEN
    IF AIy>0 THEN p2=4 ELSE p2=8
  ENDIF
END SUB

SUB movAIy
  IF AIy>0 THEN p2=4 ELSE p2=8
  IF INT(RND*10)=0 THEN
    IF AIx>0 THEN p2=1 ELSE p2=2
  ENDIF
END SUB

SUB prestart
  TEXT MM.HRES/2,30+MM.VRES/2,"A=continue, B=stop","CM",1,1,RGB(YELLOW)
  FRAMEBUFFER COPY f,n,b
  DO
    PAUSE 50
    now% = PadRead%()
  LOOP WHILE (now% AND (PAD_A OR PAD_B)) = 0
  IF (now% AND PAD_B) THEN Salir

  CLS
  'speed is increased every level...so prepare.....
  x0=MM.HRES/2:y0=MM.VRES/3:r0=sz:c0=RGB(GREEN)
  x1=MM.HRES/3:y1=2*MM.VRES/3:r1=sz:c1=RGB(BLUE):s1=5+(u1+u2)/4
  x2=2*MM.HRES/3:y2=2*MM.VRES/3:r2=sz:c2=RGB(RED):s2=1+(u1+u2)/4

  food
END SUB

SUB newfood
  'seed new green circle
  x0=INT(RND*(MM.HRES-2*r0))+r0
  y0=INT(RND*(MM.VRES-2*r0))+r0
  food
END SUB

SUB introscreen
  CLS
  TEXT MM.HRES/2,MM.VRES/2-50,"CIRCLE ONE","CM",1,2,RGB(YELLOW)
  PAUSE 2000
  TEXT MM.HRES/2,MM.VRES/2,"a single player game","CM",1,1,RGB(YELLOW)
  PAUSE 1500
  TEXT MM.HRES/2,MM.VRES/2+40,"D-Pad to control","CM",1,1,RGB(YELLOW)
  PAUSE 1500
  BOX 0,MM.VRES/2-10,MM.HRES,MM.VRES,,0,0
  PAUSE 1500
  TEXT MM.HRES/2,MM.VRES/2,"A to sprint, B to stop","CM",1,1,RGB(YELLOW)
  PAUSE 1500
  TEXT MM.HRES/2,MM.VRES/2+20,"eat apples to grow and win","CM",1,1,RGB(YELLOW)
  PAUSE 1500
  TEXT MM.HRES/2,MM.VRES/2+40,"avoid collisions !!","CM",1,1,RGB(YELLOW)
  PAUSE 1500
  TEXT MM.HRES/2,MM.VRES/2+60,"--- GET READY ---","CM",1,1,RGB(YELLOW)
  PAUSE 2000
END SUB

SUB food
  CIRCLE x0-4,y0-2,r0,,,c0,c0
  CIRCLE x0+4,y0,r0,,,c0,c0
  LINE x0-3,y0,x0+5,y0-2*r0,1,c0
END SUB

SUB eraseplayers
  CIRCLE x1,y1,r1+10,,,0,0
  CIRCLE x2,y2,r2+10,,,0,0
END SUB

SUB drawplayers
  INC counter,1

  'draw player
  IF v1>0 THEN
    CIRCLE x1,y1,r1,,,c1,c1
    'eyes based on movement direction
    IF dx1=1 THEN
      CIRCLE x1+6,y1-2,5,,,cw,cw
      CIRCLE x1+6,y1+2,5,,,cw,cw
      CIRCLE x1+9,y1-2,2,,,c1,c1
      CIRCLE x1+9,y1+2,2,,,c1,c1
    ELSEIF dx1=-1 THEN
      CIRCLE x1-6,y1-2,5,,,cw,cw
      CIRCLE x1-6,y1+2,5,,,cw,cw
      CIRCLE x1-9,y1-2,2,,,c1,c1
      CIRCLE x1-9,y1+2,2,,,c1,c1
    ELSEIF dy1=1 THEN
      CIRCLE x1-6,y1+2,5,,,cw,cw
      CIRCLE x1+6,y1+2,5,,,cw,cw
      CIRCLE x1-6,y1+5,2,,,c1,c1
      CIRCLE x1+6,y1+5,2,,,c1,c1
    ELSEIF dy1=-1 THEN
      CIRCLE x1-6,y1-2,5,,,cw,cw
      CIRCLE x1+6,y1-2,5,,,cw,cw
      CIRCLE x1-6,y1-5,2,,,c1,c1
      CIRCLE x1+6,y1-5,2,,,c1,c1
    ENDIF
  ELSE
    'not moving, sleepy eyes
    CIRCLE x1+6,y1+2,5,,,cw,cw
    CIRCLE x1-6,y1+2,5,,,cw,cw
    CIRCLE x1+6,y1-1,5,,,c1,c1
    IF counter+14 AND 30 THEN
      CIRCLE x1-6,y1-1,5,,,c1,c1
    ELSE
      CIRCLE x1-6,y1+4,2,,,0,0
    ENDIF
  ENDIF

  'draw player 2 (AI)
  IF v2>0 THEN
    CIRCLE x2,y2,r2,,,c2,c2
    IF dx2=1 THEN
      CIRCLE x2+6,y2-2,5,,,cw,cw
      CIRCLE x2+6,y2+2,5,,,cw,cw
      CIRCLE x2+9,y2-2,2,,,c2,c2
      CIRCLE x2+9,y2+2,2,,,c2,c2
    ELSEIF dx2=-1 THEN
      CIRCLE x2-6,y2-2,5,,,cw,cw
      CIRCLE x2-6,y2+2,5,,,cw,cw
      CIRCLE x2-9,y2-2,2,,,c2,c2
      CIRCLE x2-9,y2+2,2,,,c2,c2
    ELSEIF dy2=1 THEN
      CIRCLE x2-6,y2+2,5,,,cw,cw
      CIRCLE x2+6,y2+2,5,,,cw,cw
      CIRCLE x2-6,y2+5,2,,,c2,c2
      CIRCLE x2+6,y2+5,2,,,c2,c2
    ELSEIF dy2=-1 THEN
      CIRCLE x2-6,y2-2,5,,,cw,cw
      CIRCLE x2+6,y2-2,5,,,cw,cw
      CIRCLE x2-6,y2-5,2,,,c2,c2
      CIRCLE x2+6,y2-5,2,,,c2,c2
    ENDIF
  ELSE
    CIRCLE x2,y2,r2,,,c2,c2
    CIRCLE x2+6,y2+2,5,,,cw,cw
    CIRCLE x2-6,y2+2,5,,,cw,cw
    CIRCLE x2+6,y2-1,5,,,c2,c2
    IF counter+14 AND 30 THEN
      CIRCLE x2-6,y2-1,5,,,c2,c2
    ELSE
      CIRCLE x2-6,y2+4,2,,,0,0
    ENDIF
  ENDIF
END SUB

SUB Salir
  CLS
  TEXT MM.HRES/2, MM.VRES/2, "Bye","CM",1,2,RGB(YELLOW)
  FRAMEBUFFER COPY f,n,b
  FRAMEBUFFER CLOSE
  RUN "A:/menu.bas"
END SUB

'============================================================
' PADLIB_MIN.BAS
'============================================================
SUB PadInit
  IF gPadInited% THEN EXIT SUB

  SETPIN PAD_PIN_X, DIN, PULLUP
  SETPIN PAD_PIN_A, DIN, PULLUP
  SETPIN PAD_PIN_Y, DIN, PULLUP
  SETPIN PAD_PIN_B, DIN, PULLUP

  SETPIN PAD_PIN_LEFT,  DIN, PULLUP
  SETPIN PAD_PIN_DOWN,  DIN, PULLUP
  SETPIN PAD_PIN_UP,    DIN, PULLUP
  SETPIN PAD_PIN_RIGHT, DIN, PULLUP

  SETPIN PAD_PIN_L, DIN, PULLUP
  SETPIN PAD_PIN_R, DIN, PULLUP

  SETPIN PAD_PIN_SELECT, DIN, PULLUP
  SETPIN PAD_PIN_START,  DIN, PULLUP

  gPadInited% = 1
END SUB

FUNCTION PadRead%()
  LOCAL INTEGER m%
  IF NOT gPadInited% THEN PadInit

  m% = 0
  IF PIN(PAD_PIN_UP)    = 0 THEN m% = m% OR PAD_UP
  IF PIN(PAD_PIN_DOWN)  = 0 THEN m% = m% OR PAD_DOWN
  IF PIN(PAD_PIN_LEFT)  = 0 THEN m% = m% OR PAD_LEFT
  IF PIN(PAD_PIN_RIGHT) = 0 THEN m% = m% OR PAD_RIGHT

  IF PIN(PAD_PIN_A) = 0 THEN m% = m% OR PAD_A
  IF PIN(PAD_PIN_B) = 0 THEN m% = m% OR PAD_B
  IF PIN(PAD_PIN_X) = 0 THEN m% = m% OR PAD_X
  IF PIN(PAD_PIN_Y) = 0 THEN m% = m% OR PAD_Y

  IF PIN(PAD_PIN_L) = 0 THEN m% = m% OR PAD_L
  IF PIN(PAD_PIN_R) = 0 THEN m% = m% OR PAD_R

  IF PIN(PAD_PIN_SELECT) = 0 THEN m% = m% OR PAD_SELECT
  IF PIN(PAD_PIN_START)  = 0 THEN m% = m% OR PAD_START

  PadRead% = m%
END FUNCTION
