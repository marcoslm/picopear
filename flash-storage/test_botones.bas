'============================================================
' Test de botones para placa SpotPear
' por Marcos LM 2025
'
' Requisitos:
'     OPTION LCDPANEL ST7789, LANDSCAPE, GP8, GP12, GP9, GP7
'     OPTION SYSTEM SPI GP10,GP11,GP28
'
'============================================================
OPTION EXPLICIT
OPTION DEFAULT INTEGER

' Pines fisicos
CONST PIN_X      = 4    ' GP2
CONST PIN_A      = 5    ' GP3
CONST PIN_Y      = 6    ' GP4
CONST PIN_B      = 9    ' GP6
CONST PIN_LEFT   = 17   ' GP13
CONST PIN_DOWN   = 19   ' GP14
CONST PIN_UP     = 21   ' GP16
CONST PIN_RIGHT  = 22   ' GP17
CONST PIN_L      = 24   ' GP18
CONST PIN_R      = 2    ' GP1
CONST PIN_SELECT = 31   ' GP26
CONST PIN_START  = 32   ' GP27

' --- Colores ---
CONST C_BG   = RGB(0,0,0)
CONST C_EDGE = RGB(80,80,80)
CONST C_OFF  = RGB(50,50,50)
CONST C_ON   = RGB(0,220,80)
CONST C_TXT  = RGB(255,255,255)
CONST C_SUB  = RGB(255,255,0)

SUB DrawBtnRound(x%, y%, r%, label$, pressed%)
  LOCAL INTEGER fillc%
  IF pressed% THEN fillc% = C_ON ELSE fillc% = C_OFF
  ' CIRCLE X,Y,R,LW,A,C,FILL  (A=aspect ratio, normalmente 1.0)
  CIRCLE x%, y%, r%, 2, 1.0, C_EDGE, fillc%
  TEXT x% - (LEN(label$) * 4), y% - 6, label$, "LT",, 1, C_TXT
END SUB

SUB DrawBtnRect(x%, y%, w%, h%, label$, pressed%)
  LOCAL INTEGER fillc%
  IF pressed% THEN fillc% = C_ON ELSE fillc% = RGB(30,30,30)
  BOX x%, y%, w%, h%, 2, C_EDGE, fillc%
  TEXT x% + 6, y% + (h%\2) - 7, label$, "LT",, 1, C_TXT
END SUB

FUNCTION IsPressed%(p%)
  IsPressed% = (PIN(p%) = 0)   ' con PULLUP: pulsado = 0
END FUNCTION

SUB SetupButtons()
  SETPIN PIN_X, DIN, PULLUP
  SETPIN PIN_A, DIN, PULLUP
  SETPIN PIN_Y, DIN, PULLUP
  SETPIN PIN_B, DIN, PULLUP

  SETPIN PIN_LEFT, DIN, PULLUP
  SETPIN PIN_DOWN, DIN, PULLUP
  SETPIN PIN_UP, DIN, PULLUP
  SETPIN PIN_RIGHT, DIN, PULLUP

  SETPIN PIN_L, DIN, PULLUP
  SETPIN PIN_R, DIN, PULLUP

  SETPIN PIN_SELECT, DIN, PULLUP
  SETPIN PIN_START, DIN, PULLUP
END SUB

' ---------------- MAIN ----------------
CLS
BACKLIGHT 100
SetupButtons

BOX 0, 0, MM.HRES, MM.VRES, 1, C_EDGE, C_BG
TEXT 8, 6, "PAD TEST    Salir: SEL+START", "LT",, 1, C_TXT
TEXT 8, 24, "Pulsa botones del pad", "LT",, 1, C_SUB

' Dibujo inicial
DrawBtnRect 10, 40, 60, 24, "L", 0
DrawBtnRect 170, 40, 60, 24, "R", 0

DrawBtnRound 60, 95, 14, "U", 0
DrawBtnRound 60, 145,14, "D", 0
DrawBtnRound 35, 120,14, "L", 0
DrawBtnRound 85, 120,14, "R", 0

DrawBtnRound 180, 95, 14, "Y", 0
DrawBtnRound 155, 120,14, "X", 0
DrawBtnRound 205, 120,14, "B", 0
DrawBtnRound 180, 145,14, "A", 0


DrawBtnRect 40, 190, 70, 28, "SELECT", 0
DrawBtnRect 130,190, 70, 28, "START", 0

DIM lX%, lA%, lY%, lB%, lU%, lD%, lLf%, lRt%, lL%, lR%, lSel%, lSta%

DIM INTEGER p%

DO
  IF INKEY$ = CHR$(27) THEN EXIT DO

  p% = IsPressed%(PIN_L) : IF p%<>lL% THEN DrawBtnRect 10,40,60,24,"L",p% : lL%=p%
  p% = IsPressed%(PIN_R) : IF p%<>lR% THEN DrawBtnRect 170,40,60,24,"R",p% : lR%=p%

  p% = IsPressed%(PIN_UP)    : IF p%<>lU%  THEN DrawBtnRound 60,95,14,"U",p%  : lU%=p%
  p% = IsPressed%(PIN_DOWN)  : IF p%<>lD%  THEN DrawBtnRound 60,145,14,"D",p% : lD%=p%
  p% = IsPressed%(PIN_LEFT)  : IF p%<>lLf% THEN DrawBtnRound 35,120,14,"L",p% : lLf%=p%
  p% = IsPressed%(PIN_RIGHT) : IF p%<>lRt% THEN DrawBtnRound 85,120,14,"R",p% : lRt%=p%

  p% = IsPressed%(PIN_Y) : IF p%<>lY% THEN DrawBtnRound 180, 95, 14, "Y", p% : lY%=p%
  p% = IsPressed%(PIN_X) : IF p%<>lX% THEN DrawBtnRound 155, 120,14, "X", p% : lX%=p%
  p% = IsPressed%(PIN_B) : IF p%<>lB% THEN DrawBtnRound 205, 120,14, "B", p% : lB%=p%
  p% = IsPressed%(PIN_A) : IF p%<>lA% THEN DrawBtnRound 180, 145,14, "A", p% : lA%=p%

  p% = IsPressed%(PIN_SELECT) : IF p%<>lSel% THEN DrawBtnRect 40,190,70,28,"SELECT",p% : lSel%=p%
  p% = IsPressed%(PIN_START)  : IF p%<>lSta% THEN DrawBtnRect 130,190,70,28,"START",p%  : lSta%=p%

  IF IsPressed%(PIN_SELECT) AND IsPressed%(PIN_START) THEN
    ' ambos pulsados a la vez
    EXIT DO
  ENDIF


  PAUSE 10
LOOP

CLS
TEXT 10, 10, "Volviendo al menu...", "LT",, 1, C_TXT
RUN "menu.bas"
END
