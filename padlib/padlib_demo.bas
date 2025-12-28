OPTION EXPLICIT
OPTION DEFAULT INTEGER
#include "PADLIB.BAS"   ' MMReplace addon

CONST C_BG   = RGB(0,0,0)
CONST C_W    = RGB(255,255,255)
CONST C_C    = RGB(0,200,255)
CONST C_G    = RGB(0,220,80)
CONST C_Y    = RGB(255,255,0)
CONST C_D    = RGB(120,120,120)
CONST C_EDGE = RGB(80,80,80)

DIM INTEGER now%, hit%, rep%, lastNow%, lastHit%, lastRep%
DIM INTEGER y0%, dy%, xl%, xr%

FUNCTION Hex4$(v%)
  Hex4$ = RIGHT$("0000" + HEX$(v%), 4)
END FUNCTION

SUB DrawFlag(x%, y%, label$, onNow%, onHit%, onRep%)
  LOCAL INTEGER col%
  ' prioridad: HIT (amarillo) > REP (cian) > NOW (verde) > gris
  IF onHit% THEN
    col% = C_Y
  ELSEIF onRep% THEN
    col% = C_C
  ELSEIF onNow% THEN
    col% = C_G
  ELSE
    col% = C_D
  ENDIF
  TEXT x%, y%, label$, "LT",, 1, col%
END SUB

BACKLIGHT 100
CLS C_BG

PadInit
PadSetRepeat 350, 80

BOX 0,0,MM.HRES,MM.VRES,1,C_EDGE,C_BG
TEXT 6, 4,  "PAD VIEW 240x240   (ESC sale)", "LT",, 1, C_W
TEXT 6, 18, "NOW=G  HIT=Y  REP(DPad)=C", "LT",, 1, C_C

' Layout (2 columnas x 6 filas)
y0% = 58
dy% = 18
xl% = 10
xr% = 128

DO
  IF INKEY$ = CHR$(27) THEN EXIT DO

  PadScan
  now% = PadRead%()
  hit% = PadPressed%()
  rep% = PadRepeat%()

  IF now%<>lastNow% OR hit%<>lastHit% OR rep%<>lastRep% THEN

    ' Status line
    BOX 0, 34, MM.HRES, 16, , C_BG, C_BG
    TEXT 6, 34, "N:" + Hex4$(now%) + "  H:" + Hex4$(hit%) + "  R:" + Hex4$(rep%), "LT",, 1, C_W

    ' Limpia zona lista
    BOX 0, 54, MM.HRES, MM.VRES-54, , C_BG, C_BG

    ' fila 1
    DrawFlag xl%, y0% + 0*dy%, "UP",     (now% AND PAD_UP)<>0,    (hit% AND PAD_UP)<>0,    (rep% AND PAD_UP)<>0
    DrawFlag xr%, y0% + 0*dy%, "Y",      (now% AND PAD_Y)<>0,     (hit% AND PAD_Y)<>0,     0

    ' fila 2
    DrawFlag xl%, y0% + 1*dy%, "DOWN",   (now% AND PAD_DOWN)<>0,  (hit% AND PAD_DOWN)<>0,  (rep% AND PAD_DOWN)<>0
    DrawFlag xr%, y0% + 1*dy%, "A",      (now% AND PAD_A)<>0,     (hit% AND PAD_A)<>0,     0

    ' fila 3
    DrawFlag xl%, y0% + 2*dy%, "LEFT",   (now% AND PAD_LEFT)<>0,  (hit% AND PAD_LEFT)<>0,  (rep% AND PAD_LEFT)<>0
    DrawFlag xr%, y0% + 2*dy%, "X",      (now% AND PAD_X)<>0,     (hit% AND PAD_X)<>0,     0

    ' fila 4
    DrawFlag xl%, y0% + 3*dy%, "RIGHT",  (now% AND PAD_RIGHT)<>0, (hit% AND PAD_RIGHT)<>0, (rep% AND PAD_RIGHT)<>0
    DrawFlag xr%, y0% + 3*dy%, "B",      (now% AND PAD_B)<>0,     (hit% AND PAD_B)<>0,     0

    ' fila 5
    DrawFlag xl%, y0% + 4*dy%, "L",      (now% AND PAD_L)<>0,     (hit% AND PAD_L)<>0,     0
    DrawFlag xr%, y0% + 4*dy%, "R",      (now% AND PAD_R)<>0,     (hit% AND PAD_R)<>0,     0

    ' fila 6
    DrawFlag xl%, y0% + 5*dy%, "SELECT", (now% AND PAD_SELECT)<>0,(hit% AND PAD_SELECT)<>0,0
    DrawFlag xr%, y0% + 5*dy%, "START",  (now% AND PAD_START)<>0, (hit% AND PAD_START)<>0, 0

    lastNow% = now% : lastHit% = hit% : lastRep% = rep%
  ENDIF

  PAUSE 10
LOOP

CLS C_BG
TEXT 10,10,"Bye!", "LT",, 2, C_W
END
