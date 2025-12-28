'============================================================
' PADLIB.BAS  -  Spotpear Pico 1.54" Gamepad (12 botones)
' Para firmware PicoMite
' Marcos LM 2025
'
' Uso:
'   PadInit
'   DO
'     PadScan
'     now% = PadRead%()
'     hit% = PadPressed%()
'     rep% = PadRepeat%()
'     ...
'   LOOP
'
' Notas:
' - PadRead%    = estado actual (bits mantenidos)
' - PadPressed% = flanco (solo cuando se pulsa "nuevo")
' - PadRepeat%  = pulsos repetidos SOLO para D-Pad (UP/DOWN/LEFT/RIGHT)
'                 (incluye el primer pulso al presionar)
'============================================================

'--------------------------
' Pines Fisicos
'--------------------------
' ABXY
CONST PAD_PIN_X      = 4    ' GP2
CONST PAD_PIN_A      = 5    ' GP3
CONST PAD_PIN_Y      = 6    ' GP4
CONST PAD_PIN_B      = 9    ' GP6

' D-Pad
CONST PAD_PIN_LEFT   = 17   ' GP13
CONST PAD_PIN_DOWN   = 19   ' GP14
CONST PAD_PIN_UP     = 21   ' GP16
CONST PAD_PIN_RIGHT  = 22   ' GP17

' Hombros
CONST PAD_PIN_L      = 24   ' GP18
CONST PAD_PIN_R      = 2    ' GP1

' Start/Select
CONST PAD_PIN_SELECT = 31   ' GP26
CONST PAD_PIN_START  = 32   ' GP27

'--------------------------
' Bits del mando (bitmask)
'--------------------------
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

'--------------------------
' Estado interno (global)
'--------------------------
DIM INTEGER gPadNow%, gPadLast%, gPadHit%, gPadRel%, gPadRep%
DIM INTEGER gPadRepNext%(4)   ' 0:UP 1:DOWN 2:LEFT 3:RIGHT
DIM INTEGER gPadDelayFirst%   ' ms hasta primer repeat
DIM INTEGER gPadDelayNext%    ' ms entre repeats
DIM INTEGER gPadInited%

'--------------------------
' Init
'--------------------------
SUB PadInit()
  ' Entradas con pull-up (pulsado = 0)
  SETPIN PAD_PIN_X, DIN, PULLUP
  SETPIN PAD_PIN_A, DIN, PULLUP
  SETPIN PAD_PIN_Y, DIN, PULLUP
  SETPIN PAD_PIN_B, DIN, PULLUP

  SETPIN PAD_PIN_LEFT, DIN, PULLUP
  SETPIN PAD_PIN_DOWN, DIN, PULLUP
  SETPIN PAD_PIN_UP, DIN, PULLUP
  SETPIN PAD_PIN_RIGHT, DIN, PULLUP

  SETPIN PAD_PIN_L, DIN, PULLUP
  SETPIN PAD_PIN_R, DIN, PULLUP

  SETPIN PAD_PIN_SELECT, DIN, PULLUP
  SETPIN PAD_PIN_START, DIN, PULLUP

  gPadDelayFirst% = 350
  gPadDelayNext%  = 80

  gPadNow% = 0 : gPadLast% = 0 : gPadHit% = 0 : gPadRel% = 0 : gPadRep% = 0
  gPadRepNext%(0) = 0 : gPadRepNext%(1) = 0 : gPadRepNext%(2) = 0 : gPadRepNext%(3) = 0

  gPadInited% = 1
END SUB

SUB PadSetRepeat(firstDelayMs%, nextDelayMs%)
  IF firstDelayMs% < 0 THEN firstDelayMs% = 0
  IF nextDelayMs%  < 1 THEN nextDelayMs%  = 1
  gPadDelayFirst% = firstDelayMs%
  gPadDelayNext%  = nextDelayMs%
END SUB

'--------------------------
' Lectura HW -> bitmask
'--------------------------
FUNCTION PadHwRead%()
  LOCAL INTEGER m%

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

  PadHwRead% = m%
END FUNCTION

'--------------------------
' Scan (llamar 1 vez/frame)
'--------------------------
SUB PadScan()
  LOCAL INTEGER t%, now%, hit%, rel%

  IF gPadInited% = 0 THEN PadInit

  t% = TIMER
  now% = PadHwRead%()

  hit% = now% AND (INV gPadLast%)
  rel% = gPadLast% AND (INV now%)


  gPadNow% = now%
  gPadHit% = hit%
  gPadRel% = rel%
  gPadRep% = 0

  ' --- Repeat solo para D-Pad ---
  ' Indices: 0 UP, 1 DOWN, 2 LEFT, 3 RIGHT
  PadRepeatOne t%, 0, PAD_UP,    hit%, now%
  PadRepeatOne t%, 1, PAD_DOWN,  hit%, now%
  PadRepeatOne t%, 2, PAD_LEFT,  hit%, now%
  PadRepeatOne t%, 3, PAD_RIGHT, hit%, now%

  gPadLast% = now%
END SUB

SUB PadRepeatOne(t%, idx%, bit%, hit%, now%)
  ' Si esta pulsado:
  ' - en el flanco (hit) emitimos 1 pulso y armamos timer de primer repeat
  ' - si no es flanco, repetimos cuando toca
  IF (now% AND bit%) <> 0 THEN
    IF (hit% AND bit%) <> 0 THEN
      gPadRep% = gPadRep% OR bit%
      gPadRepNext%(idx%) = t% + gPadDelayFirst%
    ELSE
      IF gPadRepNext%(idx%) <> 0 THEN
        IF t% >= gPadRepNext%(idx%) THEN
          gPadRep% = gPadRep% OR bit%
          gPadRepNext%(idx%) = t% + gPadDelayNext%
        ENDIF
      ENDIF
    ENDIF
  ELSE
    gPadRepNext%(idx%) = 0
  ENDIF
END SUB

'--------------------------
' Getters (despues de PadScan)
'--------------------------
FUNCTION PadRead%()
  PadRead% = gPadNow%
END FUNCTION

FUNCTION PadPressed%()
  PadPressed% = gPadHit%
END FUNCTION

FUNCTION PadReleased%()
  PadReleased% = gPadRel%
END FUNCTION

FUNCTION PadRepeat%()
  PadRepeat% = gPadRep%
END FUNCTION
