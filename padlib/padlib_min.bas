'============================================================
' padlib_min.bas  -  Spotpear Pico 1.54" Gamepad (MIN)
' Para firmware PicoMite
' Marcos LM 2025
'
' Uso:
'   PadInit          ' una vez
'   m% = PadRead%()  ' estado actual (bitmask)
'============================================================

'--------------------------
' Pines Fisicos
'--------------------------
CONST PAD_PIN_X      = 4    ' GP2
CONST PAD_PIN_A      = 5    ' GP3
CONST PAD_PIN_Y      = 6    ' GP4
CONST PAD_PIN_B      = 9    ' GP6

CONST PAD_PIN_LEFT   = 17   ' GP13
CONST PAD_PIN_DOWN   = 19   ' GP14
CONST PAD_PIN_UP     = 21   ' GP16
CONST PAD_PIN_RIGHT  = 22   ' GP17

CONST PAD_PIN_L      = 24   ' GP18
CONST PAD_PIN_R      = 2    ' GP1

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

DIM INTEGER gPadMinInited%

SUB PadInit
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

  gPadMinInited% = 1
END SUB

FUNCTION PadRead%()
  LOCAL INTEGER m% = 0

  IF gPadMinInited% = 0 THEN PadInit

  ' PULLUP -> pulsado = 0
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
