'============================================================
' pad_lib_min.bas - Demo minima para padlib_min
'
' Muestra:
'   - HEX del bitmask
'   - Lista de botones pulsados (texto)
'============================================================

OPTION EXPLICIT
OPTION DEFAULT INTEGER

#include "padlib_min.bas"   ' MMReplace addon

DIM INTEGER now%, last%
DIM STRING s$

CLS
TEXT 2, 2, "padlib_min demo - pulsa boton"

PadInit

DO
  now% = PadRead%()

  IF now% <> last% THEN
    BOX 0, 20, MM.HRES, 60, 0, 0, 0

    TEXT 2, 22, "NOW: &H" + HEX$(now%, 4)
    s$ = PadMaskToNames$(now%)
    TEXT 2, 42, "BTN: " + s$

    last% = now%
  ENDIF

  PAUSE 10
LOOP

FUNCTION PadMaskToNames$(m%)
  LOCAL STRING out$

  IF m% = 0 THEN
    PadMaskToNames$ = "-"
    EXIT FUNCTION
  ENDIF

  IF (m% AND PAD_UP)    THEN AddName out$, "UP"
  IF (m% AND PAD_DOWN)  THEN AddName out$, "DOWN"
  IF (m% AND PAD_LEFT)  THEN AddName out$, "LEFT"
  IF (m% AND PAD_RIGHT) THEN AddName out$, "RIGHT"

  IF (m% AND PAD_A) THEN AddName out$, "A"
  IF (m% AND PAD_B) THEN AddName out$, "B"
  IF (m% AND PAD_X) THEN AddName out$, "X"
  IF (m% AND PAD_Y) THEN AddName out$, "Y"

  IF (m% AND PAD_L) THEN AddName out$, "L"
  IF (m% AND PAD_R) THEN AddName out$, "R"

  IF (m% AND PAD_SELECT) THEN AddName out$, "SELECT"
  IF (m% AND PAD_START)  THEN AddName out$, "START"

  PadMaskToNames$ = out$
END FUNCTION

SUB AddName(BYREF s$, name$)
  IF s$ <> "" THEN s$ = s$ + " "
  s$ = s$ + name$
END SUB
