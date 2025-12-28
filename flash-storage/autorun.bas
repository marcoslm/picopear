'============================================================
' Autorun/bootloader para placa PicoPear (SpotPear)
' por Marcos LM 2025 - WIP
'
' Instalar:
'   FLASH OVERWRITE 1
'   OPTION AUTORUN 1
'============================================================
OPTION EXPLICIT
OPTION DEFAULT NONE

CLS
COLOR RGB(GREEN)
TEXT 0, 0, "PicoPear Boot Menu..."

IF MM.INFO(EXISTS FILE "A:/menu.bas") THEN
  COLOR RGB(WHITE),RGB(BLACK)
  RUN "A:/menu.bas"
ELSE
  COLOR RGB(RED)
  TEXT 0, 30, "ERROR: no existe A:/menu.bas"
  TEXT 0, 50, "Pulsa Ctrl-C para salir."
  DO : LOOP
ENDIF

END
