'============================================================
' Menu de juegos para placa SpotPear
' por Marcos LM 2025 - WIP
'
' Requisitos:
'     OPTION LCDPANEL ST7789, LANDSCAPE, GP8, GP12, GP9, GP7
'     OPTION SYSTEM SPI GP10,GP11,GP28
'
'============================================================
OPTION EXPLICIT
OPTION DEFAULT INTEGER

CLEAR
FRAMEBUFFER CLOSE

' Configuracion menu
CONST MENU_Y = 46
CONST MENU_COUNT = 9
DIM STRING item$(MENU_COUNT)
DIM STRING file$(MENU_COUNT)

item$(1) = "PicoVaders"
file$(1) = "A:/picovaders.bas"

item$(2) = "PicoFrog"
file$(2) = "A:/picofrog.bas"

item$(3) = "Circle One"
file$(3) = "A:/circle.bas"

item$(4) = "vacio"
file$(4) = ""

item$(5) = "vacio"
file$(5) = ""

item$(6) = "vacio"
file$(6) = ""

item$(7) = "vacio"
file$(7) = ""

item$(8) = "vacio"
file$(8) = ""

item$(9) = "Pads Test"
file$(9) = "A:/test_botones.bas"

' Pad bits (estado)
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

' Mapeo Pines-Botones
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

DIM INTEGER sel%, now%, last%, press%

' Inicializar
CLS RGB(3,3,9)
BACKLIGHT 90
PadInit
COLOR RGB(255,255,255), RGB(3,3,9)
TEXT MM.HRES/2, 0,  "Picopear Game Menu", "CT",2, 1, RGB(255,255,0)
sel% = 1
DrawMenu sel%, MENU_Y

'------- Bucle principal ----------------------------------
DO
  now% = PadRead%()

  ' flanco (pressed) -> OJO: INV (NO NOT)
  press% = now% AND (INV last%)

  IF (press% AND PAD_UP) THEN
    IF sel% > 1 THEN sel% = sel% - 1 : DrawMenu sel%, MENU_Y
  ENDIF

  IF (press% AND PAD_DOWN) THEN
    IF sel% < MENU_COUNT THEN sel% = sel% + 1 : DrawMenu sel%, MENU_Y
  ENDIF

  IF (press% AND (PAD_A OR PAD_START)) THEN
    IF file$(sel%) <> "" THEN
      CLS 'ClearScreen
      TEXT MM.HRES/2, 90, "cargando", "CM",3, 1, RGB(WHITE)
      TEXT MM.HRES/2 , 130, item$(sel%), "CM",3, 1, RGB(YELLOW)
      PAUSE 500
      RUN file$(sel%)
    ENDIF
  ENDIF

  last% = now%
  PAUSE 20
LOOP
END

'-----------------------------------------------------

SUB DrawMenu(s%, y%)
  LOCAL INTEGER i%
  ClearScreen
  FOR i% = 1 TO MENU_COUNT
    IF i% = s% THEN
      COLOR RGB(0,255,0), RGB(6,12,30)
      TEXT 8, y%, FormatTxt(item$(i%), 14), "LM",3, 1
    ELSE
      COLOR RGB(150,150,150), RGB(30,40,90)
      TEXT 8, y%, FormatTxt(item$(i%), 20), "LM",1, 1
    ENDIF
    y% = y% + 22
  NEXT i%
END SUB

SUB GradientV(x%, y%, w%, h%, r1%, g1%, b1%, r2%, g2%, b2%)
  LOCAL INTEGER i%, rr%, gg%, bb%
  FOR i% = 0 TO h%-1
    rr% = r1% + (r2%-r1%) * i% \ (h%-1)
    gg% = g1% + (g2%-g1%) * i% \ (h%-1)
    bb% = b1% + (b2%-b1%) * i% \ (h%-1)
    LINE x%, y%+i%, x%+w%-1, y%+i%, 1, RGB(rr%,gg%,bb%)
  NEXT i%
END SUB

SUB ClearScreen
  GradientV 0,25,MM.HRES,MM.VRES-25,  5,5,10,   0,130,200   ' azul oscuro -> cian
END SUB

FUNCTION FormatTxt (txt$, wid%) AS STRING
  LOCAL ttemp$
  ttemp$ = " "
  ttemp$ = ttemp$ + LEFT$(txt$, wid%-1)
  IF LEN(ttemp$)< wid% THEN
    ttemp$ = ttemp$ + SPACE$(wid%-LEN(ttemp$))
  END IF
  FormatTxt = ttemp$
END FUNCTION

SUB PadInit
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
END SUB

FUNCTION PadRead%()
  LOCAL INTEGER m%
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
