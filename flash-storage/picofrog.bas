  'Pico Frog
  ' by Martin Herhaus 2022
  ' This is a Frogger game which resembles the original Arcade version
  ' from Konami  (c) released in 1981, but completely rewritten from the
  ' ground up in MMBasicVGA for the Raspberry Pico
  '
  ' This code has an open source license, you can copy it, change it,
  ' for any purpose for commercial use or not. But these comments
  ' should be in you main file.
  ' The graphics and Music are based upon but not the original
  '------
  ' developed on PicoMite Version 5.07.05RC...
  ' runs on PicoMite Version 5.07.06
  ' would not run on lower Versions due to the use of
  ' Framebuffer, Layer and Sprites. For this, the Pico also has to run
  ' at 252000 KHz or higher
  '
  ' Use OPTION CPUSPEED 252000
  ' @harm : patches for PicoMite MMbasic 6 (tested on 6.01.00b11) and RP2350
  '------------------------------------------------------------------------ 
  ' Adapted version for SpotPear board by Marcos LM 2025 - WIP
  '
  OPTION BASE 0
  OPTION DEFAULT NONE
  OPTION EXPLICIT ON
  
  'platform button map
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

  DIM f_merge%=0, l_time%, h_beat%=50'30 '@harm to get suitible game play with LCD
  IF INSTR(MM.DEVICE$,"2350") THEN
    CONST n_adsr%=10 '10 adsr steps per note
    CONST beat%=23 'ms
    DIM ADSR%(n_adsr%)=(25,25,25,24,22,20,18,13,7,3,0)
    DIM ADSR2%(n_adsr%)=(2,8,12,17,22,20,15,10,5,2,0)
  ELSE '2040, less cpu power
    CONST n_adsr%=7
    CONST beat%=33 'ms
    DIM ADSR%(n_adsr%)=(25,24,22,18,13,7,3,0)
    DIM ADSR2%(n_adsr%)=(3,10,21,20,13,9,3,0)
  END IF
  
  'game
  DIM is_fly%=0  '@fly
  DIM Col%(15)
  DIM FPOSX%,FPOSY%,FPDIR%,FPOSP%=18,FMOV%,tmp$,f%,RSTRT%,FDEL%,Level%,Homes%,Time%
  DIM flx%,fly%,fox%,foy%,cl1%,cl2%,snr%,mt%,FL%,FR%,JP%,n%,Score%,High%,i%
  DIM Lane$(10) Length 32,PLD$ Length 1,lps%(10),Fmax%,LTime%,Lives%,Tm%,FD%=0
  DIM ld%(10)=(0,2,-1,1,2,-2,-2,4,-2,1,-1),dxl%
  DIM ly%(10)=(0,24,40,56,72,88,120,136,152,168,184)
  DIM dt_seq%(9)=(9,10,11,12,13,63,13,12,11,10),dt2%=0,dtx%,dt5%=0,FU$
  DIM SND%,LP%,TX%,TY%,TDX%,TDY%,spnr%,Slot%,Freq1%,Freq2%,Nest%(5),str%(20)
  
  'alternate controllers @harm
  init_kb 'initialize keys
  
  '--------- prepare the Graphic
  High%=2500
  RESTORE colors:FOR f%=1 TO 15:READ Col%(f%):NEXT f%
  JP%=600
  SND%=1
  FONT 9
  FRAMEBUFFER CREATE
  FRAMEBUFFER LAYER
  FRAMEBUFFER WRITE F
  
  'Prepare the playing field
  disp
  FRAMEBUFFER WRITE L
  CLS
  BOX 0,0,224,218,,col%(1),col%(1)
  'Box 224,0,96,216,,COL%(7),COL%(1)
  'Box 226,2,92,212,,COL%(7),COL%(1)
  'Box 239,6,64,44,,Col%(6),COL%(4)
  bing
  FRAMEBUFFER merge 0,b
  FRAMEBUFFER WRITE F
  read_Sprites
  BOX 0,0,224,24,,0,0
  FOR f%=0 TO 192 STEP 8:SPRITE WRITE 29,f%,0: NEXT f%
  FOR f%=0 TO 192 STEP 48:BOX f%,0,32,24,,0,0:SPRITE WRITE 28,f%,0: NEXT f%
  
start:
  FRAMEBUFFER WRITE L
  BOX 0,0,224,220,,0,0
  startscreen
  Score%=0:Level%=0:Lives%=3
  ShowLives
  RSTRT%=0
  'Start Music
  RESTORE intro
  Mt%=1
  IF SND% THEN SETTICK beat%, music,4
  LTime%=TIMER:FDEL%=50
  
restart: 'this is a new game
  Homes%=0
  'load the new level
  INC Level% : loadlevel Level% : Create_street
  'clear the whole Playfeeld on the layer
  FRAMEBUFFER WRITE L
  BOX 0,0,220,220,,0,0
  TEXT 50,220, "SCORE",,,,Col%(7),0
  TEXT 100,220,"HI-SCORE",,,,Col%(7),0
  TEXT 0,220,"LIVES",,,,Col%(7),0
  TEXT 180,220,"TIME",,,,Col%(7),0
  write_high
  
restart1: 'this is the next frog
  ' clear Homeslots
  is_fly%=0 : add_fly is_fly% '@fly
  
  INC Score%,10*(INT((TIMER-LTime%)/500)) : write_score
  IF FD%=0 THEN BOX 0,0,220,220,,0,0
  FD%=0 : FU$=""
  LTime%=TIMER
  DO WHILE INKEY$<>"":LOOP 'clear input buffer
  FPOSX%=96:FPOSY%=200:FPDIR%=1'@harm
  FMOV%=0:FMAX%=FPOSY%
  
  '----------gameloop-----------------------------------
  LP%=1
  TX%=0:TY%=0:TDX%=2:TDY%=2
  spnr%=10
  DO
    move_lanes
    add_fly is_fly% '@fly
    f_merge%=1 'trigger for framebuffer merge 0,b
    FRAMEBUFFER WRITE L
    
    DO : LOOP UNTIL TIMER>l_time% '@harm, to get consisten speed
    l_time% = TIMER + h_beat%
    
    IF FDEL%=1 THEN
      BOX 0,0,220,220,,0,0
      LTime%=TIMER:tm%=1
      SPRITE WRITE FPOSP%+1,FPOSX%,FPOSY%
      DO WHILE INKEY$<>"":LOOP ' @thwill
    ENDIF
    
    IF FDEL% THEN
      INC FDEL%,-1
    ELSE
      Move_Player : INC tm%,-1
      SELECT CASE tm%
        CASE 0 'update timer
          Update_Timer 60-INT((TIMER-LTime%)/500)
          tm%=20 '20 x 50ms(h_beat%) = 1 second
        CASE 4 'diving turtles lane 2
          dxl%=INSTR(lane$(2),"uuu") : dtx%=16*(dxl%)-lps%(2)-4
          'text 0,225,str$(dxl%,2,0," ")+" "+str$(lps%(2),2,0," ") '@harm debug
          IF dt2%<10 THEN
            IF dtx%<240 AND dtx%>0 THEN
              FRAMEBUFFER WRITE F
              SPRITE WRITE dt_seq%(dt2%),dtx%,ly%(2)
              SPRITE WRITE dt_seq%(dt2%),dtx%+16,ly%(2)
              SPRITE WRITE dt_seq%(dt2%),dtx%+32,ly%(2)
              FRAMEBUFFER WRITE l
            END IF
            INC dt2%
          ELSE
            dt2%=0 'end of sequence
          END IF
        CASE 8 'diving turtles lane 5
          dxl%=INSTR(lane$(5),"uuu") : dtx%=16*(dxl%)-lps%(5)-4
          'text 0,225,str$(dxl%,2,0," ")+" "+str$(lps%(2),2,0," ") '@harm debug
          IF dt5%<10 THEN
            IF dtx%<240 AND dtx%>0 THEN
              FRAMEBUFFER WRITE F
              SPRITE WRITE dt_seq%(dt5%),dtx%,ly%(5)
              SPRITE WRITE dt_seq%(dt5%),dtx%+16,ly%(5)
              SPRITE WRITE dt_seq%(dt5%),dtx%+32,ly%(5)
              FRAMEBUFFER WRITE l
            END IF
            INC dt5%
          ELSE
            dt5%=0 'end of sequence
          END IF
        CASE 12 'check if you die
          IF FU$="u" AND FD%=0 THEN
            IF FPOSY%>ly%(3) THEN 'turtle line 5
              IF dt5%>3 AND dt5%<7 THEN  FD%=1
            ELSE 'turtle line 2
              IF dt2%>3 AND dt2%<7 THEN  FD%=1
            END IF
          END IF
        CASE 16 'animate croco
          'for croc in line 3
          i%=INSTR(lane$(3),"m")
          IF i% THEN
            f%=17-(RND<0.5)
            FRAMEBUFFER WRITE F
            SPRITE WRITE f%,MIN(16*(i%-2)+lps%(3)+4,224),ly%(3)
            FRAMEBUFFER WRITE L
          END IF
      END SELECT
    ENDIF
    
    IF RSTRT%=1 THEN RSTRT%=0 : GOTO restart1 'new frog
    IF RSTRT%=2 THEN RSTRT%=0 : GOTO restart  'next level
    IF FMOV%=0 THEN automove
    
    Get_frog_Undergrnd
    
    IF FD% THEN
      'frog death
      RESTORE FKill
      JP%=220:SETTICK 15,DieSnd,1
      Hide_Frog
      FRAMEBUFFER WRITE L:SPRITE WRITE 30,FPOSX%,FPOSY%
      FDEL%=20
      INC Lives%,-1
      ShowLives
      IF Lives%=0 THEN
        RESTORE Game_OverSng : GameOver
      ELSE
        GOTO restart1
      END IF
    ENDIF
    
  LOOP
  
SUB add_fly n% '@fly
  'fly sprite = 27, n% = 0(none),1,2,3,4,5 home slot
  IF n% THEN SPRITE WRITE 27,8+(n%-1)*48,8
END SUB
  
SUB GameOver
  TEXT 112,112,"GAME OVER",CM,,2,col%(8),col%(7)
  FRAMEBUFFER merge 0,b
  PAUSE 10000
  RUN '@harm prevent nesting issue
END SUB
  
SUB ShowLives
  LOCAL F%
  FRAMEBUFFER WRITE L
  BOX 0,228,42,16,,0,0
  FOR f%=0 TO Lives%-1 
    SPRITE WRITE FPOSP%+1,f%*14 ,228
  NEXT f%
END SUB
  
SUB write_score
  LOCAL S$
  s$=STR$(Score%)
  FRAMEBUFFER WRITE L
  DO WHILE LEN(s$)<5:s$="0"+s$:LOOP
  TEXT 50,232, S$,,,,Col%(7),0
  IF Score%>High% THEN High%=Score%: write_High
END SUB
  
SUB write_high
  LOCAL S$
  s$=STR$(High%)
  FRAMEBUFFER WRITE L
  DO WHILE LEN(s$)<5:s$="0"+s$:LOOP
  TEXT 100,232, S$,,,,Col%(7),0
END SUB
  
SUB Update_Timer TimeLeft%
  IF TimeLeft%>0 THEN
    FRAMEBUFFER WRITE L
    BOX 180+TimeLeft%,230,60-TimeLeft%,10,,0,0
    BOX 180,230,TimeLeft%,10,,,Col%(2+2*(TimeLeft%<20))
    BOX 180,230,60,10,,col%(7)
    
    '@fly handling, appear <30 sec, remove >5 sec, do not remove frog in nest
    IF timeleft%<30 THEN
      IF is_fly%=0 THEN
        IF timeleft%>5 THEN
          DO '@fly : find free slot
            is_fly%=INT(1+5*RND)
          LOOP UNTIL nest%(is_fly%)=0
        END IF
      ELSE
        IF timeleft%<5 THEN 'remove fly again
          IF nest%(is_fly%)=0 THEN 'no frog landed
            FRAMEBUFFER WRITE F
            BOX 8+(is_fly%-1)*48,8,16,16,,col%(1),col%(1) 'clear fly
            FRAMEBUFFER WRITE L
          END IF
          is_fly%=0 'too late to land on frog
        END IF
      END IF
    END IF
  ELSE '@fly ---end fly section
    FD%=1 'frog = dead
  ENDIF
END SUB
  
SUB startscreen
  LOCAL title$,tk%,tc%,x%,y%,f%,bit%
  DO WHILE INKEY$<>"":LOOP ' @thwill
  FRAMEBUFFER WRITE F
  CLS
  FOR x%=200 TO 16 STEP -8
    FOR y%=16 TO 184 STEP 16
      SPRITE WRITE 29,x%,y%
    NEXT y%
  NEXT x%
  RESTORE Logo
  FOR y%=1 TO 17
    READ str%(y%)
  NEXT
  FOR f%=0 TO 218 STEP 16
    flowers f%,0,col%(1),Col%(4):flowers f%,200,col%(1),Col%(4)
  NEXT
  bit%=4
  BOX 16,16,200,6,,0,0
  BOX 204,16,8,136,,0,0
  BOX 16,16,12,136,,0,0
  BOX 16,124,200,64,,0,0
  FOR x%=30 TO 1 STEP -1
    FOR y%=1 TO 17
      IF NOT (str%(y%) AND bit%) THEN
        BOX 20+x%*6,16+y%*6,6,6,,0,0
      ENDIF
    NEXT
    INC bit%,bit%:PAUSE 20
  NEXT
  FRAMEBUFFER WRITE L
  FOR y%=16 TO 200 STEP 16
    flowers 0,y%,col%(1),Col%(4):flowers 208,y%,col%(1),Col%(4)
  NEXT
  FRAMEBUFFER WRITE F
  COLOUR col%(7):'Font 7
  TEXT 112,140,"PicoMite Version",C ' @thwill
  TEXT 112,148,"by Martin Herhaus 2022",C ' @thwill
  TEXT 112,156,"PicoGAME VGA Support",C ' @thwill
  TEXT 112,164,"by Thomas Williams",C ' @thwill
  Title$=" Game based on the Arcade Game Frogger - copyright Konami 1981  - "
  Title$=Title$+"The Graphics and Music are based upon but not the original ***"
  tc%=0: tk%=7
  DO
    IF TK%=7 THEN
      COLOUR col%(6) : TEXT 14,176,LEFT$(title$,34)
      title$=RIGHT$(title$,LEN(title$)-1)+LEFT$(title$,1)
      COLOUR Col%(tc%):INC tc%:TC%=TC% AND 15
      TEXT 112,184,"Press Fire,'A' or Space",C
    ELSE
      BLIT 17,176,16,176,197,8
      BOX 207,168,1,8,,0,0
    ENDIF
    INC tk%,-1:IF tk%=0 THEN TK%=7
    FRAMEBUFFER merge 0,b
    IF rd_kb$()="S " THEN
      FRAMEBUFFER CLOSE
      RUN "A:/menu.bas"
    END IF
  LOOP UNTIL INKEY$=" " OR rd_kb$()="A "
  
  bing : PAUSE 500 : start_sound
  FRAMEBUFFER WRITE L
  'clear the Game layer
  BOX 0,0,224,220,,0,0
END SUB
  
SUB Get_frog_Undergrnd
  'Guess what is under the Frog
  IF FMOV%>=2 THEN EXIT SUB
  LOCAL LPX%
  LP%=INT(FPOSY% /16)
  IF LP%=6 THEN EXIT SUB
  IF LP%>6 THEN INC LP%,-1
  'calculate frog position in string array
  IF LP%<11 THEN
    IF ld%(LP%) > 0 THEN
      LPX%=(FPOSX%-Lps%(LP%)-4)/16+2
    ELSE
      LPX%=(FPOSX%+Lps%(LP%)-8)/16+1
    ENDIF
    FU$=MID$(lane$(LP%),LPx%,1) 'this is at frog location in the lanes/water
    IF LP%>5  THEN
      IF FU$<>"0" THEN FD%=1 'street, when not on street=0 you die
    ELSE
      IF INSTR("0km",FU$) THEN FD%=1  'water/croq tail,head, you drown/die
    ENDIF
  ENDIF
END SUB
  
SUB automove
  LP%=INT(FPOSY%/16)
  IF LP%<6 THEN
    FRAMEBUFFER WRITE L
    BOX FPOSX%,FPOSY%,14,16,,0,0
    INC FPOSX%,ld%(LP%)
    IF FPOSX%>200 THEN FPOSX%=200
    IF FPOSX%<0 THEN FPOSX%=0
    SPRITE WRITE FPOSP%+1,FPOSX%,FPOSY%
  ENDIF
END SUB
  
SUB Hide_Frog
  FRAMEBUFFER WRITE L
  BOX FPOSX%,FPOSY%,16,16,,0,0
  FRAMEBUFFER WRITE F
END SUB
  
SUB Frog
  FRAMEBUFFER WRITE L
  SPRITE WRITE FPOSP%+1,FPOSX%,FPOSY%
  FRAMEBUFFER WRITE F
END SUB
  
SUB test_home
  slot% =0
  SELECT CASE FPOSx%
    CASE 4 TO 20
      slot% =1
    CASE 52 TO 68
      slot% =2
    CASE 102 TO 128
      slot% =3
    CASE 148 TO 164
      slot% =4
    CASE 196 TO 212
      slot% = 5
  END SELECT
  IF slot%=0 THEN EXIT SUB
  IF Nest%(slot%) THEN slot%=0:EXIT SUB
  Nest%(slot%)=1 : INC Score%,50
  
  'fly section
  IF slot%=is_fly% THEN INC lives%,(lives%<4) : ShowLives 'you landed on a @fly
  FRAMEBUFFER WRITE F
  BOX 8+(is_fly%-1)*48,8,16,16,,col%(1),col%(1) 'clear fly in nest (in F, frog = L)
  FRAMEBUFFER WRITE L
  is_fly%=0 '@fly regardless we need a new fly next round
  
  write_score
  Hide_Frog
  FDEL%=70
  INC Homes%
  'draw HomeFrog in the Slot
  FRAMEBUFFER WRITE F
  SPRITE WRITE 26,8+(slot%-1)*48,8 'overwites fly if it is in slot
  'Last_Slot?
  IF Homes%=5 THEN
    Homes%=0:INC Level%
    RESTORE complete
    FDEL%=150
    RSTRT%=2
    INC score%,1000
    EXIT SUB
  ENDIF
  SELECT CASE homes%
      CASE 1:RESTORE Yankee:CASE 2:RESTORE Mus2
      CASE 3:RESTORE Mus3:CASE 4:RESTORE home1
  END SELECT
  RSTRT%=1
END SUB
  
SUB Move_Player
  'allready in Motion?, then give time to show animated Sprite
  IF FMOV%>=2 THEN INC FMOV%,-1:EXIT SUB
  IF FMOV%=1 THEN
    Hide_Frog
    FRAMEBUFFER WRITE L
    IF FPDIR%=1 THEN
      INC FPOSY%,-8
      IF FPOSY%< FMAX% THEN INC Score%,10:FMAX%=FPOSY%:write_score
      GOTO fmout
    ENDIF
    IF FPDIR%=2 THEN
      INC FPOSY%,8
      GOTO fmout
    ENDIF
    IF FPDIR%=3 THEN
      INC FPOSX%,-8
      GOTO fmout
    ENDIF
    IF FPDIR%=4 THEN
      INC FPOSX%,8
      IF FPOSX%>200 THEN FPOSX%=200
    ENDIF
FMout:
    SPRITE WRITE FPOSP%+1,FPOSX%,FPOSY%
    FMOV%=0
    EXIT SUB
    FRAMEBUFFER WRITE F
  ENDIF
  
  ' Read selected controller/keyboard
  SELECT CASE INKEY$
      CASE CHR$(128) : PLD$="U"
      CASE CHR$(129) : PLD$="D"
      CASE CHR$(130) : PLD$="L"
      CASE CHR$(131) : PLD$="R"
      CASE ELSE : PLD$ = LEFT$(rd_kb$(),1)
  END SELECT
  
  '@harm: added vector the Fjump so FPOSP% could remain unchanged
  'UP
  SELECT CASE PLD$
    CASE "U"
      IF FPOSY%>16 THEN
        IF FPOSY%-8 <32 THEN
          test_home
          IF slot%=0 THEN EXIT SUB
        ENDIF
        FPDIR%=1:FMOV%=4':FPOSP%=10
        Fjump 0,-8,0
      ENDIF
      'down
    CASE "D"
      IF PLD$="D" AND FPOSY%<200  THEN
        FPDIR%=2:FMOV%=4':FPOSP%=12
        Fjump 0,8,2
      ENDIF
      'left
    CASE "L"
      IF FPOSX%>8 THEN
        FPDIR%=3:FMOV%=4':FPOSP%=14
        FJump -8,0,4
      ENDIF
      'right
    CASE "R"
      IF FPOSX%<208 THEN
        FPDIR%=4:FMOV%=4':FPOSP%=16
        Fjump 8,0,6
      ENDIF
  END SELECT
END SUB
  
SUB FJump(FDX%,FDY%,o%)
  SETTICK 20,jump,1
  FRAMEBUFFER WRITE l
  BOX FPOSX%,FPOSY%,16,16,,0,0
  INC FPOSX%,FDX%
  INC FPOSY%,FDY%
  SPRITE WRITE FPOSP%+o%,FPOSX%,FPOSY%
END SUB
  
SUB move_lanes
  FRAMEBUFFER WRITE F
  LOCAL f AS INTEGER,co%,tst$
  ' Get a Lane, move it ld%() Pixels to
  ' Right if Positiv
  ' Left if negativ
  ' and fill the gap with Color co%
  FOR f=1 TO 10
    co%=(f<6)
    IF ld%(f)>0 THEN
      BLIT 0,ly%(f),ld%(f),ly%(f),224-ld%(f),16
      BOX 0,ly%(f),LD%(f),16,,col%(co%),col%(co%)
      INC Lps%(f),ld%(f)
      ' if Pixelsteps>= 16 Rotate the Lane$() Right
      IF Lps%(f)>=16 THEN
        Lane$(f)=RIGHT$(Lane$(f),1)+LEFT$(Lane$(f),LEN(Lane$(f))-1)
        Lps%(f)=0
      ENDIF
      tst$=LEFT$(Lane$(f),1)
      IF tst$<>"0" THEN
        'add Sprite to the lane
        snr%=INSTR("BrCRTtwWUu   kl m",tst$) '@harm
        SPRITE WRITE snr%,-12+Lps%(f),ly%(f)
      ENDIF
    ELSE
      BLIT -ld%(f),ly%(f),0,ly%(f),222,16 '@harm
      BOX 223+ld%(f),ly%(f),-ld%(f),16,,col%(co%),col%(co%)
      INC Lps%(f),-ld%(F)
      ' if Pixelsteps>= 16 Rotate the Lane$() Left
      IF Lps%(f)>=16 THEN
        Lane$(f)=RIGHT$(Lane$(f),LEN(Lane$(f))-1)+LEFT$(Lane$(f),1)
        Lps%(f)=0
      ENDIF
      tst$=MID$(Lane$(f),14,1)
      IF tst$<>"0" THEN
        'add Sprite to the lane
        snr%=INSTR("BrCRTtwWUu   kl m",tst$) '@harm
        BOX 222-Lps%(f),ly%(f),18,16,,col%(co%),col%(co%)
        SPRITE WRITE snr%,220-Lps%(f),ly%(f)
      ENDIF
    ENDIF
  NEXT f
END SUB

  'some Decoration
SUB flowers flx%,fly%,cl1%,cl2%
  BOX flx%,fly%,16,16,,COL%(13),COL%(13)
  flower flx%+1,fly%+3,cl1%,cl2%:flower flx%+0,fly%+10,cl1%,cl2%
  flower flx%+2,fly%+7,cl1%,cl2%:flower flx%+6,fly%+3,cl1%,cl2%
  flower flx%+6,fly%+8,cl1%,cl2%:flower flx%+6,fly%+8,cl1%,cl2%
  flower flx%+7,fly%+12,cl1%,cl2%:flower flx%+9,fly%+6,cl1%,cl2%
  flower flx%+12,fly%+3,cl1%,cl2%
END SUB
  
SUB Flower fox%,foy%,cl1%,cl2%
  LINE fox%,foy%,flx%+fox%+2,fly%+foy%,,cl1%
  LINE fox%+1,foy%-1,flx%+fox%+1,fly%+foy%+1,,cl1%
  PIXEL fox%+1,foy%,cl2%
END SUB
  
  'Street "BrCRTtwWUu   klm"
SUB Create_street
  LOCAL LNE%,lx%,tst$
  FRAMEBUFFER WRITE F
  disp
  FOR LNE%=1 TO 10
    FOR lx%=1 TO 13
      tst$=MID$(Lane$(LNE%),lx%,1)
      snr%=INSTR("BrCRTtwWUu   klm",tst$)'@harm
      IF snr%<>0 THEN SPRITE WRITE snr%,16*lx%,ly%(LNE%)
    NEXT lx%
  NEXT LNE%
END SUB
  
SUB Disp
  CLS
  BOX 0,0,224,112,,col%(1),col%(1)
  BOX 0,0,224,8,,COL%(2),COL%(2)
  BOX 0,8,8,16,,COL%(2),COL%(2)
  BOX 216,8,8,16,,COL%(2),COL%(2)
  BOX 24,8,32,16,,COL%(2),COL%(2)
  BOX 72,8,32,16,,COL%(2),COL%(2)
  BOX 120,8,32,16,,COL%(2),COL%(2)
  BOX 168,8,32,16,,COL%(2),COL%(2)
  BOX 0,104,224,16,,COL%(13),COL%(13)
  FOR f%=0 TO 218 STEP 16
    flowers f%,104,col%(1),Col%(4):flowers f%,200,col%(1),Col%(4)
  NEXT f%
END SUB
  
SUB read_Sprites
  LOCAL nr%,p%,n%,byt$,m$
  '--- Read/Create Sprites
  RESTORE Bull
  'because the Cars are vertical symmetric, reading half a Sprite is enough
  FOR nr%=1 TO 6
    FOR p%=1 TO 8
      READ Byt$:Byt$=expand$(Byt$)
      FOR n%=1 TO LEN(Byt$)
        m$=MID$(Byt$,n%,1)
        PIXEL n%-1,p%+119,COL%(VAL("&H"+m$))
        PIXEL n%-1,136-p%,COL%(VAL("&H"+m$)))
      NEXT n%
    NEXT p%
    SPRITE READ nr%,0,120,16,16
  NEXT nr%
  '2 wood (7/8)
  FOR nr%=7 TO 8
    FOR p%=1 TO 16
      READ Byt$:Byt$=expand$(Byt$)
      FOR n%=1 TO LEN(Byt$)
        PIXEL n%-1,p%+119,COL%(VAL("&H"+MID$(Byt$,n%,1)))
      NEXT n%
    NEXT p%
    SPRITE READ nr%,0,120,16,16
  NEXT nr%
  '@harm following sprites are new, diving turtles and such
  'sprites 9 = turtle, 10 primary diving turtle
  FOR nr%=9 TO 10
    FOR p%=1 TO 16
      READ Byt$:Byt$=expand$(Byt$)
      FOR n%=1 TO LEN(Byt$)
        PIXEL n%-1,p%+119,COL%(VAL("&H"+MID$(Byt$,n%,1)))
      NEXT n%
    NEXT p%
    SPRITE READ nr%,0,120,16,16
  NEXT nr%
  'sprites 11,12,13 are diving turtles for animations
  FOR nr%=11 TO 13
    FOR p%=1 TO 16
      READ Byt$:Byt$=expand$(Byt$)
      FOR n%=1 TO LEN(Byt$)
        PIXEL n%-1,p%+119,COL%(VAL("&H"+MID$(Byt$,n%,1)))
      NEXT n%
    NEXT p%
    SPRITE READ nr%,0,120,16,16
  NEXT nr%
  'sprites 14,15,16,17 are 4 parts of crocodile
  FOR nr%=14 TO 17
    FOR p%=1 TO 16
      READ Byt$:Byt$=expand$(Byt$)
      FOR n%=1 TO LEN(Byt$)
        PIXEL n%-1,p%+119,COL%(VAL("&H"+MID$(Byt$,n%,1)))
      NEXT n%
    NEXT p%
    SPRITE READ nr%,0,120,16,16
  NEXT nr%
  BOX 0,120,16,16,,0,0
  
  'Frog (sprite 18 - 25) same template but swapped colors
  'Read the Frog-Sprites and rotate in all Directions ;-)
  RESTORE FROG11
  FOR p%=1 TO 32
    READ Byt$:Byt$=expand$(Byt$)
    'swap colors Green>Cyan, Yello > Magena, Red > Green
    FOR n%=1 TO LEN(Byt$)
      m$=MID$(Byt$,n%,1)
      'If m$="A" Then m$="3" '@harm, these where the ladyfrog colors
      'If m$="6" Then m$="5"
      'If m$="4" Then m$="2"
      PIXEL n%-1,p%+119,COL%(VAL("&H"+m$))
      PIXEL 16+n%-1,152-p%,COL%(VAL("&H"+m$))
      PIXEL 32+p%-1,n%+119,COL%(VAL("&H"+m$))
      PIXEL 64-p%-1,n%+135,COL%(VAL("&H"+m$))
    NEXT n%
  NEXT p%
  SPRITE READ 18,0,120,16,16:SPRITE READ 19,0,136,16,16
  SPRITE READ 20,16,136,16,16:SPRITE READ 21,16,120,16,16
  SPRITE READ 22,32,120,16,16:SPRITE READ 23,48,120,16,16
  SPRITE READ 24,48,136,16,16:SPRITE READ 25,32,136,16,16
  
  'Lady Frog (sprite 32 - 39) same template but swapped colors
  'Read the Frog-Sprites and rotate in all Directions ;-)
  RESTORE FROG11
  FOR p%=1 TO 32
    READ Byt$:Byt$=expand$(Byt$)
    'swap colors Green>Cyan, Yello > Magena, Red > Green
    FOR n%=1 TO LEN(Byt$)
      m$=MID$(Byt$,n%,1)
      IF m$="A" THEN m$="3" 'ladyfrog colors
      IF m$="6" THEN m$="5"
      IF m$="4" THEN m$="2"
      PIXEL n%-1,p%+119,COL%(VAL("&H"+m$))
      PIXEL 16+n%-1,152-p%,COL%(VAL("&H"+m$))
      PIXEL 32+p%-1,n%+119,COL%(VAL("&H"+m$))
      PIXEL 64-p%-1,n%+135,COL%(VAL("&H"+m$))
    NEXT n%
  NEXT p%
  SPRITE READ 32,0,120,16,16:SPRITE READ 33,0,136,16,16
  SPRITE READ 34,16,136,16,16:SPRITE READ 35,16,120,16,16
  SPRITE READ 36,32,120,16,16:SPRITE READ 37,48,120,16,16
  SPRITE READ 38,48,136,16,16:SPRITE READ 39,32,136,16,16
  BOX 0,120,96,32,,0,0
  
  ' Frog_Home 26, Fly 27, 31=kroko
  FOR p%=1 TO 48
    READ Byt$:Byt$=expand$(Byt$)
    FOR n%=1 TO LEN(Byt$)
      PIXEL n%-1,p%+119,COL%(VAL("&H"+MID$(Byt$,n%,1)))
    NEXT n%
  NEXT p%
  SPRITE READ 26,0,120,16,16
  SPRITE READ 27,0,136,16,16
  SPRITE READ 31,0,152,16,16 '@harm, kroko
  
  RESTORE home_slot @harm
  'Home slot=28, green = 29, death = 30,
  BOX 0,120,96,32,,0,0
  FOR p%=1 TO 24
    READ Byt$:Byt$=expand$(Byt$)
    FOR n%=1 TO LEN(Byt$)
      PIXEL n%-1,p%+119,COL%(VAL("&H"+MID$(Byt$,n%,1)))
    NEXT n%
  NEXT p%
  SPRITE READ 28,0,120,32,24
  BOX 0,120,32,32,,0,0
  FOR p%=1 TO 24
    READ Byt$:Byt$=expand$(Byt$)
    FOR n%=1 TO LEN(Byt$)
      PIXEL n%-1,p%+119,COL%(VAL("&H"+MID$(Byt$,n%,1)))
    NEXT n%
  NEXT p%
  SPRITE READ 29,0,120,8,24
  BOX 0,120,32,32,,0,0
  FOR p%=1 TO 16
    READ Byt$:Byt$=expand$(Byt$)
    FOR n%=1 TO LEN(Byt$)
      PIXEL n%-1,p%+119,COL%(VAL("&H"+MID$(Byt$,n%,1)))
    NEXT n%
  NEXT p%
  SPRITE READ 30,0,120,16,16
  BOX 0,120,32,32,,0,0
  
  'test 64 sprites
  BOX 0,120,16,16,,col%(1),col%(1)
  SPRITE READ 63,0,120,16,16
  
END SUB
  
  '----------Audio subs--------
SUB bing
  LOCAL vol%
  FOR Vol%=25 TO 0 STEP -1
    'Play sound 4,"B","S",2700,Vol%
    PAUSE 20
  NEXT vol%
  PLAY STOP
END SUB
  
SUB start_sound
  LOCAL n%,f%
    FOR n%=1 TO 8:FOR f%=150+n%*50 TO 450+n%*50 STEP 20
      'Play sound 4,"B","Q",f%,25:Pause 6
  NEXT f%:NEXT n%
  'Play sound 4,"B","Q",1,0
END SUB
  
SUB DieSnd
  'Play sound 4,"B","Q",JP%,25
  INC JP%,-5
  IF JP%<65 THEN 'Play sound 4,"B","O",JP%,0
    SETTICK 0,0,1
    JP%=600
  ENDIF
END SUB
  
SUB jump
  'Play sound 4,"B","Q",JP%,25
  INC JP%,100
  IF JP%>1500 THEN 'Play sound 4,"B","O",JP%,0
    SETTICK 0,0,1
    JP%=600
  ENDIF
END SUB
  
SUB music
  LOCAL Fcent%
  IF mt%=1 THEN
    READ Freq1%,Freq2%
    IF Freq1%=99999 THEN RESTORE Main_song:READ Freq1%,Freq2%
    IF Freq1%=88888 THEN SETTICK 0,0,4:PLAY STOP:EXIT SUB
    
    'Pseudo Chorus
    Fcent%=INT(Freq1%/100)
    FL%=Freq1% - Fcent%
    FR%=Freq1% + Fcent%
  ENDIF
  IF Freq1%>1 THEN
    'Play sound 1,"L","q",FL%,adsr%(mt%):Play sound 3,"R","q",FR%,adsr%(mt%)
  ENDIF
  'If Freq2%>1  Then Play sound 2,"B","w",Freq2%,adsr2%(mt%)
  INC mt%:IF mt%>n_adsr% THEN mt%=1 '@harm different adsr
  IF f_merge% THEN FRAMEBUFFER merge 0,b : f_merge%=0 '@harm combine merge and sound
END SUB
  
FUNCTION expand$(pxl$)
  LOCAL n%,nmb%,tmp$,co$
  FOR n%=1 TO LEN(pxl$)
    IF ASC(MID$(pxl$,n%,1))< 71 THEN
      tmp$=tmp$+MID$(pxl$,n%,1)
    ELSE
      co$=HEX$(ASC(MID$(pxl$,n%,1))-71)
      INC n%:nmb%=VAL("&H"+MID$(pxl$,n%,1)):tmp$=tmp$+STRING$(nmb%+1,co$)
    ENDIF
  NEXT n%
  expand$=tmp$
END FUNCTION

SUB loadlevel n%
  LOCAL i%,dum$
  'stop music while loading
  IF SND% THEN SETTICK 0,0,4
  RESTORE levels
  IF n%>1 THEN 'skip lower levels
    FOR i%=1 TO 10*(n%-1) : READ dum$ : NEXT
  ENDIF
  'load level
  FOR i%=1 TO 10 : READ Lane$(i%) : NEXT
  'restart music
  RESTORE intro
  IF SND% THEN SETTICK beat%, music,4
END SUB

FUNCTION PadRead%()
  LOCAL INTEGER m% = 0
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
  
'for PicoPear
SUB init_kb
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
END SUB
  
FUNCTION rd_kb$()
  LOCAL s$ = "", x%
  x% = PadRead%()
  IF (x% AND PAD_UP)     THEN CAT s$, "U "
  IF (x% AND PAD_DOWN)   THEN CAT s$, "D "
  IF (x% AND PAD_LEFT)   THEN CAT s$, "L "
  IF (x% AND PAD_RIGHT)  THEN CAT s$, "R "
  IF (x% AND PAD_A)      THEN CAT s$, "A "
  'IF (x% AND PAD_B)      THEN Cat s$, "B "
  'IF (x% AND PAD_X)      THEN Cat s$, "X "
  'IF (x% AND PAD_Y)      THEN Cat s$, "Y "
  'IF (x% AND PAD_L)      then Cat s$, "L "
  'IF (x% AND PAD_R)      THEN Cat s$, "R "
  IF (x% AND PAD_SELECT) THEN CAT s$, "S "'SELECT
  'IF (x% AND PAD_START)  THEN Cat s$, "G "'START
  rd_kb$=s$
END FUNCTION
  
  '
  '
  '----------Data Section----------
  ' I try to store everything in this File to make it possible to run
  'without an SD Card
  '
colors:
  '--Colorscheme accordung to matherp
  DATA RGB(BLUE),RGB(GREEN),RGB(CYAN),RGB(RED)
  DATA RGB(MAGENTA),RGB(YELLOW),RGB(WHITE),RGB(MYRTLE)
  DATA RGB(COBALT) ,RGB(MIDGREEN),RGB(CERULEAN),RGB(RUST)
  DATA RGB(FUCHSIA),RGB(BROWN),RGB(LILAC)
  
  'Sprites
  'repainted the sprites mostly from this source
  'https://www.spriters-resource.com/resources/sheets/11/11067.png
  '
  'Compression of repeating Pixels:
  'if a Pixel repeats more than two times, Colors replaced
  'to Color +16  from '0123456789ABCDEF to GHIJKLMNOPQRSTUV
  'followed by the number (single Hex value) of repeats-1
Bull:
  '1=Bulldozer 1/2
  DATA "GF","GF","04747474G3N20","04747474G37400","002G2200I2N20","00N7007400"
  DATA "0N4227200N20","077I27277007400"
  '2=Racer1  1/2
  DATA "GF","0K4G9","0K4G2K400","0K4G2K400","G2DG6DG3","00MB00"
  DATA "T566T2M30","064646466T2M3"
  '3=car  1/2
  DATA "GF","GF","GF","G266G566G2","03T500T300","3T3J2D3T330"
  DATA "36DD33T3J2D00","3T233T3J2D00"
  '4=Racer4  1/2
  DATA "GF","G9K40","00K4G2K40","00K4G2K40","G32G62G2","00NC0"
  DATA "0N3I277I5","N3I2774047400"
  '5,6=Truck  1/2
  DATA "GF","GF","GF","G4I200I300","G24N30N6","004N40N6","004N44N6"
  DATA "004N44N6","GF","GF","GF","G8I2G3","NCG2","NCG2","NCG2","NCG2"
  '7=Wood1
  DATA "HF","HF","HF","U41U4N211","U67EEN41","U97U271","87U77U277"
  DATA "U87U47","U87EE7EE7","U4EEN2E7E77","EES77S271","S97CC771"
  DATA "S61S287711","HF","HF","HF"
  '8=Wood2
  DATA "HF","HF","HF","11U61U31E","U57U47EEE","EE77UB","UEE","U977U3"
  DATA "E77U37U57E","U67U7","U61C7S5","11C1SB","1EEECCC1S61","HF","HF","HF"
  ''9=Turtle 1
  'Data "HF","HF","H22H62H3","11I2H4I2H2","H32K42H4","H3K7H4","172K81121"
  'Data "I2K9211","172K674H3","H347K274H4","H324N243H4","11I2H4I2H2"
  'Data "H22H62H3","HF","HF","HF"
  ''new
  ''turtle0
  DATA "HF","HF","HF","H47H47H3","H3I17H22N1H2","H32K42H4","H3K6H4","172K8H3","I2K92H1"
  DATA "172K674H121","H347K274H4","H324N242H4","H3I17H22N1H2","H47H47H3","HF","HF"
  'turtle1
  DATA "HF","HF","HF","H22H62H3","H1I2H4I2H2","H32K42H4","H3K6H4","172K8H121","I2K92H1"
  DATA "172K674H3","H347K274H4","H324N242H4","H1I2H4I2H2","H22H62H3","HF","HF"
  'turtle2
  DATA "HF","HF","HF","HF","H32H42H4","H2I1K4I1H3","H3K6H4","H17K8H3","1I1K92H1"
  DATA "H17K674H3","H347K274H4","H2I14N24I1H3","H32H42H4","HF","HF","HF"
  'turtle3
  DATA "HF","HF","HF","H37HA","H27H67H3","H5I3H17H2","H4I5H4","H3I7H3","H3I572H3"
  DATA "H4I1N22H4","H5I3H5","H27H77H2","H37H57H3","HF","HF","HF"
  'turtle4
  DATA "HF","HF","H6N1H6","HF","HF","H47H27H5","H37H47H4","17H3I2H471","17H2I4H371"
  DATA "H5I17H6","H37H47H4","H47H27H5","HF","HF","H6N1H6","HF"
  'croqu1
  DATA "HF","HF","HF","HF","HF","HF","1CHD","1CH7CH2C1","1S1H2CH1S6","H1S17S47S4"
  DATA "H3S27S47S1","N1H5S7","H1N3H9","H9N3H1","H2N4H7","HBN3"
  'croqu2
  DATA "HF","HF","HF","HF","HF","H7CH3CH1","H1CH3S2H1C7C1","1SE","S67S7","S17SC"
  DATA "S97S4","7SB1S1","H3C7S1H7","N21S317H1N11C","H2S3H27H2S1","H3N5H3N1"
  'croqu3
  DATA "H8S1H4","H7C7S1H3","H7S3H3","H6S37H3","H5S2717H3","H1N1S317H5","H1N1S27H7","S517H7"
  DATA "CN1S1HA","S17S1H1717H5","S67S11717H1","SE1","S3HB","S317H27H5","S2H27H27H4"
  DATA "1N4H1N1H5"
  'croqu4
  DATA "HF","HF","HF","HF","HF","HF","H1N1S1H9","S1N1S3H4S11","SC7C1","S5N1S61"
  DATA "S7N61","SE1","S3HB","S317H27H5","S2H27H27H4","1N4H1N1H5"
  
FROG11:
  'Frog  Hop
  DATA "GF","GF","G2AG7AG2","00AA006A6600AA00","G2A04A66A40AG2","G2A0AA66AA0AG2"
  DATA "G3AM5AG3","G46AM3G4","G46AM3G4","G4A6A66AG4","G3Q266Q2G3","G2AAG5AAG2"
  DATA "00AAG7AA00","00AAG7AA00","G2AG7AG2","GF"
  'Frog Sit
  DATA "GF","GF","GF","G2A006A6600AG2","00AA04A66A40AA00","G2A0AA66AA0AG2"
  DATA "G2AAM5AAG2","G46AM3G4","G2AA6AM3AAG2","G2A0A6A66A0AG2"
  DATA "00AA00A66A00AA00","G2AG7AG2","GF","GF","GF","GF"
Fhome:
  'Frog Home Sprite
  DATA "H2AAH5AAH2","11A11AH3A11A11","11ACCQ5CCA11","H2Q9H2","H4Q5H4"
  DATA "H3AACAACAAH3","1A11AAACCAAA11A1","AAA13Q531AAA","Q33Q53Q3","Q333Q333Q3"
  DATA "Q4J5Q4","1Q33CCC33Q31","1Q33CCC33Q31","H2AAAJ3AAAH2","11Q3H3Q311"
  DATA "1AAA1AH3A1AAA1"
  'Fly
  DATA "HF","HF","HF","H6J4H3","H5J4H4","H2CC1J3CCH3"""
  DATA "H2CAAJ2S3H2","11AAAS7311","H2CAAJ2S3H2","H2CC1J3CCH3","H5J4H4"
  DATA "H6J4H3","HF","HF","HF","HF"
  'new
  'krok
  DATA "HF","HF","H7S3H3","H6S32H3","H5S2212H3","H1I1S312H5","H1I1S22H7","S512H7","CI1S1HA"
  DATA "S12S1H1212H5","3S52S11212H1","SE1","S3HB","S312H22H5","S2H22H22H4","1I4H1I1H5"
  'new
Home_slot:
  DATA "0I50I20I60I60I20","I20EI70EEI40EEI4022","20220EI20EE220220EE220220EE2202"
  DATA "I90I60I60I4","020EEIEI70EE2","I2022U220U220E20U220EEI2022"
  DATA "0I3EE1E1EE1E1EE1EE1E1E1EEI30","I4EHEH4EI4","I40EHF11EEI4"
  DATA "0EEI2HEH4220E22","20I2EEHF11E0220E0","I4EHEH4EI4","20EE2EHEH4EI4"
  DATA "220220EHF11EE22U2","I5HEH4I304","0I3EEHF11E0I4","I4EHEH4EI4"
  DATA "20EE2EHEH3EE20EE2","220220EHEH3E22022","0I4HEH4I40","I4EEHF11E0I4"
  DATA "U221EHEH4EE21EE","U22HEH7E1E11","HFHF"
  'green
  DATA "0I50","I4022","I7","20EEI3","220I30","I7","202200EE2","I4022","0I50"
  DATA "I4022","I7","20EEI3","220I30","I7","202200EE2","I4022","I20I3"
  DATA "I40EE","20EE2202","220I4","I7","E20U220","E1EE1E1E","H7"
  'Death
  DATA "G4M4G5","G3M6G4","G2660M2066G3","G260M406G3","660M80660"
  DATA "66G2M4G2660","G26G66G3","006G2M2G26G2","G36G46G4","G46G26G5"
  DATA "G5606G6","G66G60","G5606G6","G46G26G5","G266G466G3","G266G466G3"
  ' more to come
  '
  
Logo:
  
  DATA 1060110336,830472192,833872864,1060517424,808846896,808859184,808707040
  DATA 0,4261412864,3221225472,3235508720,4242742040,3234011928,3234011928
  DATA 3233935864, 24, 496
  
  
  ' ----- Music -----
  'Music data. Values are the Frequencies alternating Voice 1 and Voice 2
  '1 is muted,99999 marks the End of the Song
  
Yankee:
  DATA 741,185,741,233,829,139,932,233,741,185,932,233,829,139
  DATA 554,233,741,185,741,233,829,139,932,233,741,185,1,233,699,208
  DATA 554,139,741,185,741,233,829,139,932,233,988,185,932,247,829,156
  DATA 741,247,699,174,554,277,621,139,699,277,741,185,1,233,741,139,1,233
  DATA 621,185,699,247,621,156,554,247,621,185,699,247,741,156,1,247
  DATA 554,185,621,233,554,139,493,233,466,185,1,233,554,139,1,233
  DATA 621,185,699,247,621,156,554,247,621,185,699,247,741,156,621,247
  DATA 554,174,741,247,699,139,829,247,741,185,1,233,741,139,99999,99999
Mus2:
  DATA 1,233,1,233,1,1,1,233,1,1,1,233,1,1,1,233,350,1,466,233,1,1,466,233
  DATA 554,1,699,233,1,1,1,233,350,1,440,174,350,1,440,174,523,1
  DATA 621,174,1,1,1,174,350,1,440,174,1,1,440,174,523,1,621,174
  DATA  1,1,1,174,350,1,466,233,350,1,466,233,554,1,699,233,1,1
  DATA 1,233,350,1,466,233,1,1,466,233,554,1,699,233,1,1,1,233,350,1
  DATA 440,174,350,1,440,174,523,1,621,174,1,1,1,174,621,1,621,156,621,1
  DATA 699,1,621,1,554,156,554,1,621,1,554,1,523,174,523,1,554,1,621,1
  DATA 699,174,1,1,1,1,1,1,621,156,554,1,523,1,350,1,1,174,440,1
  DATA 466,1,1,1,1,233,1,1,99999,99999
Mus3:
  DATA 1,1,1,1,1,1,523,147,1,1,589,1,658,1
  DATA 699,131,1,1,880,1,782,1,699,165,1,1,523,147,699,165,658,174
  DATA 523,220,1,131,1,220,350,174,392,220,440,131,392,220,466,174,440,233
  DATA 392,147,350,233,466,174,523,233,589,147,523,233,621,174,589,220
  DATA  523,131,466,220,440,174,523,220,350,131,440,220,261,165,350,196
  DATA 220,131,261,196,330,147,261,196,392,165,330,196,523,174,392,220
  DATA 658,131,523,220,699,174,523,220,350,131,261,220,174,174,131,233
  DATA 87,147,65,233,58,174,87,233,117,147,174,233,233,174,350,233
  DATA 466,147,699,233,932,174,1177,233,1398,147,1865,233,1398,196,1177,261
  DATA 932,131,699,261,658,196,589,261,523,131,589,261,658,174,699,220
  DATA 782,131,932,220,880,174,782,220,699,131,589,220,523,174,440,233
  DATA 392,147,350,233,466,174,523,233,589,147,658,233,699,165,782,233
  DATA 699,131,589,233,658,165,589,233,523,131,466,233,440,174,392,220
  DATA 294,131,330,220,350,185,1,1,1,1,1,1,99999,99999
Main_song:
  DATA 1,1,589,392,1,1,493,1
  DATA 392,1,782,330,1,1,741,1,658,1,589,392,1,1,782,1,493,1
  DATA 440,370,589,1,1,1,1,1,1,196,589,247,589,147,589,247,589
  DATA 196,493,247,440,147,392,247,1,196,782,261,782,165,782
  DATA 261,881,196,782,261,741,165,658,261,589,196,493,247,1
  DATA 147,493,247,782,196,1,247,493,147,1,247,589,220,440,294
  DATA 1,147,1,294,1,165,1,294,1,185,1,294,1,196,493,247,493
  DATA 147,523,247,589,196,658,247,741,147,658,247,1,196,1,261
  DATA 1,165,1,261,1,196,1,261,1,165,1,261,523,196,523,261,589
  DATA 165,658,261,1,196,741,261,782,165,741,261,1,220,1,294,1
  DATA 147,1,294,1,220,589,294,1,147,782,294,1,196,1,247,1,147
  DATA 782,247,741,196,658,247,589,147,741,247,1,196,1,247,1
  DATA 156,658,247,1,165,658,261,1,220,589,261,1,220,881,294
  DATA 1,147,782,294,1,220,741,294,1,147,782,294,1,196,1,247,1
  DATA 147,1,247,1,196,1,1,1,1,782,1,782,261,782,1,782,1,782,1
  DATA 782,1,741,1,658,1,782,1,1,247,1,1,1,1,589,1,1,1,493,1
  DATA 493,1,440,1,1,1,440,1,440,1,782,1,782,1,741,1,658,1,658
  DATA 1,1,1,1,1,1,1,589,1,1,1,1,1,1,1,589,1,493,1,493,1,493,1
  DATA 493,1,1,1,440,1,392,1,523,1,493,1,523,1,589,1,658,1,1,1
  DATA 1,1,1,1,589,1,523,1,440,1,440,1,440,1,1,1,392,1,370,1
  DATA 392,1,370,1,392,1,440,1,493,1,1,1,1,1,1,1,589,1,493,1
  DATA 493,1,493,1,493,1,1,1,440,1,392,1,523,1,493,1,523,1,589
  DATA 1,658,1,1,1,741,1,658,1,589,1,1,1,589,1,658,1,589,1,523
  DATA 1,493,1,440,1,330,1,1,1,1,1,1,1,370,1,1,1,440,1,1,1,392
  DATA 1,1,1,1,1,1,1,99999,99999
intro:
  'Main song intro
  DATA 493,196,392,247,392,147,392,247,493,196,392,247,392
  DATA 147,392,247,523,220,523,261,493,147,493,261,440,220,1
  DATA 261,1,147,1,261,523,220,523,261,493,147,493,261,440
  DATA 220,440,261,658,147,658,261,589,220,523,261,493,147
  DATA 440,261,392,247,1,1,99999,99999
complete:
  'Level complete
  DATA 1,1,1,1,1,1,392,247,440,294,493,247
  DATA 523,294,589,247,1,294,493,247,1,294,392,247,440,294
  DATA 493,247,440,294,392,247,1,294,392,247,1,294,392,247
  DATA 440,294,493,247,523,294,589,247,1,294,493,247,1,294
  DATA 589,261,523,294,493,261,440,294,392,247,1,1,1,1
  DATA 1,1,99999,99999
Home1:
  'Frog-Home1
  DATA 1,1,1,1,881,741,990,699,881,741,1,1,741,589,1,1,881,741
  DATA 990,699,881,741,1,1,741,589,1,1,881,741,881,658,990,621
  DATA 1,1,1,1,881,621,782,658,554,699,741,589,1,1,1,1,1,1,440
  DATA 440,440,440,440,440,554,554,658,658,1,1,440,440,440,392
  DATA 440,370,589,440,741,589,1,1,881,741,881,658,990,589,1,1
  DATA 1,1,881,554,782,440,554,392,589,370,1,1,1,1,99999,99999
FKill:
  DATA 1,1,1,1,1,1,1,1,1,1,1,1,1,11,99999,99999
Game_OverSng:
  DATA 311, 156,311, 156,311, 156, 311, 156,440, 185,440, 185,1, 220,370, 220
  DATA 589, 185, 589, 185, 589, 220, 589, 220, 741, 147, 741, 147, 658, 220
  DATA 658, 220, 658, 185, 658, 185, 589, 220, 589, 220, 589, 165, 589, 165
  DATA 554, 220, 554, 220, 554, 196, 554, 196, 493, 220, 493, 220, 493, 147
  DATA  493,147,440,220,440,220,440,185,440,185,392,185,392,185,440,0,88888,88888
  
  'levels ----------------------------------------------------------------------
Levels:
  'level 1
  'legend W=log,w=log-end,U=turtle, T+t=truck,r,R=racer,B=bulldozer, C=car
  DATA "0WWWWw000WWWw00000WWWWw000WWw000"
  DATA "0UU000UUU000UUU000UU00U0000UU000"
  DATA "00WWWWWw000WWWWWw00WWWWw0000WWw0"
  DATA "00Ww0Ww000WWw000WWw0000WWWw00WWw"
  DATA "00UUU00UUU00UUU00UUU000000UUU0U0"
  DATA "000000Tt00000000Tt00000Tt000Tt00"
  DATA "000r000000r0000000000r000000r000"
  DATA "00C00000C00000C000000C0000C00000"
  DATA "00000000B00000B00000B0000B0000B0"
  DATA "0000R000000R00000R0000000R000R00"
  
  'level 2 : u=diving turtle, klm=crocodile
  DATA "0WWWWw000WWWw00000WWWWw000WWw000"
  DATA "0UU000uuu000UUU000UU00U000uuu000"
  DATA "00WWWWWw000WWWWWw0000klm0000klm0"
  DATA "00Ww0Ww000WWw000WWw0000WWWw00WWw"
  DATA "00uuu00UUU00uuu00UUU000000UUU0U0"
  DATA "000000Tt00000000Tt00000Tt000Tt00"
  DATA "000r000000r0000000000r000000r000"
  DATA "00C00000C00000C000000C0000C00000"
  DATA "00000000B00000B00000B0000B0000B0"
  DATA "0000R000000R00000R0000000R000R00"
  
  
  '-----------------------------------------------------------------------------
  ' Konami Style Font (Martin H.)
  ' Font type    : Full (95 ChArACtErs)
  ' Font size    : 8x8 pixels
  ' Memory usage : 764 Bytes
DEFINEFONT 9
  5F200808
  00000000 00000000 18181818 00180018 006C6C6C 00000000 367F3636 0036367F
  3E683F0C 00187E0B 180C6660 00066630 386C6C38 003B666D 0030180C 00000000
  3030180C 000C1830 0C0C1830 0030180C 3C7E1800 0000187E 7E181800 00001818
  00000000 30181800 7E000000 00000000 00000000 00181800 180C0600 00006030
  7E6E663C 003C6676 18183818 007E1818 0C06663C 007E3018 1C06663C 003C6606
  6C3C1C0C 000C0C7E 067C607E 003C6606 7C60301C 003C6666 180C067E 00303030
  3C66663C 003C6666 3E66663C 00380C06 18180000 00181800 18180000 30181800
  6030180C 000C1830 007E0000 0000007E 060C1830 0030180C 180C663C 00180018
  6A6E663C 003C606E 7E66663C 00666666 7C66667C 007C6666 6060663C 003C6660
  66666C78 00786C66 7C60607E 007E6060 7C60607E 00606060 6E60663C 003C6666
  7E666666 00666666 1818187E 007E1818 0C0C0C3E 00386C0C 70786C66 00666C78
  60606060 007E6060 6B7F7763 0063636B 7E766666 0066666E 6666663C 003C6666
  7C66667C 00606060 6666663C 00366C6A 7C66667C 0066666C 3C60663C 003C6606
  1818187E 00181818 66666666 003C6666 66666666 00183C66 6B6B6363 0063777F
  183C6666 0066663C 3C666666 00181818 180C067E 007E6030 6060607C 007C6060
  18306000 0000060C 0606063E 003E0606 42663C18 00000000 00000000 FF000000
  7C30361C 007E3030 063C0000 003E663E 667C6060 007C6666 663C0000 003C6660
  663E0606 003E6666 663C0000 003C607E 7C30301C 00303030 663E0000 3C063E66
  667C6060 00666666 18380018 003C1818 18380018 70181818 6C666060 00666C78
  18181838 003C1818 7F360000 00636B6B 667C0000 00666666 663C0000 003C6666
  667C0000 60607C66 663E0000 07063E66 766C0000 00606060 603E0000 007C063C
  307C3030 001C3030 66660000 003E6666 66660000 00183C66 6B630000 00367F6B
  3C660000 00663C18 66660000 3C063E66 0C7E0000 007E3018 7018180C 000C1818
  00181818 00181818 0E181830 00301818 00466B31 00000000 FFFFFFFF FFFFFFFF
END DEFINEFONT
