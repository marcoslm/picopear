  'PicoVaders
  '-------------------------------------
  '
  ' Martin Herhaus 2022
  ' V 0.9.8
  ' Concept based on the Game Space Invaders
  '  (c) 1978 by Tomohiro Nishikado of Taito,
  '
  ' Adapted to SpotPear board
  ' by Marcos LM 2025 - WIP

  '-----------------Init----------------
  
  ' Vars and Arrays
  DIM alien$(3,2) :'3 Alientypes with 2 Animantion states
  DIM aliens(55,4):'x,y,type,alive
  DIM ply$(3)
  DIM Bnk%(4,2,8):'4 Bunker, 2 Rows of 8 Blocks
  DIM A_Bomb%(10,4):'x,y,alive,owned_by
  'Dim Noise%(200)
  DIM Uxpl%(3)
  'Dim snd%(4) = (100,90,85,80,70)
  Udir%=1:Ux%=0:UA%=0
  USCR=50
  'UfoSndMin%=800:UfoSndMax%=1100:UfoSnd%=800:Ustp%=100
  anr%=55
  Myst%=0
  Score%=0:HighScore%=0
  mvsnd=0
  x_max=204:adir=1
  BX=0:BY=0:BA=0
  plx=103
  weiss=RGB(WHITE)
  Rot=RGB(RED)
  gelb=RGB(YELLOW)
  grn=RGB(GREEN)
  gry=RGB(GRAY)
  
  ' varias
  CONST H_CENTER = MM.HRES/2
  
  ' padlib
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
  
  'Setup User Defined Graphics
  RESTORE sr1
      FOR al=1 TO 3:FOR f=1 TO 2:bitm$="":FOR n=1 TO 16
      READ a:bitm$=bitm$+CHR$(a):NEXT n
    alien$(al,f)=bitm$:NEXT f
  NEXT al
  FOR f=1 TO 3
    bitm$=""
    FOR n=1 TO 16:READ a:bitm$=bitm$+CHR$(a):NEXT n:ply$(f)=bitm$
  NEXT f
  RESTORE xpld
  xpl$=""
  FOR n=1 TO 16:READ a:xpl$=xpl$+CHR$(a):NEXT n
  RESTORE ufo
  bitm$=""
  FOR n=1 TO 16:READ a:bitm$=bitm$+CHR$(a):NEXT n:uf1$=bitm$
  bitm$=""
  FOR n=1 TO 16:READ a:bitm$=bitm$+CHR$(a):NEXT n:uf2$=bitm$
  '
  'Generate Noise
  '
  'For f=1 To 200:noise%(f)=Int(Rnd*1000):Next f
  '
  '-------------------------------------
  '
  
coldstart:
  plx=103
  Y_Pos=48
  l=1
  tn=1
  Intro
  Level%=1
  Score%=0
  plyers%=3
start:
  tic=1
  bn%=1
  bmax%=2+INT(Level%/2):IF bmax%>10 THEN bmax%=10
  Setup_aliens
  'Draw Screen
  CLS
  BOX 10,229,220,1,,,grn
  TEXT 18,0,"SCORE <1>  HI-SCORE"
  TEXT 26,16,Pscore(Score%)
  TEXT 114,16,PScore(HighScore%)
  TEXT 6,230, STR$(Level%)
  draw_bunkers
  Levelup%=0
  ADth%=0
nxtPlyr:
  plHit%=0
  FOR f=1 TO 10
    IF A_Bomb%(f,3)=1 THEN
      LINE 10+A_Bomb%(f,1),A_Bomb%(f,2),10+A_Bomb%(f,1),A_Bomb%(f,2)+4,,0
    ENDIF
    A_Bomb%(f,3)=0
  NEXT f
  Bombs_out%=0
  BOX 32,232,40,8,1,0,0
  IF plyers%>1 THEN GUI BITMAP 32,232,ply$(1),16,8,1,grn,0
  IF plyers%=3 THEN GUI BITMAP 48,232,ply$(1),16,8,1,grn,0

  '------------------------------
  'Game Loop
  '
  DO
    ps=5 :'Timing
    move_single
    Draw_Player
    Move_Player
    Draw_Bullet
    Draw_Bomb
    IF NOT (tic MOD 16) THEN drop_bomb
    IF NOT (tic MOD 4) THEN Draw_Ufo
    INC tic
    start_ufo
    IF aDth%=1 THEN Levelup%=1:EXIT
    IF plHit% THEN EXIT
    IF A_Ground THEN Expl_Player:GameOver%=1: EXIT
    IF ps>0 THEN PAUSE ps
    INC bn%: IF bn%>Bmax% THEN bn%=1
  LOOP

  IF plHit% THEN
    Expl_Player
    INC plyers%,-1
    IF plyers%=0 THEN GameOver%=1:GOTO GmOv
    PAUSE 2000
    GOTO nxtPlyr
  ENDIF
  
  IF Levelup%=1 THEN
    INC Level%:PAUSE 2000
    IF Level%<6 THEN INC y_pos,8
    GOTO Start
  ENDIF

GmOV:
  IF gameover%=1 THEN
    TEXT H_CENTER,100," PLAYER<1> ","CT",2
    FOR f=1 TO 10
      TEXT H_CENTER,116," GAME OVER ","CT",2
      PAUSE 600
      TEXT H_CENTER,116,"           ","CT",2
      PAUSE 600
      IF INKEY$=" " THEN EXIT FOR
    NEXT f
    gameover%=0:level%=1:GOTO coldstart
  ENDIF
  '-------------------------------

SUB Intro
  CONST INTRO_SIDE = 20
  CONST INTRO_CLR_X = MM.HRES - 16     ' 224 en 240x240
  CONST INTRO_TX_MAX = MM.HRES - 2     ' 238 en 240x240
  CLS
  BOX 0,0,INTRO_SIDE,MM.VRES,,0,0
  BOX MM.HRES-INTRO_SIDE,0,INTRO_SIDE,MM.VRES,,0,0
  BOX 10,229,220,1,,,grn
  TEXT 106,30,"PLA"
  TEXT 136, 40, "Y", "I", 1
  PAUSE 600
  TEXT H_CENTER,50, "PICOVADERS","CT"
  PAUSE 600
  TEXT H_CENTER,70, "*SCORE ADVANCE TABLE*","CT"
  PAUSE 600
  GUI BITMAP 64,88,uf1$,16,8,1,rot,0
  TEXT 90,88, "= ? MYSTERY"
  PAUSE 600
  TEXT 90,108, "=30 POINTS"
  PAUSE 600
  GUI BITMAP 64,108,alien$(1,1),16,8,1,weiss,0
  PAUSE 600
  TEXT 90,128, "=20 POINTS"
  PAUSE 600
  GUI BITMAP 64,128,alien$(2,1),16,8,1,weiss,0
  PAUSE 600
  TEXT 90,148, "=10 POINTS"
  PAUSE 600
  GUI BITMAP 64,148,alien$(3,1),16,8,1,weiss,0
  TEXT H_CENTER,170, "(C) 1978 BY TAITO","CT"
  TEXT H_CENTER,190, "SPOTPEAR VERSION","CT"
  'TEXT H_CENTER,210, "2022 BY MARTIN HERHAUS","CT"
  PAUSE 2000
  FOR f=INTRO_CLR_X TO 138 STEP -1
    GUI BITMAP f,30,alien$(1,1+(f MOD 2)),16,8,1,weiss,0
    BOX INTRO_CLR_X,30,16,10,,0,0
    PAUSE 30
  NEXT f
  FOR f=138 TO INTRO_TX_MAX
    TEXT f, 40, "Y", "I", 1
    IF f<=INTRO_CLR_X THEN GUI BITMAP f,30,alien$(1,1+(f MOD 2)),16,8,1,weiss,0
    BOX INTRO_CLR_X,30,16,10,,0,0
    PAUSE 30
  NEXT f
  FOR f=238 TO 136 STEP -1
    TEXT f-6, 30, "Y", "", 1
    IF f<=INTRO_CLR_X THEN GUI BITMAP f,30,alien$(1,1+(f MOD 2)),16,8,1,weiss,0
    BOX INTRO_CLR_X,30,16,10,,0,0
    PAUSE 30
  NEXT f
  FOR f=138 TO INTRO_CLR_X
    GUI BITMAP f,30,alien$(1,1+(f MOD 2)),16,8,1,weiss,0
    BOX INTRO_CLR_X,30,16,10,,0,0
    PAUSE 30
  NEXT f
  
    ' --- Espera a A (start) o B (menu) ---
  LOCAL INTEGER now%, last%, press%
  ' Inicializa el estado para que no dispare por boton mantenido
  last% = PadRead%()
  'TEXT H_CENTER,210, "A - FIRE  B - MENU", "CT"
  DO
    now% = PadRead%()
    press% = now% AND (INV last%)
    last% = now%
    PAUSE 200
    TEXT H_CENTER,210, "                  ", "CT"
    PAUSE 150
    TEXT H_CENTER,210, "A - FIRE  B - MENU", "CT"
    IF (press% AND PAD_B) <> 0 THEN RUN "A:/menu.bas"
  LOOP UNTIL (press% AND PAD_A) <> 0
END SUB

SUB start_ufo
  ufo_x
  IF Myst% >20 AND Ua%=0 THEN
    ua%=1: Myst%=0
    USCR=50
    f=INT(RND*10): IF f>6 THEN USCR=100:IF f=9 THEN USCR=150
    udir%=2:UX%=0:IF INT(RND*2)=1 THEN udir%=-2:Ux%=204
  ENDIF
END SUB

SUB Drop_Bomb
  'Number of active bombs =Max then Get out
  IF Bombs_out%>=BMax% THEN EXIT SUB
  'start at the bottom right Alien
  INC ps,-3:'Timing
  aln%=56
test_next_alien:
  INC  aln%,-1
  '
  'Has the Countdown arrived 0? No bomb possible: Get out
  IF aln%=0 THEN INC ps,-1:EXIT SUB
  'active? no, test next alien..
  IF NOT aliens(aln%,4) THEN GOTO test_next_alien
  '---only one Bomb at once per Alien
  'no own bomb on the way yet? No, test next alien.
  FOR bn%=1 TO 10
    IF A_Bomb%(bn%,3)=1 AND A_Bomb%(bn%,4)=aln% THEN GOTO test_next_alien
  NEXT bn%
  
  'near the ship? no? Random(1/25) otherwise test next alien.
  IF aliens(aln%,1)<plx-8 OR aliens(aln%,1)>plx+8 AND INT(RND*25)>1 THEN
    GOTO test_next_alien
  ENDIF
  'And Int(Rnd*25)>1
  'if row less than 5: is there an active alien below me? Yes,test next alien.
  IF aln%<45 THEN IF aliens(aln%+11,4)=1 THEN GOTO test_next_alien
  'Drop the bomb
  bn%=1
  'find_free place in Array:
testSlot:
  IF A_Bomb%(bn%,3)=1 THEN
    INC bn%: IF bn%>10 THEN EXIT SUB
    GOTO testSlot
  ENDIF
  A_Bomb%(bn%,1)=aliens(aln%,1)+8
  A_Bomb%(bn%,2)=aliens(aln%,2)+6
  A_Bomb%(bn%,3)=1
  A_Bomb%(bn%,4)=aln%
  INC Bombs_out%
END SUB
  
SUB Draw_Bomb
  FOR f=1 TO 10
    IF A_Bomb%(f,3)=1 THEN
      'x,y,active
      'delete old position
      LINE 10+A_Bomb%(f,1),A_Bomb%(f,2),10+A_Bomb%(f,1),A_Bomb%(f,2)+4,,0
      INC A_Bomb%(f,2),1
      IF Hit_Bunker(A_Bomb%(f,1),A_Bomb%(f,2)+4) THEN
        A_Bomb%(f,3)=0
        INC Bombs_out%,-1
        EXIT SUB
      ENDIF
      IF A_Bomb%(f,2)>224 THEN
        A_Bomb%(f,3)=0
        INC Bombs_out%,-1
        EXIT SUB
      ENDIF
      'draw one bomb
      LINE 10+A_Bomb%(f,1),A_Bomb%(f,2),10+A_Bomb%(f,1),A_Bomb%(f,2)+4,,gelb
      'Hit bullet?
      IF BA THEN
        SELECT CASE A_Bomb%(f,1)
          CASE bx-2 TO bx+2
            SELECT CASE A_Bomb%(f,2)
              CASE by-4 TO by
                ba=0: A_Bomb%(f,3)=0
                INC Bombs_out%,-1
                expl 42+A_Bomb%(f,1),A_Bomb%(f,2),0
                EXIT SUB
            END SELECT
        END SELECT
      ENDIF
      'Hit Player?
      IF A_Bomb%(f,2) >210 THEN
        IF A_Bomb%(f,1) >=plx AND A_Bomb%(f,1)<plx+16 THEN plHit%=1
      ENDIF
      
    ENDIF
    LINE 10+bx,by,10+bx,by+4,,0
  ENDIF
NEXT f
END SUB

SUB Draw_Ufo
  IF Ua%=0 THEN INC ps,3:EXIT SUB
  'Play tone UfoSnd%,UfoSnd%,150
  INC UfoSnd%,Ustp%
  IF UfoSnD%=UfoSndMin% OR UfoSnd%>UfoSndMax% THEN Ustp%=-Ustp%
  BOX 10+ux%,32,16,10,,0,0
  INC Ux%,udir%
  IF Ux%>204 OR Ux%<0 THEN Ua%=0: EXIT SUB
  GUI BITMAP 10+Ux%,32,uf1$,16,8,1,rot,0
END SUB

SUB ufo_x
  IF Uxpl%(1) THEN
    INC Uxpl%(3)
    'Play tone 900+15*Uxpl%(3),900+15*Uxpl%(3),100
    IF uxpl%(3)=40 THEN TEXT 42+Uxpl%(2),32, USCR
    IF uxpl%(3)=60 THEN
      BOX 10+Uxpl%(2),32,32,10,,0,0
      Uxpl%(1)=0
      Uxpl%(3)=0
      INC Score%,USCR
      PRN_SCR
    ENDIF
  ENDIF
END SUB

SUB Draw_Bunkers
  FOR f=0 TO 3
    Draw_Bunker 40+F*45,184
    FOR n=1 TO 8
      Bnk%(f+1,1,n)=1
      Bnk%(f+1,2,n)=1
    NEXT n
  NEXT f
END SUB

SUB Draw_Bunker(bx,by)
  BOX bx  ,by+4,22,12,,grn,grn
  BOX bx+1,by+3,20,1 ,,grn,grn
  BOX bx+2,by+2,18,1 ,,grn,grn
  BOX bx+3,by+1,16,1 ,,grn,grn
  BOX bx+4,by  ,14,1 ,,grn,grn
  BOX bx+5,by+14,12,2,,0,0
  BOX bx+6,by+13,10,1,,0,0
  BOX bx+7,by+12,8,1,,0,0
END SUB

FUNCTION Hit_Bunker(TsX%,Tsy%)
  Hit_Bunker=0
  'Y in Range?
  SELECT CASE TsY%
    CASE 184 TO 200
      'Yes, X in Range of one of the 4 Bunkers?
      SELECT CASE TsX%
        CASE 30 TO 51
          'Bunker1
          bhy=INT((Tsy%-184)/8)
          bhx=1+INT((TsX%-30)/3)
          IF Bnk%(1,bhy,bhx)=1 THEN
            Bnk%(1,bhy,bhx)=0
            Hit_Bunker=1
            LINE 10+TsX%,Tsy%,10+TsX%,TsY%+4,,0
            debunk 10+TsX%,tsy%
            'BA=0
          ENDIF
        CASE 75 TO 96
          'Bunker2
          bhy=INT((TsY%-184)/8)
          bhx=1+INT((TsX%-75)/3)
          IF Bnk%(2,bhy,bhx)=1 THEN
            Bnk%(2,bhy,bhx)=0
            Hit_Bunker=2
            LINE 10+TsX%,Tsy%,10+TsX%,TsY%+4,,0
            debunk 10+TsX%,TsY%
            'BA=0
          ENDIF
          
        CASE 120 TO 141
          'Bunker3
          bhy=INT((TsY%-184)/8)
          bhx=1+INT((TsX%-120)/3)
          IF Bnk%(3,bhy,bhx)=1 THEN
            Bnk%(3,bhy,bhx)=0
            Hit_Bunker=3
            LINE 10+TsX%,Tsy%,10+TsX%,TsY%+4,,0
            debunk 10+TsX%,TsY%
            'BA=0
          ENDIF
          
        CASE 165 TO 186
          'Bunker4
          bhy=INT((TsY%-184)/8)
          bhx=1+INT((TsX%-165)/3)
          IF Bnk%(4,bhy,bhx)=1 THEN
            Bnk%(4,bhy,bhx)=0
            Hit_Bunker=4
            LINE 10+TsX%,Tsy%,10+TsX%,TsY%+4,,0
            debunk 10+TsX%,TsY%
          ENDIF
      END SELECT
  END SELECT
END FUNCTION

SUB debunk zx,zy
  ' Destroy part of the Bunker
  FOR xf= 1 TO 40
    PIXEL zx-3+RND*8,zy-5+RND*8,0
  NEXT xf
  INC ps,-3
END SUB

SUB Draw_Bullet
  IF NOT BA THEN INC ps,4:EXIT SUB :'No Bullet active
  LINE 10+bx,by,10+bx,by+4,,0
  INC BY,-2
  IF BY<=32 THEN BA=0:EXIT SUB
  LINE 10+bx,by,10+bx,by+4,,weiss
  IF BY MOD 8 THEN EXIT SUB :' to speed up, do the Test only every 8 Pixel
  IF collition1(bx,by) THEN
    LINE 10+bx,by,10+bx,by+4,,0
    BA=0
    'last Alien?
    ADth%=aldeath()
  ENDIF
  IF Hit_Bunker(bx,by) THEN BA=0:EXIT SUB
  'ufo?
  IF ua% THEN
    SELECT CASE by
      CASE 32 TO 40
        SELECT CASE bx
          CASE ux% TO ux%+15
            'expl ufo
            Uxpl%(1)=1:Uxpl%(2)=ux%:Uxpl%(3)=0
            GUI BITMAP 10+ux%,32,uf2$,16,8,1,rot,0
            ua%=0
        END SELECT
    END SELECT
  ENDIF
END SUB

FUNCTION collition1(blx,bly)
  collition1 =0
  SELECT CASE bly
    CASE ypos+16 TO 214
      'Alien?
      FOR f=1 TO 55
        IF aliens(f,4)<>0 THEN
          ax=aliens(f,1):ay=aliens(f,2)
          SELECT CASE blx
            CASE ax+1 TO ax+13
              SELECT CASE bly
                CASE ay TO ay+7
                  collition1=1
                  expl ax+10,ay,1
                  aliens(f,4)=0
                  INC Score%,40-(10*Aliens(f,3))
                  PRN_SCR
                  EXIT FUNCTION
              END SELECT
          END SELECT
        ENDIF
      NEXT f
  END SELECT
END FUNCTION

SUB PRN_SCR
  TEXT 26,16,Pscore(Score%)
  IF Score%>HighScore% THEN
    Highscore%=Score%
    TEXT 114,16,PScore(HighScore%)
  ENDIF
END SUB

FUNCTION PScore (wert) AS STRING
  IF wert<1000 THEN PScore = "0"
  IF wert<100 THEN PScore = "0"
  IF Wert<10 THEN PScore = "0"
  PScore = STR$(wert)
END FUNCTION

' Explode Alien or Bomb
SUB expl ex,ey,snd
  GUI BITMAP ex,ey,xpl$,16,8,1,gelb,0
  Draw_Ufo
  IF snd=1 THEN
    FOR nse=1 TO 75
      'Play tone noise%(nse),noise%(nse),2
      PAUSE 1
    NEXT nse
    'Play tone 0,0,1
  ELSE
    PAUSE 20
  ENDIF
  Draw_Ufo
  BOX ex,ey,16,10,,0,0
END SUB

SUB Move_Player
  LOCAL INTEGER now%
  now% = PadRead%()
  ' Movimiento
  IF (now% AND PAD_LEFT)  <> 0 THEN plx = plx - 1
  IF (now% AND PAD_RIGHT) <> 0 THEN plx = plx + 1
  ' Clamps
  IF plx < 16  THEN plx = 16
  IF plx > 188 THEN plx = 188
  ' Disparo
  IF BA = 0 THEN
    IF (now% AND PAD_A) <> 0 THEN
      IF NOT UA% THEN INC Myst%, INT(RND*3)
      BA = 1 : BX = plx + 7 : BY = 210
      FOR f = 1000 TO 1 STEP -50
        'Play tone 1000+f,1000+f,5
        PAUSE 2
      NEXT f
    ENDIF
  ENDIF
END SUB

SUB Draw_Player
  GUI BITMAP 10+plx,214,ply$(1),16,8,1,grn,0
END SUB

SUB Expl_Player
  'show Player Explosion
  FOR f=1 TO 3
    GUI BITMAP 10+plx,214,ply$(2),16,8,1,grn,0
    FOR nse=1 TO 100
      'Play tone noise%(nse),noise%(nse),2
      PAUSE 1
    NEXT nse
    GUI BITMAP 10+plx,214,ply$(3),16,8,1,grn,0
    FOR nse=100 TO 200
      'Play tone noise%(nse),noise%(nse),2
      PAUSE 1
    NEXT nse
  NEXT f
  FOR nse=1 TO 200
    'Play tone noise%(nse),noise%(nse),2
    PAUSE 1
  NEXT nse
  'Play tone 0,0,1
  PAUSE 500
  BOX 10+plx,214,16,10,,0,0
END SUB

SUB Setup_aliens
  'Fills the array of Aliens with x,y,type and Live Values
  num=1
  A_Ground=0
  Num_aliens%=55
  FOR R=1 TO 5
    at=3: ' Alien Type
    IF R=1 THEN at=1
    IF R=2 OR R=3 THEN at=2
    FOR n=1 TO 11
      aliens(num,1)=n*16
      aliens(num,2)=Y_Pos+R*16
      aliens(num,3)=at
      aliens(num,4)=1
      INC num
    NEXT n
  NEXT R
  trn=0
  'wait%=80
END SUB

SUB Draw_aliens
FOR f=55 TO 1 STEP -1
  ax=10+aliens(f,1)
  ay=aliens(f,2)
  at=aliens(f,3)
  IF aliens(f,4) THEN
    BOX ax,ay,16,10,,0,0
    GUI BITMAP ax,ay,alien$(at,l),16,8,1,weiss,0
  ENDIF
NEXT f
END SUB

SUB move_single
  ' Move a Single Alien one Step
  ' Aliens are counted from bottom right to top left
mslife:
  'Is this alien alive
  IF aliens(anr%,4) THEN
    ax=aliens(anr%,1)
    ay=aliens(anr%,2)
    at=aliens(anr%,3)
    BOX ax+10,ay,16,10,,0,0
    INC ax,Adir
    GUI BITMAP ax+10,ay,alien$(at,l),16,8,1,weiss,0
    aliens(anr%,1)=ax
    IF ax >=x_max OR ax <1 THEN trn=1
  ELSE
    'move to previous alien
    INC anr%, -1
    IF anr%<1 THEN
      anr%=55
      IF trn=1 THEN Adir=-Adir:down_aliens:Draw_aliens:trn=0
      INC mvsnd: mvsnd=mvsnd AND 3
      'Play Sound if all Aliens move a Step.. but not if Ufo is out
      'If Ua%=0 Then Play TONE snd%(mvsnd+1),snd%( mvsnd+1), 80
      INC l:IF l=3 THEN l=1:'Flip Alien Sprites (one or two)
    ENDIF
    GOTO mslife
  ENDIF
  INC anr%,-1
END SUB

SUB move_aliens
  trn=0
  INC mvsnd: mvsnd=mvsnd AND 3
  'Play TONE snd%(mvsnd+1),snd%( mvsnd+1), 80
  FOR f=55 TO 1 STEP -1
    IF aliens(f,4) THEN
      ax= aliens(f,1)
      INC ax,Adir
      aliens(f,1)=ax
      IF ax >=x_max OR ax <1 THEN trn=1
    ENDIF
  NEXT f
  IF trn=1 THEN Adir=-Adir:down_aliens
END SUB

SUB down_aliens
  ' Moves all Aliens Down by 8 Pixel
  FOR f=55 TO 1 STEP -1
    IF aliens(f,4) THEN
      ax= aliens(f,1):ay=aliens(f,2)
      BOX ax+10,ay,16,10,,0,0
      aliens(f,2)=ay+8
      IF ay+8>=202 THEN A_Ground=1
    ENDIF
  NEXT f
END SUB

FUNCTION aldeath()
  'counts number of Aliens in Num_Aliens%,
  'returns 1 if no Alien left, else 0
  Num_aliens%=0
  aldeath=1
  FOR f=1 TO 55
    INC Num_aliens%,aliens(f,4)
  NEXT f
  IF Num_aliens% THEN aldeath=0
END FUNCTION

' padlib
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

'----UDG----
sr1:
DATA  1, 128, 3, 192, 7, 224, 13, 176, 15, 240, 5, 160, 8, 16, 4, 32
DATA  1, 128, 3, 192, 7, 224, 13, 176, 15, 240, 2, 64, 5, 160, 10, 80
sr2:
DATA  8, 32, 4, 64, 15, 224, 27, 176, 63, 248, 47, 232, 40, 40, 6, 192
DATA  8, 32, 36, 72, 47, 232, 59, 184, 63, 248, 31, 240, 8, 32, 16, 16
sr3:
DATA  7, 224, 31, 248, 63, 252, 57, 156, 63, 252, 14, 112, 25, 152, 48, 12
DATA  7, 224, 31, 248, 63, 252, 57, 156, 63, 252, 14, 112, 25, 152, 12, 48
plyr:
DATA  1, 0, 3, 128, 3, 128, 63, 248, 127, 252, 127, 252, 127, 252, 127, 252
DATA  2, 0, 0, 16, 2, 160, 18, 0, 1, 176, 69, 168, 31, 228, 63, 245
DATA  16, 4, 130, 25, 16, 192, 2, 2, 75, 97, 33, 196, 31, 224, 55, 228
xpld:
DATA  4, 64, 34, 136, 16, 16, 8, 32, 96, 12, 8, 32, 18, 144, 36, 72
ufo:
DATA  0, 0, 7, 224, 31, 248, 63, 252, 109, 182, 255, 255, 57, 156, 16, 8
ufo_xpl:
DATA  148, 10, 64, 48, 143, 24, 31, 206, 58, 167, 143, 140, 5, 24, 136, 136
