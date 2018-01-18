*********************************************************************
****  Sistema......: SINCON - Sistema Integrado de Controle      ****
****  Versao.......: FAZENDA & HARAS HARMONIA                    ****
****  Programa.....: SINCON04.PRG                                ****
****  Finalidade...: Funcoes de Uso Geral do Sistema             ****
****  Programador..: Juliano Ribeiro                             ****
****                                                             ****
****                 Copyright(C) SERIAL SOFTWARE                ****
****                                                             ****
****  Compilador...: CLIPPER (R) Versao 5.01                     ****
****  Linkeditor...: RTLink (R)  Versao 3.13                     ****
*********************************************************************
#INCLUDE "INKEY.CH"

***********************************
function LIMPA_VAR(LIN, COL, SPACE)
***********************************

if pcount() == 2
   @ LIN, COL say space(05)
else
   @ LIN, COL say space(SPACE)
endif

return .t.

*****************
function VER_DATE
*****************

WVAR = readvar()

if empty(&WVAR)

   return .t.
endif

WDT_TEST = ctod(transform(&WVAR,"@R 99/99")+"/"+right(str(year(date()),4),2))

if empty(WDT_TEST)
   return .f.
else
   return .t.
endif

return .t.

*****************
function VER_HORA
*****************

WVAR = readvar()

if empty(&WVAR)
   return .t.
endif

if (left(&WVAR,2) <= "23" .and. left(&WVAR,2) >= "00") .and.;
   (right(&WVAR,2) <= "59" .and. right(&WVAR,2) >= "00")

   return .t.
endif

return .f. 

*****************
function CHCK_SEQ
*****************

WREGISTRO = recno()

do while .t.
   seek WSEQ

   if recno() == WREGISTRO
      exit
   endif

   ordsetfocus('SINC001')
   go bottom

   WSEQ    = SEQ + 1

   go WREGISTRO

   if ! reglock()
      loop
   endif
   
   replace SEQ    with WSEQ
      
   grava()
enddo

return .t.

********************
function CHCK_CODIGO
********************

WREGISTRO = recno()

do while .t.

   seek WCODIGO

   if recno() == WREGISTRO
      exit
   endif

   ordsetfocus('SINC001')
   go bottom

   WCODIGO    = CODIGO + 1

   go WREGISTRO

   if ! reglock()
      loop
   endif
   
   replace CODIGO    with WCODIGO
      
   grava()
   
enddo

return .t.

*****************
function PROTETOR
*****************
local WP_A, WP_I, WP_C, WP_MEM, WP_SINCON

WP_A   = 0
WP_I   = 1
WP_MEN = alltrim(CFG_MENS)+space(60)

WP_SINCON = savescreen(00,00,24,79)

if val(right(time(),2)) <= 3
   WP_C = "01"
elseif val(right(time(),2)) <= 7
   WP_C = "02"
elseif val(right(time(),2)) <= 11
   WP_C = "03"
elseif val(right(time(),2)) <= 15
   WP_C = "04"
elseif val(right(time(),2)) <= 19
   WP_C = "05"
elseif val(right(time(),2)) <= 23
   WP_C = "06"
elseif val(right(time(),2)) <= 27
   WP_C = "07"
elseif val(right(time(),2)) <= 31
   WP_C = "08"
elseif val(right(time(),2)) <= 35
   WP_C = "09"
elseif val(right(time(),2)) <= 39
   WP_C = "10"
elseif val(right(time(),2)) <= 43
   WP_C = "11"
elseif val(right(time(),2)) <= 47
   WP_C = "12"
elseif val(right(time(),2)) <= 51
   WP_C = "13"
elseif val(right(time(),2)) <= 55
   WP_C = "14"
elseif val(right(time(),2)) <= 59
   WP_C = "15"
endif

setcolor(WP_C+"/0")
@ 00,00 clear to 24,79

do while .t.
   @ 12,69-WP_A say right(left(WP_MEN,WP_I),60)

   WP_I = WP_I + 1
   WP_A = WP_A + 1

   if WP_A > 59               
      WP_A = 59
   endif

   if WP_I > len(WP_MEN)
      WP_I = 1
      WP_A = 0
   endif

   if inkey(.2) <> 0
      restscreen(00,00,24,79,WP_SINCON)
      setcolor(COR_TEXT)
      return .t.
   endif
enddo

setcolor(COR_TEXT)

return .t.

******************
function VIEW_USER
******************

if KEY == 278 .and. SENHA_MESTRE
   WTT_USUARIO = savescreen(08,14,17,67)                  
        
   sombra(09,16,17,67,8)
   setcolor(COR_CONS)
   @ 08,14 clear to 16,65
   @ 08,14 to 16,65 double

   @ 10,16 say "ARQUIVO DE BANCO DE DADOS: "+dbf()
   @ 11,16 say "              Registro N"+chr(167)+": "+strzero(recno(),6)+"/"+strzero(lastrec(),6)
   @ 12,16 say "       Ultima atualiza"+chr(135)+chr(132)+"o: "+dtoc(DATE)
   @ 13,16 say "                  Horario: "+TIME
   @ 14,16 say "                  Usu"+chr(160)+"rio: "+USUARIO

   @ 08,15 say strzero(day(date()),2)+"/"+upper(left(mes(month(date())),3))+"/"+right(strzero(year(date()),4),2)

   do while inkey() = 0
      @ 16,57 say time()
   enddo      
                  
   setcolor(COR_TEXT)             
   restscreen(08,14,17,67,WTT_USUARIO)
endif

KEY = lastkey()

return .t.

*****************************
function BUSCA_SOBRENOME(VAR)
*****************************

X    = 0

for I = 1 to len(alltrim(VAR)) 
    if substr(alltrim(VAR),len(alltrim(VAR))-I,1) == space(01)
       exit
    endif

    X = X + 1
next  

return(substr(alltrim(VAR),len(alltrim(VAR))-X,X+1))


*****************
function CHCK_CGC
*****************

private D[12], WCPF, DF1, RESTO, PRIDIG, SEGDIG
parameters WCPF

if len(alltrim(WCPF)) < 11
   set cursor off
   centro(24,"CPF/CGC Invalido...")
   inkey(1)
   @ 24,10 clear to 24,70
   set cursor on

   return .f.
endif   

if alltrim(WCPF) = replicate("9",14) .or. alltrim(WCPF) = replicate("9",11)
   return .f.
endif

afill(D,0)

for I = 1 to len(alltrim(WCPF))-2
    D[I] = val(substr(WCPF,I,1)) 
next

if len(alltrim(WCPF)) > 11
   DF1 = 5*D[1] + 4*D[2] + 3*D[3] + 2*D[4] + 9*D[5] + 8*D[6] + 7*D[7] + 6*D[8] + 5*D[9] + 4*D[10] + 3*D[11] + 2*D[12]
else
   DF1 = 10*D[1] + 9*D[2] + 8*D[3] + 7*D[4] + 6*D[5] + 5*D[6] + 4*D[7] + 3*D[8] + 2*D[9]
endif

RESTO = mod(DF1,11)

if RESTO = 0 .or. RESTO = 1
   PRIDIG = 0
else
   PRIDIG = 11 - RESTO
endif

if len(alltrim(WCPF)) > 11
   DF1 = 6*D[1] + 5*D[2] + 4*D[3] + 3*D[4] + 2*D[5] + 9*D[6] + 8*D[7] + 7*D[8] + 6*D[9] + 5*D[10] + 4*D[11] + 3*D[12] + 2*PRIDIG 
else
   DF1 = 11*D[1] + 10*D[2] + 9*D[3] + 8*D[4] + 7*D[5] + 6*D[6] + 5*D[7] + 4*D[8] + 3*D[9] + 2*PRIDIG 
endif

RESTO = mod(DF1,11)

if RESTO = 0 .or. RESTO = 1
   SEGDIG = 0
else
   SEGDIG = 11 - RESTO
endif

if PRIDIG <> val(substr(WCPF,len(alltrim(WCPF))-1,1)) .or. SEGDIG <> val(substr(WCPF,len(alltrim(WCPF)),1))
   set cursor off
   centro(24,"CPF/CGC Invalido...")
   inkey(1)
   @ 24,10 clear to 24,70
   set cursor on
   return .f.
endif

return .t.


************
function FIM
************

setcursor(0)
setcolor("W/N")
//SHOWKEY()

if right(time(),1) == "0"
   for I = 0 to 40                         && fecha vertical
       WPARTE1 = savescreen(00,I,24,38)
       WPARTE2 = savescreen(00,41,24,79-I)

       @ 00,I clear to 24,I
       @ 00,79-I clear to 24,79-I
    
       restscreen(00,01+I,24,39,WPARTE1)
       restscreen(00,40,24,78-I,WPARTE2)
   next   
elseif right(time(),1) == "1"
   for I = 0 to 38                         && abre vertical
       WPARTE1 = savescreen(00,01,24,39-I)
       WPARTE2 = savescreen(00,40+I,24,78)

       @ 00,39-I clear to 24,40+I
    
       restscreen(00,00,24,38-I,WPARTE1)
       restscreen(00,41+I,24,79,WPARTE2)
   next   
elseif right(time(),1) == "2"
   for I = 0 to 78                         && sai pra esquerda
       WPARTE1 = savescreen(00,01,24,79-I)

       @ 00,79-I clear to 24,79
    
       restscreen(00,00,24,78-I,WPARTE1)
   next   
elseif right(time(),1) == "3"
   for I = 0 to 78                         && sai pra direita
       WPARTE1 = savescreen(00,00+I,24,78)

       @ 00,00 clear to 24,00+I
    
       restscreen(00,01+I,24,79,WPARTE1)
   next   
elseif right(time(),1) == "4"
   for I = 0 to 24                        && sai pra cima
       scroll(00,00,24,79,1)
       inkey(.05)
   next   
elseif right(time(),1) == "5"
   for I = 0 to 24                        && sai pra baixo
       scroll(00,00,24,79,-1)
       inkey(.05)
   next   
elseif right(time(),1) == "6"
   for I = 0 to 12                       && fecha Horizontal
       scroll(00,00,11,79,-1)
       scroll(12,00,24,79,1)
       inkey(.05)
   next   
elseif right(time(),1) == "7"
   for I = 0 to 12                       && abre horizontal
       scroll(00,00,11,79,1)
       scroll(12,00,24,79,-1)
       inkey(.05)
   next   
elseif right(time(),1) == "8"
   for I = 0 to 78                         && sai menu pra direita
       WPARTE1 = savescreen(00,01,04,79-I)
       WPARTE2 = savescreen(23,01,24,79-I)
       WPARTE3 = savescreen(05,00+I,22,78)

       @ 00,79-I clear to 04,79
       @ 23,79-I clear to 24,79
       @ 02,00 clear to 22,00+I
    
       restscreen(00,00,04,78-I,WPARTE1)
       restscreen(23,00,24,78-I,WPARTE2)
       restscreen(05,01+I,22,79,WPARTE3)
   next   
elseif right(time(),1) == "9"
   for I = 0 to 78                         && sai pra esquerda
       WPARTE1 = savescreen(00,00+I,04,78)
       WPARTE2 = savescreen(23,00+I,24,78)
       WPARTE3 = savescreen(05,01,22,79-I)

       @ 00,00 clear to 04,00+I
       @ 23,00 clear to 24,00+I
       @ 05,79-I clear to 22,79
    
       restscreen(00,01+I,04,79,WPARTE1)
       restscreen(23,01+I,24,79,WPARTE2)
       restscreen(05,00,22,78-I,WPARTE3)
   next   
endif

@ 00,00 clear to 24,79

for I = 79 to 01 step -1
    @ 01,I  say left("SINCON - Vers"+chr(132)+"o 2.0          ",80-I)
    @ 02,I  say left("Sistema Integrado de Controle           ",80-I)
    @ 03,I  say left("Copyright(C) Sr. MARCIANO, Jun/12       ",80-I)
    @ 04,I  say left("All Rights Reserved.                    ",80-I)
    @ 05,I  say left("                                        ",80-I)
    @ 06,I  say left("Uso exclusivo: FAZENDA & HARAS HARMONIA ",80-I)
next

@ 08,00    say ""

setcolor("W/N")
setcursor(1)
dbcloseall()
quit

return .t.


***************
function NOCOPY
***************
private SERIE, SERIA_A
parameters SERIE

! vol > SERIE1.TXT
SERIE_A = memoread("SERIE1.TXT")
ERASE("SERIE1.TXT")
SERIE_A = memoline(SERIE_A,"",03)

if SERIE $ SERIE_A
   return .t.
else
   return .f.
endif

*******************
function NOCOPYREDE
*******************
private SERIE, SERIE_A
parameters SERIE

SERIE_A = memoread("\NWCLIENT\AMSESAC.DSF")
SERIE_A = memoline(SERIE_A,"",03)

if SERIE $ SERIE_A
   return .t.
else
   return .f.
endif

***************
function CENTRO
***************

private LLIN, STRING, COLUNA
parameters LLIN, STRING

COLUNA = 0

COLUNA = int(40 - (len(STRING)/2)) 
@ LLIN,COLUNA say STRING

return .t.

************
function MES
************
private MES   
parameters MES

MES = MES * 9 - 8

return alltrim(substr("Janeiro  "+"Fevereiro"+"Marco    "+"Abril    "+"Maio     ";
                     +"Junho    "+"Julho    "+"Agosto   "+"Setembro "+"Outubro  ";
                     +"Novembro "+"Dezembro ",Mes,9))

********************
function SEMANA(WDT)
********************

if valtype(WDT) = "D"
   WDT = dow(WDT) * 13 - 12
else
   WDT = WDT * 13 - 12
endif

return substr("Domingo      "+"Segunda-Feira"+"Terca-Feira  "+"Quarta-Feira ";
             +"Quinta-Feira "+"Sexta-Feira  "+"Sabado       ",WDT,13)

*********************
function EXTENSO(num)
*********************

local NUM0
local VEZES
local WEXTENSO
local WEXTENSO2
local WEXTENSO3
local WEXTENSO4
local WEXTENSO5
local NUMERO_P
local CADEIA_P
local PRIM_CENT
local PASSADAS
local POSICAO
local COD
local PESQUISAR2
local PESQUISAR
local NUM1
local NUM2 
local NUM3
local NUM4
local NUM5
local I
local ONZE_DES
local PESQ_COMP
local J
local PESQ_CENT

AREA = dbf()

set softseek off

select EXTENSO

NUM  = abs(NUM)
NUM0 = NUM

VEZES = lennum(NUM ) / 5

WEXTENSO2 = space(01)
WEXTENSO3 = space(01)
WEXTENSO4 = space(01)
WEXTENSO5 = space(01)

NUMERO_P  = NUM
CADEIA_P  = str(NUM,15,2) 
WEXTENSO  = space(01)
PRIM_CENT = .t.     

for I = 1 to 5
    PASSADAS = 0
    POSICAO  = 03

    COD        = 0
    PESQUISAR2 = space(01)

    if I == 1
       CADEIA = subst(CADEIA_P,10,3)
       NUM1   = val(CADEIA)
    elseif I == 2
       CADEIA = subst(CADEIA_P,7,3)
       NUM2   = val(CADEIA)
    elseif I = 3
       CADEIA = subst(CADEIA_P,4,3)
       NUM3   = val(CADEIA)
    elseif I = 4
       CADEIA = LEFT(CADEIA_P,3)
       NUM4   = val(CADEIA)
    elseif I = 5
       CADEIA = RIGHT(CADEIA_P,3)
       NUM5   = val(CADEIA)
    endif

    NUM = val(CADEIA)

    ONZE_DES   = .f.
    PESQ_COMP  = subst(CADEIA,POSICAO,1)

    for J = 1 to 3                    
        PESQUISAR  = subst(CADEIA,POSICAO,1)

        if J = 1
           PESQ_CENT = PESQUISAR
        elseif J = 2
           PESQ_COMP = PESQUISAR
        endif

        if J = 1 .and. PESQUISAR <> "0"
           EXT_AUX = subst(CADEIA,POSICAO-1,2)

           if val(EXT_AUX) >= 11 .and. val(EXT_AUX) <= 19
              seek val(EXT_AUX)

              ONZE_DES   = .t.
              J          = J + 1
              POSICAO    = POSICAO - 1
              PASSADAS   = PASSADAS + 1
           else
              seek val(PESQUISAR)
           endif
     
           WEXTENSO = alltrim(WEXTENSO) + alltrim(EXTENSO) + " "

           if (found() .and.  NUM > 10 .and. NUM > 20) .or. I = 5
              WEXTENSO = " e " + alltrim(WEXTENSO) 
           endif
        else
           COD = val(PESQUISAR) * ( 10 ^ PASSADAS)
           seek COD

           if J <> 3
              WEXTENSO = alltrim(EXTENSO) + " " + alltrim(WEXTENSO)          
           else   
              if PESQ_COMP <> "0" .and. ! ONZE_DES
                 WEXTENSO = alltrim(EXTENSO) + " E " + alltrim(WEXTENSO)          
              else
                 WEXTENSO = alltrim(EXTENSO) + " " + alltrim(WEXTENSO)          
              endif
           endif

           if COD = 100 .and. J = 3 .and. (PESQ_COMP <> "0" .or. PESQ_CENT <> "0")
              WEXTENSO = subst(WEXTENSO,4,20)
              WEXTENSO = "CENTO " +  alltrim(WEXTENSO)
              
           elseif J = 2
              WEXTENSO = alltrim(WEXTENSO) 
           endif
        endif

        POSICAO    = POSICAO - 1
        PASSADAS   = PASSADAS + 1

        if left(alltrim(WEXTENSO),1) = "E"
           WEXTENSO = subst(alltrim(WEXTENSO),2,100)
        endif
    next

    if I == 1
       WEXTENSO1 = WEXTENSO
    elseif I == 2
       WEXTENSO2 = WEXTENSO
    elseif I == 3
       WEXTENSO3 = WEXTENSO
    elseif I == 4
       WEXTENSO4 = WEXTENSO
    endif

    WEXTENSO5 = WEXTENSO
    WEXTENSO  = space(01)

    if I = 1 
       WEXTENSO_FINAL  = WEXTENSO1 + " "
       WEXTENSO        = space(01) 
    elseif I = 2 .and. ! empty(alltrim(WEXTENSO2)) 
       WEXTENSO_FINAL  = WEXTENSO2 + " MIL, "
       WEXTENSO_FINAL = WEXTENSO_FINAL + WEXTENSO1
       WEXTENSO       = space(01)
    elseif I = 3 .and. ! empty(alltrim(WEXTENSO3))
       WEXTENSO_FINAL = WEXTENSO3 
 
       if NUM3 <= 1
          WEXTENSO_FINAL = WEXTENSO_FINAL + " MILHAO, "
       else
          WEXTENSO_FINAL = WEXTENSO_FINAL + " MILHOES, "
       endif

       if ! empty(alltrim(WEXTENSO2))
          WEXTENSO_FINAL = WEXTENSO_FINAL + WEXTENSO2 + " MIL, " 
       endif

       WEXTENSO_FINAL = WEXTENSO_FINAL + WEXTENSO1
    elseif I = 4 .and. ! empty(alltrim(WEXTENSO4))
       WEXTENSO_FINAL = WEXTENSO4
        
       if NUM4 <= 1
          WEXTENSO_FINAL = WEXTENSO_FINAL + " BILHAO, "
       else
          WEXTENSO_FINAL = WEXTENSO_FINAL + " BILHOES, "
       endif

       if ! empty(WEXTENSO3)  
          if NUM3 <= 1
             WEXTENSO_FINAL = WEXTENSO_FINAL + WEXTENSO3 + " MILHAO, "
          else
             WEXTENSO_FINAL = WEXTENSO_FINAL + WEXTENSO3 + " MILHOES, "
          endif
       endif

       if ! empty(alltrim(WEXTENSO2))
          WEXTENSO_FINAL = WEXTENSO_FINAL + WEXTENSO2 + " MIL, " 
       endif

       WEXTENSO_FINAL = WEXTENSO_FINAL + WEXTENSO1
    endif
next

if right(alltrim(WEXTENSO_FINAL),1) = ","
   WEXTENSO_FINAL = subst(alltrim(WEXTENSO_FINAL),1,len(alltrim(WEXTENSO_FINAL)) -1)
endif

*if ! empty(alltrim(WEXTENSO1))
   if NUM0 = 1
      WEXTENSO_FINAL = WEXTENSO_FINAL + " REAL"
   else
      WEXTENSO_FINAL = WEXTENSO_FINAL + " REAIS"
   endif
*endif

if ! empty(alltrim(WEXTENSO5))
   if upper(left(alltrim(WEXTENSO5),1)) <> "E" 
      COMPLEMENTO = " E "
   else
      COMPLEMENTO = " "
   endif

   if NUM5 <= .01
      WEXTENSO_FINAL = WEXTENSO_FINAL + COMPLEMENTO + WEXTENSO5 + " CENTAVO"
   else
      WEXTENSO_FINAL = WEXTENSO_FINAL + COMPLEMENTO + WEXTENSO5 + " CENTAVOS"
   endif
endif

WEXTENSO_FINAL = strtran(WEXTENSO_FINAL,"e","E")
WEXTENSO_FINAL = strtran(WEXTENSO_FINAL,"   "," ")
 
if left(alltrim(WEXTENSO_FINAL),1) = "E"
   WEXTENSO_FINAL = subst(alltrim(WEXTENSO_FINAL),2,100)
endif

WEXTENSO_FINAL = alltrim(WEXTENSO_FINAL)+"."

select &AREA

set softseek on

return(WEXTENSO_FINAL)

***************
function SOMBRA
***************

private L1,C1,L2,C2,ATR,TELA,TAM,FORM
parameters L1,C1,L2,C2,ATR

TELA = savescreen(L1,C1,L2,C2,ATR)
TAM  = len(TELA)
FORM = replicate("X"+chr(ATR),TAM/2)

restscreen(L1,C1,L2,C2,transform(TELA,FORM))

return .t.

***************
function READ_F
***************

setcursor(1)
read
setcursor(0)

return .t.


*****************
function CHCK_PRN
*****************
private T_PR

T_PR = savescreen(10,23,14,54)

if ! isprinter()
   setcolor(COR_CONS)
   @ 10,23 clear to 14,54  
   @ 10,23 to 14,54 double 
   @ 12,24 say "  Impressora Desativada...  "
   setcolor(COR_TEXT)   
endif

do while ! isprinter() .and. inkey() <> 27
enddo

restscreen(10,23,14,54,T_PR)

if lastkey() == 27
   return .f.
endif

return .t.

*********************
function CALENDARIO()
*********************
static L
static C

local WWCOR, WWCURSOR, WWTELA, WWDATA, WWMES, WWDIA, WWCAB, WWLIMITE, COL, LIN, A, I

WWCOR    = setcolor()
WWCURSOR = setcursor()

save screen to WWTELA

setcursor(0)

if valtype(C) == "U"
   C = 0
endif

if valtype(L) == "U"
   L = 0
endif

WWDATA = date()

do while .t. 
   WWMES = month(WWDATA)
   WWCAB = mes(WWMES)+"/"+str(year(WWDATA),4)

   if WWMES = 2
      if year(WWDATA) % 4 = 0
         WWLIMITE = 29
      else 
         WWLIMITE = 28
      endif 
   elseif WWMES = 1 .or. WWMES = 3 .or. WWMES = 5 .or.  WWMES = 7 .or. WWMES = 8 .or. WWMES = 10 .or. WWMES = 12
      WWLIMITE = 31
   else
      WWLIMITE = 30
   endif

   setcolor("N/W")
   @ 01+L,00+C clear to 09+L,23+C

   setcolor("W/N")
   @ 00+L,01+C say replicate(chr(220),22)
   @ 09+L,01+C say replicate(chr(223),22)
   @ 00+L,00+C to 09+L,00+C 
   @ 00+L,23+C to 09+L,23+C 
   setcolor("W/B")
   @ 01+L,02+C clear to 01+L,21+C
   @ 01+L,02+C+((22-len(alltrim(WWCAB)))/2) say upper(WWCAB)
   setcolor("N/W")

   @ 02+L,02+C say " D  S  T  Q  Q  S  S"
   setcolor("R/W")
   @ 02+L,02+C say " D"
   setcolor("N/W") 

   WWDIA = ctod("01/"+str(WWMES,2)+"/"+right(str(year(WWDATA),4),2))
   COL   = ((dow(WWDIA)*3)-2)+1
   LIN   = 03

   for I = 1 to WWLIMITE
       if I == day(date())
          setcolor("B/W")
          @ 02+L,COL+C say substr(" D  S  T  Q  Q  S  S",COL-1,2)
          if COL == 02     
	     setcolor("B/W")
          endif
          @ LIN+L,COL+C say str(I,2)
          setcolor("N/W")
       else
          if COL == 02
             setcolor("R/W") 
             @ LIN+L,COL+C say str(I,2)
             setcolor("N/W")
          else
             @ LIN+L,COL+C say str(I,2)
          endif   
       endif   

       COL = COL + 3
     
       if (COL) > 21
          LIN = LIN + 1
          COL = 02
       endif
   next

   A = inkey(0)

   if A == 3
      if month(WWDATA) < 12
         WWDATA = ctod("01/"+strzero(month(WWDATA)+1,2)+"/"+strzero(year(WWDATA),4))
      else
         WWDATA = ctod("01/01/"+strzero(year(WWDATA)+1,4))
      endif
   elseif A == 18
      if month(WWDATA) > 1
         WWDATA = ctod("01/"+strzero(month(WWDATA)-1,2)+"/"+strzero(year(WWDATA),4))
      else
         WWDATA = ctod("01/12/"+strzero(year(WWDATA)-1,4))
      endif
   elseif A == 5
      restore screen from WWTELA
      L = L - 1

      if L < 0
         L = 15
      endif
   elseif A == 24
      restore screen from WWTELA
      L = L + 1

      if L > 15
         L = 0
      endif
   elseif A == 19
      restore screen from WWTELA
      C = C - 1

      if C < 0
         C = 56
      endif           
   elseif A == 4
      restore screen from WWTELA
      C = C + 1

      if C > 56
         C = 0 
      endif
   elseif A == 27
      restore screen from WWTELA

      setcursor(WWCURSOR)
      setcolor(WWCOR)
      return .t.
   endif
enddo

return .t.

*********************
***  SuperCalc 
***        versao 2.0
***  
***  by  SERGIO A. DE CARVALHO
***  (c) SERIAL SOFTWARE, Nov/93
********************************************
function SCALC

static C

local WCOR
local WCURSOR
local TELA
local CONTROL  
local PAUSA
local PARC
local PONTO
local RESP
local COMANDO
local KEY
local SINAL
local WWTELA
local STOTAL
local I
local A

set fixed on
set decimals to 12

if valtype(C) == "U"
   C       = 0
endif

WCOR    = setcolor()
WCURSOR = setcursor()

save screen to TELA
setcursor(0)

setcolor(COR_TEXT)
@ 14,01+C to 15,19+C
setcolor(COR_CONS)

@ 15,00+C clear to 24,20+C
@ 15,00+C to 24,20+C double

@ 20,14+C to 22,18+C double
@ 21,16+C say "H"

@ 20,02+C say "SuperCalc"
@ 21,02+C say "    ver 2.0"
@ 23,04+C say "(C) SERIAL" 

setcolor(COR_TEXT)
@ 16,01+C clear to 18,19+C
@ 16,01+C to 18,19+C 

CONTROL = 0
PAUSA   = .t.
PARC    = ""
PONTO   = "."
RESP    = 0
COMANDO = "+"
KEY     = .t.
SINAL   = " "
WWTELA  = savescreen(14,01+C,15,19+C)

do while .t.
   ****************
   ***  DISPLAY ***
   ****************

   @ 17,02+C clear to 17,18+C

   if empty(SINAL) .or. SINAL == "="
      if int(val(PARC)) == val(PARC)
         set fixed off
      endif

      if empty(PARC) 
         @ 17,16-len("0")+C say "0"
      else
         @ 17,16-len(PARC)+C say PARC
      endif

      @ 17,16+C           say PONTO
      @ 17,18+C           say SINAL
   else
      if int(RESP) == RESP
         set fixed off

         PONTO  = "."
         STOTAL = left(alltrim(transform(int(RESP),"@Z")),14)
      else
         PONTO  = " "
         STOTAL = left(alltrim(transform(RESP,"@Z")),14)

         for I = 14 to 1 step -1
             if right(STOTAL,1) == "0" .or. empty(right(STOTAL,1))
                STOTAL = left(STOTAL,I)
             else
                exit
             endif
         next
      endif

      if empty(STOTAL) 
         @ 17,16-len("0")+C say "0"
      else
         @ 17,16-len(STOTAL)+C say STOTAL
      endif
   
      @ 17,16+C             say PONTO
      @ 17,18+C             say SINAL

      PONTO = "."
   endif

   set fixed on

   A = inkey(0) 

   if A == 67 .or. A == 99           
      *********************
      ***  CANCELA - C  ***      
      *********************

      if SINAL <> "=" 
         scroll(13-CONTROL,01+C,14,19+C,+1)

         @ 14,01+C say chr(179)
         @ 14,19+C say chr(179)

         if PAUSA
            WWTELA = savescreen(13-CONTROL,01+C,15,19+C)
         else
            WWTELA = savescreen(00,01+C,15,19+C)
         endif      
         if CONTROL < 13 .and. PAUSA
            CONTROL = CONTROL + 1
         else
            PAUSA   = .f.
            CONTROL = 12
         endif
      endif

      RESP    = 0
      PARC    = ""
      PONTO   = "."
      COMANDO = "+"
      SINAL   = " "

      tone(400,2)

   elseif A == 27
      ***************************
      ***  FINALIZACAO - Esc  *** 
      ***************************

      restore screen from TELA
      setcursor(WCURSOR)
      setcolor(WCOR)
      set fixed on
      set decimals to 2
      return 

   elseif A == 84 .or. A == 116
      ************************
      ***  TRANSPORTA - T  *** 
      ************************

      restore screen from TELA
      setcursor(WCURSOR)
      setcolor(WCOR)
      set fixed on
      set decimals to 2

      WVARR = readvar()

      if ! empty(WVARR)
         if valtype(&WVARR) == "N" 
            &WVARR = val(PARC)
         endif
      endif

      return 

   elseif A == 19
      **************************************
      ***  MOVIMENTACAO - Seta Esquerda  ***
      **************************************

      restore screen from TELA
      C = C - 1
      if C < 0
         C = 59
      endif

      setcolor(COR_CONS)
      @ 15,00+C clear to 24,20+C
      @ 15,00+C to 24,20+C double

      @ 20,14+C to 22,18+C double
      @ 21,16+C say "H"

      @ 20,02+C say "SuperCalc"
      @ 21,02+C say "    ver 2.0"
      @ 23,04+C say "(C) SERIAL"

      setcolor(COR_TEXT)
      @ 16,01+C clear to 18,19+C
      @ 16,01+C to 18,19+C 
    
      if PAUSA
         restscreen((13-CONTROL+1),01+C,15,19+C,WWTELA)
      else
         restscreen(00,01+C,15,19+C,WWTELA)
      endif

   elseif A == 4
      *************************************
      ***  MOVIMENTACAO - Seta Direita  ***
      *************************************

      restore screen from TELA
      C = C + 1
      if C > 59
         C = 0 
      endif

      setcolor(COR_CONS)
      @ 15,00+C clear to 24,20+C
      @ 15,00+C to 24,20+C double

      @ 20,14+C to 22,18+C double
      @ 21,16+C say "H"

      @ 20,02+C say "SuperCalc"
      @ 21,02+C say "    ver 2.0"
      @ 23,04+C say "(C) SERIAL"

      setcolor(COR_TEXT)
      @ 16,01+C clear to 18,19+C
      @ 16,01+C to 18,19+C 

      if PAUSA
         restscreen((13-CONTROL+1),01+C,15,19+C,WWTELA)
      else
         restscreen(00,01+C,15,19+C,WWTELA)
      endif

   elseif A >= 48 .and. A <= 57 
      *************************
      ***  NUMEROS - 0 a 9  ***
      *************************
 
      if SINAL = "="
         PARC = ""
      endif

      if len(PARC) < 14
         PARC = PARC + chr(A) 
      endif

      SINAL = " "

   elseif A == 46 
      ***************************
      ***  PONTO DECIMAL - .  ***
      ***************************

      if PONTO <> " "
         PONTO = " "
         PARC = PARC + chr(A) 
      endif

      SINAL = " "

   elseif A == 8 
      *******************************************
      ***  ELIMINACAO DE NUMEROS - BackSpace  ***
      *******************************************

      if len(PARC) > 0
         if right(PARC,1) == "."
            PONTO = "."
         endif
         PARC = left(PARC,len(PARC)-1)
      endif

      SINAL = " "

   elseif A == 43 .and. SINAL <> "+"
      *****************
      *** SOMA - +  ***
      *****************

      if COMANDO == "/" .and. val(PARC) == 0
         RESP = 0
      else
         if COMANDO = "+"
            RESP = RESP + val(PARC)
         elseif COMANDO = "-"
            RESP = RESP - val(PARC)
         elseif COMANDO = "*"
            RESP = RESP * val(PARC)
         elseif COMANDO = "/"
            RESP = RESP / val(PARC)
         endif
      endif

      if ! empty(PARC)
         if int(val(PARC)) == val(PARC)
            set fixed off
         endif

         scroll(13-CONTROL,01+C,14,19+C,+1)

         @ 14,01+C say chr(179)
         @ 14,19+C say chr(179)
       
         @ 14,16-len(PARC)+C say PARC
         @ 14,16+C           say PONTO
         @ 14,18+C           say COMANDO
    
         if PAUSA
            WWTELA = savescreen(13-CONTROL,01+C,15,19+C)
         else
            WWTELA = savescreen(00,01+C,15,19+C)
         endif

         if CONTROL < 13 .and. PAUSA
            CONTROL = CONTROL + 1
         else
            PAUSA   = .f.
            CONTROL = 12
         endif
      endif

      set fixed on

      PARC    = ""
      COMANDO = "+"
      PONTO   = "."
      SINAL   = "+"

   elseif A == 45 .and. SINAL <> "-"
      **********************
      *** SUBTRACAO - -  ***
      **********************

      if COMANDO == "/" .and. val(PARC) == 0
         RESP = 0 
      else
         if COMANDO = "+"
            RESP = RESP + val(PARC)
         elseif COMANDO = "-"
            RESP = RESP - val(PARC)
         elseif COMANDO = "*"
            RESP = RESP * val(PARC)
         elseif COMANDO = "/"
            RESP = RESP / val(PARC)
         endif
      endif

      if ! empty(PARC)
         if int(val(PARC)) == val(PARC)
            set fixed off
         endif

         scroll(13-CONTROL,01+C,14,19+C,+1)

         @ 14,01+C say chr(179)
         @ 14,19+C say chr(179)
      
         @ 14,16-len(PARC)+C say PARC
         @ 14,16+C           say PONTO
         @ 14,18+C           say COMANDO

         if PAUSA
            WWTELA = savescreen(13-CONTROL,01+C,15,19+C)
         else
            WWTELA = savescreen(00,01+C,15,19+C)
         endif

         if CONTROL < 13 .and. PAUSA
            CONTROL = CONTROL + 1
         else
            PAUSA   = .f.
            CONTROL = 12
         endif
      endif

      set fixed on

      PARC    = ""
      COMANDO = "-"
      PONTO   = "."
      SINAL   = "-"

   elseif A == 42  .and. SINAL <> "*"
      **************************
      *** MULTIPLICACAO - *  ***
      **************************

      if COMANDO == "/" .and. val(PARC) == 0
         RESP = 0 
      else
         if COMANDO = "+"
            RESP = RESP + val(PARC)
         elseif COMANDO = "-"
            RESP = RESP - val(PARC)
         elseif COMANDO = "*"
            RESP = RESP * val(PARC)
         elseif COMANDO = "/"
            RESP = RESP / val(PARC)
         endif
      endif

      if ! empty(PARC)
         if int(val(PARC)) == val(PARC)
            set fixed off
         endif

         scroll(13-CONTROL,01+C,14,19+C,+1)

         @ 14,01+C say chr(179)
         @ 14,19+C say chr(179)
      
         @ 14,16-len(PARC)+C say PARC
         @ 14,16+C           say PONTO
         @ 14,18+C           say COMANDO

         if PAUSA
            WWTELA = savescreen(13-CONTROL,01+C,15,19+C)
         else
            WWTELA = savescreen(00,01+C,15,19+C)
         endif
  
         if CONTROL < 13 .and. PAUSA
            CONTROL = CONTROL + 1
         else
            PAUSA   = .f.
            CONTROL = 12
         endif
      endif

      set fixed on

      PARC    = ""
      COMANDO = "*"
      PONTO   = "."
      SINAL   = "*"

   elseif A == 81 .or. A == 113
      ***************************
      ***  RAIZ QUADRADA - Q  ***
      ***************************

      if COMANDO == "/" .and. val(PARC) == 0
         RESP = 0 
      else
         if COMANDO = "+"
            RESP = RESP + val(PARC)
         elseif COMANDO = "-"
            RESP = RESP - val(PARC)
         elseif COMANDO = "*"
            RESP = RESP * val(PARC)
         elseif COMANDO = "/"
            RESP = RESP / val(PARC)
         endif
      endif

      if ! empty(PARC)
         if int(val(PARC)) == val(PARC)
            set fixed off
         endif

         scroll(13-CONTROL,01+C,14,19+C,+1)

         @ 14,01+C say chr(179)
         @ 14,19+C say chr(179)
      
         @ 14,16-len(PARC)+C say PARC
         @ 14,16+C           say PONTO
         @ 14,18+C           say chr(251)

         if PAUSA
            WWTELA = savescreen(13-CONTROL,01+C,15,19+C)
         else
            WWTELA = savescreen(00,01+C,15,19+C)
         endif

         if CONTROL < 13 .and. PAUSA
            CONTROL = CONTROL + 1
         else
            PAUSA   = .f.
            CONTROL = 12
         endif
      endif

      set fixed on

      RESP = sqrt(RESP)

      if int(RESP) == RESP 
         set fixed off

         PONTO = "."
         PARC = left(alltrim(transform(int(RESP),"@Z")),14)
      else
         PONTO = " "
         PARC = left(alltrim(transform(RESP,"@Z")),14)

         for I = 14 to 1 step -1
             if right(PARC,1) == "0" .or. empty(right(PARC,1))
                PARC = left(PARC,I)
             else
                exit
             endif
         next
      endif

      scroll(13-CONTROL,01+C,14,19+C,+1)

      @ 14,01+C say chr(179)
      @ 14,19+C say chr(179)
      
      if empty(PARC) 
         @ 14,16-len("0")+C say "0"
      else
         @ 14,16-len(PARC)+C say PARC
      endif

      @ 14,16+C           say PONTO
      @ 14,18+C           say "="

      if PAUSA
         WWTELA = savescreen(13-CONTROL,01+C,15,19+C)
      else
         WWTELA = savescreen(00,01+C,15,19+C)
      endif

      if CONTROL < 13 .and. PAUSA
         CONTROL = CONTROL + 1
      else
         PAUSA   = .f.
         CONTROL = 12
      endif

      scroll(13-CONTROL,01+C,14,19+C,+1)

      @ 14,01+C say chr(179)
      @ 14,19+C say chr(179)

      if PAUSA
         WWTELA = savescreen(13-CONTROL,01+C,15,19+C)
      else
         WWTELA = savescreen(00,01+C,15,19+C)
      endif
      
      if CONTROL < 13 .and. PAUSA
         CONTROL = CONTROL + 1
      else
         PAUSA   = .f.
         CONTROL = 12
      endif

      set fixed on
  
      SINAL   = "="
      COMANDO = "+"
      RESP    = 0

   elseif A == 37 .and. SINAL <> "%"
      *************************
      ***  PORCENTAGEM - %  ***
      *************************

      if ! empty(PARC)
         if int(val(PARC)) == val(PARC)
            set fixed off
         endif

         scroll(13-CONTROL,01+C,14,19+C,+1)

         @ 14,01+C say chr(179)
         @ 14,19+C say chr(179)
      
         @ 14,16-len(COMANDO+PARC)+C say COMANDO+PARC
         @ 14,16+C                   say PONTO
         @ 14,18+C                   say "%"

         if PAUSA
            WWTELA = savescreen(13-CONTROL,01+C,15,19+C)
         else
            WWTELA = savescreen(00,01+C,15,19+C)
         endif

         if CONTROL < 13 .and. PAUSA
            CONTROL = CONTROL + 1
         else
            PAUSA   = .f.
            CONTROL = 12
         endif
      endif

      set fixed on

      if COMANDO = "+"
         RESP = RESP + ((RESP * val(PARC)) / 100) 
      elseif COMANDO = "-"
         RESP = RESP - ((RESP * val(PARC)) / 100)
      else
         RESP = ((RESP * val(PARC)) / 100)
      endif

      if int(RESP) == RESP
         set fixed off

         PONTO = "."
         PARC = left(alltrim(transform(int(RESP),"@Z")),14)
      else
         PONTO = " "
         PARC = left(alltrim(transform(RESP,"@Z")),14)

         for I = 14 to 1 step -1
             if right(PARC,1) == "0" .or. empty(right(PARC,1))
                PARC = left(PARC,I)
             else
                exit
             endif
         next
      endif

      scroll(13-CONTROL,01+C,14,19+C,+1)

      @ 14,01+C say chr(179)
      @ 14,19+C say chr(179)
      
      if empty(PARC) 
         @ 14,16-len("0")+C say "0"
      else
         @ 14,16-len(PARC)+C say PARC
      endif

      @ 14,16+C           say PONTO
      @ 14,18+C           say "="

      if PAUSA
         WWTELA = savescreen(13-CONTROL,01+C,15,19+C)
      else
         WWTELA = savescreen(00,01+C,15,19+C)
      endif

      if CONTROL < 13 .and. PAUSA
         CONTROL = CONTROL + 1
      else
         PAUSA   = .f.
         CONTROL = 12
      endif

      scroll(13-CONTROL,01+C,14,19+C,+1)

      @ 14,01+C say chr(179)
      @ 14,19+C say chr(179)

      if PAUSA
         WWTELA = savescreen(13-CONTROL,01+C,15,19+C)
      else
         WWTELA = savescreen(00,01+C,15,19+C)
      endif
      
      if CONTROL < 13 .and. PAUSA
         CONTROL = CONTROL + 1
      else
         PAUSA   = .f.
         CONTROL = 12
      endif

      set fixed on
  
      SINAL   = "="
      COMANDO = "+"
      RESP    = 0

   elseif A == 47 .and. SINAL <> "/"
      *********************
      ***  DIVISAO - /  ***
      *********************

      if COMANDO == "/" .and. val(PARC) == 0
         RESP = 0 
      else
         if COMANDO = "+"
            RESP = RESP + val(PARC)
         elseif COMANDO = "-"
            RESP = RESP - val(PARC)
         elseif COMANDO = "*"
            RESP = RESP * val(PARC)
         elseif COMANDO = "/"
            RESP = RESP / val(PARC)
         endif
      endif

      if ! empty(PARC)
         if int(val(PARC)) == val(PARC)
            set fixed off
         endif

         scroll(13-CONTROL,01+C,14,19+C,+1)

         @ 14,01+C say chr(179)
         @ 14,19+C say chr(179)
      
         @ 14,16-len(PARC)+C say PARC
         @ 14,16+C           say PONTO
         @ 14,18+C           say COMANDO

         if PAUSA
            WWTELA = savescreen(13-CONTROL,01+C,15,19+C)
         else
            WWTELA = savescreen(00,01+C,15,19+C)
         endif

         if CONTROL < 13 .and. PAUSA
            CONTROL = CONTROL + 1
         else
            PAUSA   = .f.
            CONTROL = 12
         endif
      endif

      set fixed on

      PARC    = ""
      COMANDO = "/"
      PONTO   = "."
      SINAL   = "/"

   elseif A == 13 .and. SINAL <> "="
      ***********************
      ***  RESULTADO - =  ***
      ***********************

      if COMANDO == "/" .and. val(PARC) == 0
         RESP = 0 
      else
         if COMANDO = "+"
            RESP = RESP + val(PARC)
         elseif COMANDO = "-"
            RESP = RESP - val(PARC)
         elseif COMANDO = "*"
            RESP = RESP * val(PARC)
         elseif COMANDO = "/"
            RESP = RESP / val(PARC)
         endif
      endif

      if ! empty(PARC)
         if int(val(PARC)) == val(PARC)
            set fixed off
         endif

         scroll(13-CONTROL,01+C,14,19+C,+1)

         @ 14,01+C say chr(179)
         @ 14,19+C say chr(179)
      
         @ 14,16-len(PARC)+C say PARC
         @ 14,16+C           say PONTO
         @ 14,18+C           say COMANDO

         if PAUSA
            WWTELA = savescreen(13-CONTROL,01+C,15,19+C)
         else
            WWTELA = savescreen(00,01+C,15,19+C)
         endif

         if CONTROL < 13 .and. PAUSA
            CONTROL = CONTROL + 1
         else
            PAUSA   = .f.
            CONTROL = 12
         endif
      endif

      set fixed on

      if int(RESP) == RESP
         set fixed off

         PONTO = "."
         PARC = left(alltrim(transform(int(RESP),"@Z")),14)
      else
         PONTO = " "
         PARC = left(alltrim(transform(RESP,"@Z")),14)

         for I = 14 to 1 step -1
             if right(PARC,1) == "0" .or. empty(right(PARC,1))
                PARC = left(PARC,I)
             else
                exit
             endif
         next
      endif

      scroll(13-CONTROL,01+C,14,19+C,+1)

      @ 14,01+C say chr(179)
      @ 14,19+C say chr(179)
      
      if empty(PARC) 
         @ 14,16-len("0")+C say "0"
      else
         @ 14,16-len(PARC)+C say PARC
      endif

      @ 14,16+C           say PONTO
      @ 14,18+C           say "="

      if PAUSA
         WWTELA = savescreen(13-CONTROL,01+C,15,19+C)
      else
         WWTELA = savescreen(00,01+C,15,19+C)
      endif

      if CONTROL < 13 .and. PAUSA
         CONTROL = CONTROL + 1
      else
         PAUSA   = .f.
         CONTROL = 12
      endif

      scroll(13-CONTROL,01+C,14,19+C,+1)

      @ 14,01+C say chr(179)
      @ 14,19+C say chr(179)

      if PAUSA
         WWTELA = savescreen(13-CONTROL,01+C,15,19+C)
      else
         WWTELA = savescreen(00,01+C,15,19+C)
      endif
      
      if CONTROL < 13 .and. PAUSA
         CONTROL = CONTROL + 1
      else
         PAUSA   = .f.
         CONTROL = 12
      endif

      set fixed on
  
      SINAL   = "="
      COMANDO = "+"
      RESP    = 0

   elseif A == 76 .or. A == 108
      *******************
      ***  LIMPA - L  ***       
      *******************

      restore screen from TELA

      setcolor(COR_TEXT)
      @ 14,01+C to 15,19+C  
      setcolor(COR_CONS)

      @ 15,00+C clear to 24,20+C
      @ 15,00+C to 24,20+C double

      @ 20,14+C to 22,18+C double
      @ 21,16+C say "H"

      @ 20,02+C say "SuperCalc"
      @ 21,02+C say "    ver 2.0"
      @ 23,04+C say "(C) SERIAL"

      setcolor(COR_TEXT)
      @ 16,01+C clear to 18,19+C
      @ 16,01+C to 18,19+C 
    
      CONTROL = 0
      PAUSA   = .t.

      WWTELA = savescreen(14,01+C,15,19+C)

   elseif A == 72 .or. A == 104
      ******************
      ***  HELP - H  ***      
      ******************

      scroll(13-CONTROL,01+C,14,19+C,+1)

      @ 14,01+C say chr(179)
      @ 14,19+C say chr(179)

      @ 14,03+C say "H  HELP"

      if PAUSA
         WWTELA = savescreen(13-CONTROL,01+C,15,19+C)
      else
         WWTELA = savescreen(00,01+C,15,19+C)
      endif
      
      if CONTROL < 13 .and. PAUSA
         CONTROL = CONTROL + 1
      else
         PAUSA   = .f.
         CONTROL = 12
      endif

      scroll(13-CONTROL,01+C,14,19+C,+1)

      @ 14,01+C say chr(179)
      @ 14,19+C say chr(179)

      @ 14,03+C say chr(26)+chr(27)+" Movimenta"

      if PAUSA
         WWTELA = savescreen(13-CONTROL,01+C,15,19+C)
      else
         WWTELA = savescreen(00,01+C,15,19+C)
      endif
      
      if CONTROL < 13 .and. PAUSA
         CONTROL = CONTROL + 1
      else
         PAUSA   = .f.
         CONTROL = 12
      endif

      scroll(13-CONTROL,01+C,14,19+C,+1)

      @ 14,01+C say chr(179)
      @ 14,19+C say chr(179)

      @ 14,03+C say "+  Soma"

      if PAUSA
         WWTELA = savescreen(13-CONTROL,01+C,15,19+C)
      else
         WWTELA = savescreen(00,01+C,15,19+C)
      endif
      
      if CONTROL < 13 .and. PAUSA
         CONTROL = CONTROL + 1
      else
         PAUSA   = .f.
         CONTROL = 12
      endif

      scroll(13-CONTROL,01+C,14,19+C,+1)

      @ 14,01+C say chr(179)
      @ 14,19+C say chr(179)

      @ 14,03+C say "-  Subtra"+chr(135)+chr(132)+"o"

      if PAUSA
         WWTELA = savescreen(13-CONTROL,01+C,15,19+C)
      else
         WWTELA = savescreen(00,01+C,15,19+C)
      endif
      
      if CONTROL < 13 .and. PAUSA
         CONTROL = CONTROL + 1
      else
         PAUSA   = .f.
         CONTROL = 12
      endif

      scroll(13-CONTROL,01+C,14,19+C,+1)

      @ 14,01+C say chr(179)
      @ 14,19+C say chr(179)

      @ 14,03+C say "*  Multiplica"+chr(135)+chr(132)+"o"

      if PAUSA
         WWTELA = savescreen(13-CONTROL,01+C,15,19+C)
      else
         WWTELA = savescreen(00,01+C,15,19+C)
      endif
      
      if CONTROL < 13 .and. PAUSA
         CONTROL = CONTROL + 1
      else
         PAUSA   = .f.
         CONTROL = 12
      endif

      scroll(13-CONTROL,01+C,14,19+C,+1)

      @ 14,01+C say chr(179)
      @ 14,19+C say chr(179)

      @ 14,03+C say "/  Divis"+chr(132)+"o"

      if PAUSA
         WWTELA = savescreen(13-CONTROL,01+C,15,19+C)
      else
         WWTELA = savescreen(00,01+C,15,19+C)
      endif
      
      if CONTROL < 13 .and. PAUSA
         CONTROL = CONTROL + 1
      else
         PAUSA   = .f.
         CONTROL = 12
      endif

      scroll(13-CONTROL,01+C,14,19+C,+1)

      @ 14,01+C say chr(179)
      @ 14,19+C say chr(179)

      @ 14,03+C say "%  Porcentagem"

      if PAUSA
         WWTELA = savescreen(13-CONTROL,01+C,15,19+C)
      else
         WWTELA = savescreen(00,01+C,15,19+C)
      endif
      
      if CONTROL < 13 .and. PAUSA
         CONTROL = CONTROL + 1
      else
         PAUSA   = .f.
         CONTROL = 12
      endif

      scroll(13-CONTROL,01+C,14,19+C,+1)

      @ 14,01+C say chr(179)
      @ 14,19+C say chr(179)

      @ 14,03+C say "Q  Raiz Quadrada"

      if PAUSA
         WWTELA = savescreen(13-CONTROL,01+C,15,19+C)
      else
         WWTELA = savescreen(00,01+C,15,19+C)
      endif
      
      if CONTROL < 13 .and. PAUSA
         CONTROL = CONTROL + 1
      else
         PAUSA   = .f.
         CONTROL = 12
      endif

      scroll(13-CONTROL,01+C,14,19+C,+1)

      @ 14,01+C say chr(179)
      @ 14,19+C say chr(179)

      @ 14,03+C say chr(17)+chr(217)+" Resultado"

      if PAUSA
         WWTELA = savescreen(13-CONTROL,01+C,15,19+C)
      else
         WWTELA = savescreen(00,01+C,15,19+C)
      endif
      
      if CONTROL < 13 .and. PAUSA
         CONTROL = CONTROL + 1
      else
         PAUSA   = .f.
         CONTROL = 12
      endif

      scroll(13-CONTROL,01+C,14,19+C,+1)

      @ 14,01+C say chr(179)
      @ 14,19+C say chr(179)

      @ 14,03+C say "C  Cancela"

      if PAUSA
         WWTELA = savescreen(13-CONTROL,01+C,15,19+C)
      else
         WWTELA = savescreen(00,01+C,15,19+C)
      endif
      
      if CONTROL < 13 .and. PAUSA
         CONTROL = CONTROL + 1
      else
         PAUSA   = .f.
         CONTROL = 12
      endif

      scroll(13-CONTROL,01+C,14,19+C,+1)

      @ 14,01+C say chr(179)
      @ 14,19+C say chr(179)

      @ 14,03+C say "L  Limpa"

      if PAUSA
         WWTELA = savescreen(13-CONTROL,01+C,15,19+C)
      else
         WWTELA = savescreen(00,01+C,15,19+C)
      endif
      
      if CONTROL < 13 .and. PAUSA
         CONTROL = CONTROL + 1
      else
         PAUSA   = .f.
         CONTROL = 12
      endif

      scroll(13-CONTROL,01+C,14,19+C,+1)

      @ 14,01+C say chr(179)
      @ 14,19+C say chr(179)

      @ 14,03+C say "T  Transporta"

      if PAUSA
         WWTELA = savescreen(13-CONTROL,01+C,15,19+C)
      else
         WWTELA = savescreen(00,01+C,15,19+C)
      endif
      
      if CONTROL < 13 .and. PAUSA
         CONTROL = CONTROL + 1
      else
         PAUSA   = .f.
         CONTROL = 12
      endif

      scroll(13-CONTROL,01+C,14,19+C,+1)

      @ 14,01+C say chr(179)
      @ 14,19+C say chr(179)

      @ 14,03+C say "Esc Fim"

      if PAUSA
         WWTELA = savescreen(13-CONTROL,01+C,15,19+C)
      else
         WWTELA = savescreen(00,01+C,15,19+C)
      endif
      
      if CONTROL < 13 .and. PAUSA
         CONTROL = CONTROL + 1
      else
         PAUSA   = .f.
         CONTROL = 12
      endif

      scroll(13-CONTROL,01+C,14,19+C,+1)

      @ 14,01+C say chr(179)
      @ 14,19+C say chr(179)

      if PAUSA
         WWTELA = savescreen(13-CONTROL,01+C,15,19+C)
      else
         WWTELA = savescreen(00,01+C,15,19+C)
      endif
      
      if CONTROL < 13 .and. PAUSA
         CONTROL = CONTROL + 1
      else
         PAUSA   = .f.
         CONTROL = 12
      endif
   endif      
enddo

return .t.

****************
function ACHOISE
****************
private I, LIN1, COL1, LIN2, COL2, NCRI, OPCOES, ACESSO, LETRAS, POSICAO, OBS,;
        NORMAL, FAIXA, DESTAQUE, OBS_COR, A, KEY, POS, TECLA, X, _CTRL, COR_ATUAL, WCURSOR, QQTDE

parameters I, LIN1, COL1, LIN2, COL2, NCRI, OPCOES, ACESSO, LETRAS, POSICAO, OBS,;
           NORMAL, FAIXA, DESTAQUE, OBS_COR

QQTDE = len(OPCOES)

@ 05,00 clear to 22,79
@ 24,00 clear to 24,79

@ 06,01 say "Usu"+chr(160)+"rio"
@ 07,01 say upper(WPUSUARIO)

@ 06,65 say left(semana(date()),3)+" "+strzero(day(date()),2)+"/"+upper(left(mes(month(date())),3))+"/"+right(str(year(date())),2)
@ 21,02 say "F7 - Calend"+chr(160)+"rio"
@ 22,02 say "F8 - Calculadora"

if right(COR_TEXT,1) <> "N"
   setcolor(right(COR_TEXT,len(COR_TEXT)-at("/",COR_TEXT))+"+/"+right(COR_TEXT,len(COR_TEXT)-at("/",COR_TEXT)))
else
   setcolor("W/"+right(COR_TEXT,len(COR_TEXT)-at("/",COR_TEXT)))
endif

@ 18,63 say chr(218)+chr(196)+chr(191)+" "+chr(218)+chr(196)+chr(191)+" "+chr(218)+chr(196)+chr(191)
@ 19,63 say chr(192)+chr(196)+chr(191)+" "+chr(195)+chr(196)+chr(180)+" "+chr(179)
@ 20,63 say chr(192)+chr(196)+chr(217)+" "+chr(193)+" "+chr(193)+" "+chr(192)+chr(196)+chr(217)

@ 21,59 say "SINCON - Vers"+chr(132)+"o 3.3" 
@ 22,59 say "TEL. (24) 9997-6996"
setcolor(COR_TEXT)

COR_ATUAL = setcolor()
WCURSOR   = setcursor()

setcolor(NORMAL)
sombra(LIN1+1,COL1+2,LIN2+1,COL2+1,8)
@ LIN1,COL1 clear to LIN2,COL2
@ LIN1,COL1 to LIN2,COL2

for X = 1 TO QQTDE
   if ACESSO[X]
      setcolor(NORMAL)
   else
      setcolor(right(NORMAL,1)+"/"+right(NORMAL,1)+"+")
      if X = I
         I = I + 1
      endif
   endif
   @ LIN1+X,COL1+1 say OPCOES[X]
next

setcolor(DESTAQUE)
for X = 1 to QQTDE
   if ACESSO[X]
      @ LIN1+X,COL1+1+POSICAO[X] say LETRAS[X]
   endif
next

_CTRL = .f.

if type("OBS") == "A"
   _CTRL = .t.
   setcolor(OBS_COR)
   @ 24,00 clear to 24,79
   centro(24,OBS[I])
endif

A = I 

************** C.R.I. *************
setcolor(COR_TEXT)
@ 23,67 say "[ "
@ 23,77 say " ]"
@ 23,69 say replicate(chr(177),8)

CRI = left(CRI,NCRI)

CRI = CRI + LETRAS[I]
setcolor("N/W")
@ 23,69 say CRI
setcolor(COR_TEXT)
CRI = left(CRI,len(CRI)-1)
************** C.R.I. *************

do while .t.
   if ACESSO[A]
      setcolor(NORMAL)
   else
      setcolor(right(NORMAL,1)+"/"+right(NORMAL,1)+"+")
   endif
   @ LIN1+A,COL1+1 say OPCOES[A]   
   setcolor(COR_TEXT)
   @ LIN1+A,COL1-2 say space(02)
   if ACESSO[A]
      setcolor(DESTAQUE)
      @ LIN1+A,COL1+1+POSICAO[A] say LETRAS[A] 
   endif

   setcolor(FAIXA) 
   @ LIN1+I,COL1+1 say OPCOES[I]
   setcolor(COR_TEXT)
*  @ LIN1+I,COL1-2 say chr(205)+chr(16)
   @ LIN1+I,COL1-2 say chr(240)+'>'

   if _CTRL
      setcolor(OBS_COR)
      @ 24,00 clear to 24,79
      centro(24,OBS[I])
   endif

   A = I
   
   setcolor(COR_TEXT)

   WSC_TIME = time()
   WTON     = .t.

   do while inkey() == 0
      @ 07,70 say time()

      if val(substr(elaptime(WSC_TIME,time()),4,2)) >= 2
         WTT_EXIT = savescreen(00,00,24,79)

         @ 00,00 clear to 24,79

         WEXIT_KEY = 0

         setcolor("GR+/B")
 
         for XIX = 1 to 7
             if XIX = 1 .or. XIX = 3 .or. XIX = 5 .or. XIX = 7 
                setcolor("GR+/B")
             elseif XIX = 2 .or. XIX = 4 .or. XIX = 6  
                setcolor("N/B")
             endif 
 
             @ 09,37 say chr(218)+chr(196)+chr(196)+chr(196)+chr(191) 
             @ 10,37 say chr(179)+chr(032)+chr(032)+chr(032)+chr(032)  
             @ 11,37 say chr(192)+chr(196)+chr(196)+chr(196)+chr(191)
             @ 12,37 say chr(032)+chr(032)+chr(032)+chr(032)+chr(179)
             @ 13,37 say chr(192)+chr(196)+chr(196)+chr(196)+chr(217)
        
             if XIX = 1
                WEXIT_KEY = inkey(2)
             elseif XIX = 2
                WEXIT_KEY = inkey(.1)
             elseif XIX = 3
                WEXIT_KEY = inkey(1)
             elseif XIX = 4
                WEXIT_KEY = inkey(.1)
             elseif XIX = 5
                WEXIT_KEY = inkey(.1)
             elseif XIX = 6
                WEXIT_KEY = inkey(.1)
             elseif XIX = 7
                WEXIT_KEY = inkey(.1)
             endif

             if WEXIT_KEY <> 0
                exit
             endif 
         next
         
         if WEXIT_KEY == 0
            for XIX = 1 to 7
                if XIX = 1 .or. XIX = 3 .or. XIX = 5 .or. XIX = 7 
                   setcolor("GR+/B")
                elseif XIX = 2 .or. XIX = 4 .or. XIX = 6  
                   setcolor("N/B")
                endif 
        
                @ 09,37 say chr(194)+chr(032)+chr(032)+chr(032)+chr(194)
                @ 10,37 say chr(179)+chr(032)+chr(032)+chr(032)+chr(179)
                @ 11,37 say chr(192)+chr(196)+chr(196)+chr(196)+chr(180)
                @ 12,37 say chr(032)+chr(032)+chr(032)+chr(032)+chr(179)
                @ 13,37 say chr(032)+chr(032)+chr(032)+chr(032)+chr(193)
        
                if XIX = 1
                   WEXIT_KEY = inkey(2)
                elseif XIX = 2
                   WEXIT_KEY = inkey(.1)
                elseif XIX = 3
                   WEXIT_KEY = inkey(1)
                elseif XIX = 4
                   WEXIT_KEY = inkey(.1)
                elseif XIX = 5
                   WEXIT_KEY = inkey(.1)
                elseif XIX = 6
                   WEXIT_KEY = inkey(.1)
                elseif XIX = 7
                   WEXIT_KEY = inkey(.1)
                endif
   
                if WEXIT_KEY <> 0
                   exit
                endif 
            next
         endif
      
         if WEXIT_KEY == 0
            for XIX = 1 to 7
                if XIX = 1 .or. XIX = 3 .or. XIX = 5 .or. XIX = 7 
                   setcolor("GR+/B")
                elseif XIX = 2 .or. XIX = 4 .or. XIX = 6  
                   setcolor("N/B")
                endif 
            
                @ 09,37 say chr(196)+chr(196)+chr(196)+chr(196)+chr(191)
                @ 10,37 say chr(032)+chr(032)+chr(032)+chr(032)+chr(179)
                @ 11,37 say chr(032)+chr(196)+chr(196)+chr(196)+chr(180)
                @ 12,37 say chr(032)+chr(032)+chr(032)+chr(032)+chr(179)
                @ 13,37 say chr(196)+chr(196)+chr(196)+chr(196)+chr(217)
           
                if XIX = 1
                   WEXIT_KEY = inkey(2)
                elseif XIX = 2
                   WEXIT_KEY = inkey(.1)
                elseif XIX = 3
                   WEXIT_KEY = inkey(1)
                elseif XIX = 4
                   WEXIT_KEY = inkey(.1)
                elseif XIX = 5
                   WEXIT_KEY = inkey(.1)
                elseif XIX = 6
                   WEXIT_KEY = inkey(.1)
                elseif XIX = 7
                   WEXIT_KEY = inkey(.1)
                endif
   
                if WEXIT_KEY <> 0
                   exit
                endif 
            next
         endif
         
         if WEXIT_KEY == 0
            for XIX = 1 to 7
                if XIX = 1 .or. XIX = 3 .or. XIX = 5 .or. XIX = 7 
                   setcolor("GR+/B")
                elseif XIX = 2 .or. XIX = 4 .or. XIX = 6  
                   setcolor("N/B")
                endif 
            
                @ 09,37 say chr(218)+chr(196)+chr(196)+chr(196)+chr(191)
                @ 10,37 say chr(032)+chr(032)+chr(032)+chr(032)+chr(179)
                @ 11,37 say chr(218)+chr(196)+chr(196)+chr(196)+chr(217)
                @ 12,37 say chr(179)+chr(032)+chr(032)+chr(032)+chr(032)
                @ 13,37 say chr(192)+chr(196)+chr(196)+chr(196)+chr(217)
           
                if XIX = 1
                   WEXIT_KEY = inkey(2)
                elseif XIX = 2
                   WEXIT_KEY = inkey(.1)
                elseif XIX = 3
                   WEXIT_KEY = inkey(1)
                elseif XIX = 4
                   WEXIT_KEY = inkey(.1)
                elseif XIX = 5
                   WEXIT_KEY = inkey(.1)
                elseif XIX = 6
                   WEXIT_KEY = inkey(.1)
                elseif XIX = 7
                   WEXIT_KEY = inkey(.1)
                endif
   
                if WEXIT_KEY <> 0
                   exit
                endif 
            next
         endif
      
         if WEXIT_KEY == 0
            for XIX = 1 to 7
                if XIX = 1 .or. XIX = 3 .or. XIX = 5 .or. XIX = 7 
                   setcolor("GR+/B")
                elseif XIX = 2 .or. XIX = 4 .or. XIX = 6  
                   setcolor("N/B")
                endif 
           
                @ 09,37 say chr(032)+chr(032)+chr(191)+chr(032)+chr(032)
                @ 10,37 say chr(032)+chr(032)+chr(179)+chr(032)+chr(032)
                @ 11,37 say chr(032)+chr(032)+chr(179)+chr(032)+chr(032)
                @ 12,37 say chr(032)+chr(032)+chr(179)+chr(032)+chr(032)
                @ 13,37 say chr(032)+chr(032)+chr(193)+chr(032)+chr(032)
            
                if XIX = 1
                   WEXIT_KEY = inkey(2)
                elseif XIX = 2
                   WEXIT_KEY = inkey(.1)
                elseif XIX = 3
                   WEXIT_KEY = inkey(1)
                elseif XIX = 4
                   WEXIT_KEY = inkey(.1)
                elseif XIX = 5
                   WEXIT_KEY = inkey(.1)
                elseif XIX = 6
                   WEXIT_KEY = inkey(.1)
                elseif XIX = 7
                   WEXIT_KEY = inkey(.1)
                endif
   
                if WEXIT_KEY <> 0
                   exit
                endif 
            next
         endif

         if WEXIT_KEY <> 0
            setcolor(COR_TEXT)
            restscreen(00,00,24,79,WTT_EXIT)
            exit
         else
            tone(1500,3)
            fim()
         endif             
      elseif val(substr(elaptime(WSC_TIME,time()),4,2)) >= 1 .and. WTON
         tone(400,8)
         WTON = .f.
      endif
   enddo

   KEY = lastkey()
      
   if KEY == -7
      scalc()
      loop
   endif

   if KEY == -6
      calendario()
      loop
   endif
   
   if KEY == 5
      I = I - 1
      if I == 0
         I = QQTDE
      endif

      do while ! ACESSO[I]
         I = I - 1
         if I == 0
            I = QQTDE
         endif
      enddo

      CRI = CRI + LETRAS[I]
      @ 23,69 say replicate(chr(177),8)
      setcolor("N/W")
      @ 23,69 say CRI
      setcolor(COR_TEXT)
      CRI = left(CRI,len(CRI)-1)
   elseif KEY == 24
      I = I + 1
      if I == QQTDE+1
         I = 1
      endif

      do while ! ACESSO[I]
         I = I + 1
         if I == QQTDE+1
            I = 1
         endif
      enddo

      CRI = CRI + LETRAS[I]
      @ 23,69 say replicate(chr(177),8)
      setcolor("N/W")
      @ 23,69 say CRI
      setcolor(COR_TEXT)
      CRI = left(CRI,len(CRI)-1)
   elseif (KEY >= 65 .and. KEY <= 90) .or. (KEY >= 97 .and. KEY <=122)
      TECLA = chr(KEY)
      TECLA = upper(TECLA)

      POS = ascan(LETRAS,TECLA)

      if POS <> 0 .and. ACESSO[POS]
         I = POS

         setcolor(NORMAL)
         @ LIN1+A,COL1+1 say OPCOES[A]   
         setcolor(COR_TEXT)
         @ LIN1+A,COL1-2 say space(02)
         setcolor(DESTAQUE)
         @ LIN1+A,COL1+1+POSICAO[A] say LETRAS[A] 
         setcolor(FAIXA) 
         @ LIN1+I,COL1+1 say OPCOES[I]
         setcolor(COR_TEXT)
         @ LIN1+I,COL1-2 say chr(205)+chr(16)

         if _CTRL
            setcolor(OBS_COR)
            @ 24,00 clear to 24,79
            centro(24,OBS[I])
         endif

         setcolor(COR_ATUAL)
         setcursor(WCURSOR)

         if QQTDE == I
            CRI = left(CRI,len(CRI)-1)
         else
            CRI = CRI + LETRAS[I]
         endif
         @ 23,69 say replicate(chr(177),8)
         setcolor("N/W")
         @ 23,69 say CRI
         setcolor(COR_TEXT)

         return I
      endif
   elseif KEY == 13
      setcolor(COR_ATUAL)
      setcursor(WCURSOR)
 
      if QQTDE == I
         CRI = left(CRI,len(CRI)-1)
      else
         CRI = CRI + LETRAS[I]
      endif
      @ 23,69 say replicate(chr(177),8)
      setcolor("N/W")
      @ 23,69 say CRI
      setcolor(COR_TEXT)

      return I
   elseif KEY == 27
      setcolor(COR_ATUAL)
      setcursor(WCURSOR)

      CRI = left(CRI,len(CRI)-1)
      @ 23,69 say replicate(chr(177),8)
      setcolor("N/W")
      @ 23,69 say CRI
      setcolor(COR_TEXT)

      return 0
   endif
enddo

setcolor(COR_ATUAL)
setcursor(WCURSOR)

return 0

***************** 
function CONFIRMA
***************** 

private LLIN, STRING, DEFAULT, TECLA, COLUNA, LIMPA
parameters LLIN, STRING, DEFAULT, LIMPA

setcursor(1)

COLUNA = 0
TECLA  = 0

if pcount() <> 4
   @ LLIN,10 clear to LLIN,79
endif

COLUNA = int(40 - ((len(STRING) + 1)/2))

@ LLIN,COLUNA say STRING

if DEFAULT == "S"
   @ LLIN,COLUNA+len(STRING)+1 say "Sim    "
   @ LLIN,COLUNA+len(STRING)+1 say ""
   RESP = "S"
else
   @ LLIN,COLUNA+len(STRING)+1 say "N"+chr(132)+"o    "
   @ LLIN,COLUNA+len(STRING)+1 say ""
   RESP = "N"
endif

do while .t.
   TECLA = inkey(0)

   if lastkey() == 27 .or. lastkey() == 13
      exit
   endif

   if TECLA == -7
      scalc()
   elseif TECLA == -6
      calendario()
   endif

   if TECLA == 83 .or. TECLA == 115
      RESP = "S"                       
      @ LLIN,COLUNA+len(STRING)+1 say "Sim    "
      @ LLIN,COLUNA+len(STRING)+1 say ""
      exit
   elseif TECLA == 78 .or. TECLA == 110
      RESP = "N"                       
      @ LLIN,COLUNA+len(STRING)+1 say "N"+chr(132)+"o    "
      @ LLIN,COLUNA+len(STRING)+1 say ""
      exit
   endif 
enddo  

if pcount() <> 4
   @ LLIN,10 clear to LLIN,79 
endif

setcursor(0)

return RESP

**************** 
function REDUCAO
****************

private LLIN, COLUNA, STRING, TECLA, DEFAULT
parameters LLIN, COLUNA, STRING, DEFAULT

if pcount() == 3
   DEFAULT = "N"
endif

setcursor(1)

TECLA  = 0

@ LLIN,COLUNA say STRING

if DEFAULT == "S"
   @ LLIN,COLUNA+len(STRING)+1 say "Sim    "
   @ LLIN,COLUNA+len(STRING)+1 say ""
   RESP = "S"
else
   @ LLIN,COLUNA+len(STRING)+1 say "N"+chr(132)+"o    "
   @ LLIN,COLUNA+len(STRING)+1 say ""
   RESP = "N"
endif

do while .t.
   TECLA = inkey(0)

   if TECLA == -7
      scalc()
      @ LLIN,COLUNA+len(STRING)+1 say ""
   elseif TECLA == -6
      calendario()
      @ LLIN,COLUNA+len(STRING)+1 say ""
   endif

   if TECLA == 27 .or. TECLA == 13
      exit
   endif

   if TECLA == 83 .or. TECLA == 115
      RESP = "S"                       
      @ LLIN,COLUNA+len(STRING)+1 say "Sim    "
      @ LLIN,COLUNA+len(STRING)+1 say ""
      exit
   elseif TECLA == 78 .or. TECLA == 110
      RESP = "N"                       
      @ LLIN,COLUNA+len(STRING)+1 say "N"+chr(132)+"o    "
      @ LLIN,COLUNA+len(STRING)+1 say ""
      exit
   endif 
enddo  

setcursor(0)

return RESP

*******************
function CENTRO_PRN
*******************

private T_FORM, STRING, COLUNA
parameters T_FORM, STRING

COLUNA = int((T_FORM - len(STRING))/2)    

@ prow(), COLUNA say STRING

return .t.

*************************
function COLOR_PRN(color)
*************************

if type("PRN") <> "C"
   @ prow(),00 say chr(27)+chr(114)+strzero(COLOR,1)
   @ prow(),00 say ""
    
   return .t.  
endif

if PRN == "S"
   @ prow(),00 say chr(27)+chr(114)+strzero(COLOR,1)
   @ prow(),00 say ""
endif

Return .t.

*********************
function BUSCA_INDICE
*********************
local getlist :={}
private MODO
parameters MODO

if MODO >= 4
   if lastkey() = 27 .or. lastkey() = 13
      return 0
   endif

   if (lastkey() >= 32 .and. lastkey() <= 254).or. lastkey() == 8
      if lastkey() == 8 
         if len(WWCHAVE) > 1
            WWCHAVE = left(WWCHAVE,len(WWCHAVE)-1) 
         else
            WWCHAVE = ""
         endif 
      elseif len(WWCHAVE) < 35
         WWCHAVE = WWCHAVE + chr(lastkey())
      endif

      @ 06,11 clear to 06,69   
      @ 06,11 say WWCHAVE

      seek upper(WWCHAVE)

      setcursor(0)

      return 2
   endif
endif

return 1

********************
function CRP(STRING)
********************
local VAR1, VAR2, I

VAR1 = 0
VAR2 = ""

for I = 1 to len(STRING)
    VAR1 = asc(substr(STRING,I,1)) 
 
    if VAR1 = 32
       VAR2 = VAR2 + chr(002)
    elseif int(VAR1 / 2) - VAR1 / 2 >  0
       VAR2 = VAR2 + chr((VAR1 * 2) + 60)
    else
       VAR2 = VAR2 + chr(VAR1 - 40)
    endif
next 

return VAR2

**********************
function UNCRP(STRING)
**********************
local VAR1, VAR2, I

VAR1 = 0
VAR2 = ""

for I = 1 to len(STRING)
    VAR1 = asc(substr(STRING,I,1)) 
 
    if VAR1 = 2
       VAR2 = VAR2 + chr(32)
    elseif int(VAR1 / 2) - VAR1 / 2 >  0
       VAR2 = VAR2 + chr((VAR1 - 60) / 2)
    else
       VAR2 = VAR2 + chr(VAR1 + 40)
    endif
next 

return VAR2

**************
function PAPEL
**************
local P, L, C

setcolor("B+/B")
@ 00,00 clear to 24,79

P = 15

for L = 0 to 24
    @ l,00 say right("SERIAL SOFTWARE",P)+space(01)

    for C = col() to 80 step 16
        @ L,C say "SERIAL SOFTWARE"
    next
  
    P = P - 4
    
    if P < 0
       P = 15
    endif
next

return .t.


*************
function ABRE
*************
local WTT_ABRE
private ofAREA, ofARQUIVO, ofMODO, ofALIAS, ofSTATUS
parameters ofAREA, ofARQUIVO, ofMODO, ofALIAS, ofSTATUS

select &ofAREA

if used()         
   return .f.
endif

if pcount() == 2
   ofMODO = "C"
   ofALIAS = right(ofARQUIVO, len(ofARQUIVO)-rat("\",ofARQUIVO))
   ofSTATUS = .t.
endif

if pcount() == 3
   ofALIAS = right(ofARQUIVO, len(ofARQUIVO)-rat("\",ofARQUIVO))
   ofSTATUS = .t.
endif

if pcount() == 4
   ofSTATUS = .t.
endif

if select(ofALIAS) <> 0 
   return .t.
endif

do while .t.
   if ofMODO == "E"
      use &ofARQUIVO alias &ofALIAS EXCLUSIVE
   else
      use &ofARQUIVO alias &ofALIAS
   endif

   if ! neterr() 
      return .t. 
   endif

   if ofSTATUS
      mensagem("AGUARDE UM INSTANTE. Arquivo "+alltrim(ofARQUIVO)+" n"+chr(132)+"o est"+chr(160)+" dispon"+chr(161)+"vel em modo "+if(ofMODO == "E", "EXCLUSIVO", "COMPARTILHADO")+".",20)

      if lastkey() == 27
         fim()
      endif
   else
      return .f.
   endif
enddo

return .f.


*****************
function ABRE_TMP
*****************
local WTT_ABRE
private ofAREA, ofARQUIVO, ofALIAS, ofARQNUM, ofARQNAME, ofV
parameters ofAREA, ofARQUIVO, ofALIAS, ofARQNUM

select &ofAREA

If USED()       
   Return .F.
Endif

if empty(ofALIAS)
   ofALIAS = ARQNAME
endif

do while .t.
   if ofARQNUM >= 1
      for ofV = 1 to ofARQNUM
          ofARQNAME = ofARQUIVO+strzero(ofV,if(ofARQNUM > 9,2,1))

          if ferase(ofARQNAME+".CDX") == 0 .or. ! file(ofARQNAME+".CDX")
             use &ofARQNAME alias &ofALIAS EXCLUSIVE 

             if ! neterr() 
                ferase(ofARQNAME+".CDX")
                return .t. 
             endif
          endif
      next
   else
      ofARQNAME = ofARQUIVO
   
      use &ofARQNAME alias &ofALIAS EXCLUSIVE 

      if ! neterr() 
         ferase(ofARQNAME+".CDX")
         return .t. 
      endif
   endif

   mensagem("AGUARDE UM INSTANTE. Arquivo "+alltrim(ofARQUIVO)+" n"+chr(132)+"o est"+chr(160)+" dispon"+chr(161)+"vel em modo EXCLUSIVO.",20)

   if lastkey() == 27
      fim()
   endif
enddo

return .f.


****************
function ARQLOCK
****************
local WTT_ARQLOCK

do while .t.
   if flock()
      return .t.
   endif

   mensagem("AGUARDE UM INSTANTE. Arquivo "+alltrim(alias())+" n"+chr(132)+"o est"+chr(160)+" dispon"+chr(161)+"vel para BLOQUEIO.",20)

   if lastkey() == 27
      fim()
   endif
enddo

return .f.

****************
function REGLOCK
****************
local WTT_REGLOCK

if rlock()
   return .t.
endif

mensagem("AGUARDE UM INSTANTE. Registro n"+chr(132)+"o dispon"+chr(161)+"vel em modo EXCLUSIVO.",20)

return .f.

**************
function GRAVA
**************

dbcommit()
unlock 

return .t.

****************
function INC_REG
****************
local WTT_INCREG

append blank

if ! neterr()
   return .t.
endif

MENSAGEM2('ERRO! Falha na inclusão do registro.')

return .f.

****************************************************

****************
function AGUARDE
****************

setcolor(COR_GETS)
@ 24,00 clear to 24,79
@ 24,70 say "Aguarde..."
setcolor(COR_TEXT)

return .t.

******************************
function MENSAGEM(WMSG,WDELAY)
******************************

if pcount() == 1
   WDELAY = 5
endif

if WDELAY = 0
   WDELAY = 20
endif

Alert( WMSG, , "W+/R,"+COR_BARRA, WDELAY )

return .t.

********************************
function MENSAGEM2(WTEXTO, WTIP)
********************************
#include "winuser.ch"

if valtype(WTIP) == "N" 
    WMB_ICON = 'MB_ICONEXCLAMATION'
else    
   if WTIP = 'ALERT'
      WMB_ICON = 'MB_ICONEXCLAMATION'
   elseif WTIP = 'INFO'
      WMB_ICON = 'MB_ICONINFORMATION'
   elseif WTIP = 'QUEST'
      WMB_ICON = 'MB_ICONQUESTION'
   elseif WTIP = 'ERROR'
      WMB_ICON = 'MB_ICONERROR'
   else   
      WMB_ICON = 'MB_ICONEXCLAMATION'
   endif  
endif

MessageBox(, WTEXTO, 'Aviso', WMB_ICON)

return .t.

**************************
Function busca_ANIMAL(CAD)
**************************

WROW = ROW()
WCOL = COL()+3
WVAR = READVAR()

If Empty(&WVAR)
   help_ANIMAL()
   Return .F.
Endif

AREA = ALIAS()

Select ANIMAL
ordsetfocus('SINC001')
Seek &WVAR

if ! found()
   setcursor(0)
   @ 24,10 Clear To 24,70
   centro(24,"C"+Chr(162)+"digo de animal n"+Chr(132)+"o cadastrado...")
   inkey(1)
   @ 24,10 clear to 24,70
   setcursor(1)

   @ WROW,WCOL Say Space(30)

   select &AREA

   return .f.
endif

If ! Empty(SAIDA)
   setcursor(0)
   @ 24,10 clear to 24,70
   centro(24,"Consta a saida desse animal em "+Dtoc(SAIDA)+"...")
   tone(1001,0.7)
   tone(1002,0.4)
   inkey(5)
   @ 24,10 clear to 24,70
   setcursor(1)
   Return .F.
endif

@ WROW,WCOL say NOME

If CAD = "CIO" .Or. CAD = "COB_IA" .Or. CAD = "PRENHEZ" .Or. CAD = "ABORTO" .Or. CAD = "PARTO" .Or. CAD = "LEITE"

   If SEXO <> "F"
      setcursor(0)
      @ 24,10 clear to 24,70
      centro(24,"Informe o c"+Chr(162)+"digo de um animal femea...")
      tone(1001,0.7)
      tone(1002,0.4)
      inkey(5)
      @ 24,10 clear to 24,70
      setcursor(1)
      Return .F.
   Endif

Endif
  
select &AREA

return .t.


********************
function BUSCA_CATEG
********************

WROW = row()
WVAR = readvar()

if empty(&WVAR)
   help_categ()
   return .f.
endif

AREA = alias()

select CATEG
ordsetfocus('SINC001')
seek upper(&WVAR)

if ! found()
   setcursor(0)
   @ 24,10 clear to 24,70
   centro(24,"Categoria N"+chr(132)+"o Cadastrada...")
   inkey(1)
   @ 24,10 clear to 24,70
   setcursor(1)

   @ WROW,59 say space(20)

   select &AREA

   return .f.
endif

@ WROW,59 say CATEGORIA

select &AREA

return .t.


********************
function BUSCA_PASTO
********************

WROW = row()
WVAR = readvar()

if empty(&WVAR)
   help_pasto()
   return .f.
endif

AREA = alias()

select PASTO
ordsetfocus('SINC001')
seek upper(&WVAR)

if ! found()
   setcursor(0)
   @ 24,10 clear to 24,70
   centro(24,"Pasto n"+chr(132)+"o cadastrado...")
   inkey(1)
   @ 24,10 clear to 24,70
   setcursor(1)

   select &AREA

   return .f.
endif

select &AREA

return .t.

********************
function BUSCA_TOURO
********************

WROW = row()
WVAR = readvar()
WCOL = COL()+3

if empty(&WVAR)
   help_ANIMAL()
   return .f.
endif

AREA = alias()

select ANIMAL
ordsetfocus('SINC001')
seek &WVAR

if ! found()
   setcursor(0)
   @ 24,10 clear to 24,70
   centro(24,"C"+Chr(162)+"digo de animal n"+Chr(132)+"o cadastrado...")
   inkey(1)
   @ 24,10 clear to 24,70
   setcursor(1)

   @ WROW,WCOL say space(30)

   select &AREA

   return .f.

endif

if SEXO <> "M"
   setcursor(0)
   @ 24,10 clear to 24,70
   centro(24,"Informe o c"+Chr(162)+"digo de um animal macho...")
   inkey(1)
   @ 24,10 clear to 24,70
   setcursor(1)

   @ WROW,WCOL say space(30)

   select &AREA

   return .f.
endif

if ! empty(SAIDA)
   setcursor(0)
   @ 24,10 clear to 24,70
   centro(24,"Consta a saida desse animal em "+Dtoc(SAIDA)+"...")
   tone(1001,0.7)
   tone(1002,0.4)
   inkey(5)
   @ 24,10 clear to 24,70
   setcursor(1)
endif

@ WROW,WCOL say NOME

select &AREA

return .t.

***********************
function VER_CATEG(WTP)
***********************

WVAR = readvar()
AREA = alias()

select ANIMAL
ordsetfocus('SINC001')
seek &WVAR

if upper(CATEGORIA) <> upper(WTP)
   setcursor(0)
   @ 24,10 clear to 24,70
   centro(24,"Somente Animais da Categoria "+WTP+"...")
   inkey(1)
   @ 24,10 clear to 24,70
   setcursor(1)

   select &AREA

   return .f.
endif

select &AREA

return .t.


**********************
function BUSCA_SINTOMA
**********************

WROW = row()
WVAR = readvar()

if empty(&WVAR)
   help_sintoma()
   return .f.
endif

AREA = alias()

select CSINTOMA
ordsetfocus('SINC001')
seek &WVAR

if ! found()
   setcursor(0)
   @ 24,10 clear to 24,70
   centro(24,"Sintoma n"+chr(132)+"o cadastrado...")
   inkey(1)
   @ 24,10 clear to 24,70
   setcursor(1)

   @ WROW,18 say space(25)

   select &AREA

   return .f.
endif

@ WROW,18 say SINTOMA

select &AREA

return .t.


*********************
function BUSCA_VACINA
*********************

WROW = row()
WVAR = readvar()

if empty(&WVAR)
   help_vacina()
   return .f.
endif

AREA = alias()

select CVACINA
ordsetfocus('SINC001')
seek &WVAR

if ! found()
   setcursor(0)
   @ 24,10 clear to 24,70
   centro(24,"Vacina N"+chr(132)+"o Cadastrado...")
   inkey(1)
   @ 24,10 clear to 24,70
   setcursor(1)

   @ WROW,18 say space(25)

   select &AREA

   return .f.
endif

@ WROW,18 say VACINA

select &AREA

return .t.

*******************
function BUSCA_RACA
*******************

WROW = row()
WVAR = readvar()

if empty(&WVAR)
   help_raca()
   return .f.
endif

AREA = alias()

select RACA
ordsetfocus('SINC001')
seek &WVAR

if ! found()
   setcursor(0)
   @ 24,10 clear to 24,70
   centro(24,"Ra"+chr(135)+"a N"+chr(132)+"o Cadastrado...")
   inkey(1)
   @ 24,10 clear to 24,70
   setcursor(1)

   @ WROW,18 say space(25)

   select &AREA

   return .f.
endif

@ WROW,18 say RACA

select &AREA

return .t.

*****************************
* Funcoes "HELP" de uso Geral
*****************************

********************
Function help_ANIMAL
********************
private AREA, WVAR

AREA    = alias()
WVAR    = readvar()
WWCHAVE = ""

select ANIMAL
ordsetfocus('SINC002')
Go Top

private aCAMPO[5], aPICT[5], aHEAD[5]

aCAMPO[1]  = "CODIGO"     
aCAMPO[2]  = "NOME"           
aCAMPO[3]  = "SEXO"           
aCAMPO[4]  = "DTNASC"         
aCAMPO[5]  = "PASTO"         

aPICT[1]  = "999999"
aPICT[2]  = ""
aPICT[3]  = ""
aPICT[4]  = ""
aPICT[5]  = ""

aHEAD[1]  = "C"+chr(162)+"digo"
aHEAD[2]  = "Animal"
aHEAD[3]  = "Sexo"
aHEAD[4]  = "D.N."
aHEAD[5]  = "Pasto"
                     
SC_HELP = savescreen(06,09,22,71)

SOMBRA(07,11,22,71,8)

setcolor(COR_CONS+","+COR_TEXT+",,,"+COR_CONS)
@ 06,09 clear to 21,70   
@ 07,09 to 20,70 double

@ 06,10 say chr(16)

@ 09,09 say chr(199)
@ 09,70 say chr(182)

@ 21,11 say "Help-ANIMAIS"
@ 21,53 say "PgDw/PgUp - "+chr(24)+chr(26)+chr(25)+chr(27)

dbedit(08,10,19,69,aCAMPO,"busca_indice",aPICT,aHEAD,chr(196),chr(179),"","")

setcolor(COR_TEXT+","+COR_GETS+",,,"+COR_TEXT)

restscreen(06,09,22,71,SC_HELP)

if ! empty(WVAR)
   &WVAR   = CODIGO
else
   WANIMAL = CODIGO
endif

select &AREA

return .f.


*******************
function HELP_CATEG
*******************
private AREA, WVAR

AREA    = alias()
WVAR    = readvar()
WWCHAVE = ""

select CATEG
ordsetfocus('SINC002')
Go Top

private aCAMPO[2], aPICT[2], aHEAD[2]

aCAMPO[1]  = "CODIGO"     
aCAMPO[2]  = "CATEGORIA"           

aPICT[1]  = "@!"
aPICT[2]  = ""

aHEAD[1]  = "C"+chr(162)+"digo"
aHEAD[2]  = "Categoria" 
                     
SC_HELP = savescreen(06,09,21,70)

setcolor(COR_CONS+","+COR_TEXT+",,,"+COR_CONS)
@ 06,09 clear to 21,70   
@ 07,09 to 20,70 double

@ 06,10 say chr(16)

@ 09,09 say chr(199)
@ 09,70 say chr(182)

@ 21,11 say "Help - CATEGORIA" 
@ 21,53 say "PgDw/PgUp - "+chr(24)+chr(26)+chr(25)+chr(27)

dbedit(08,10,19,69,aCAMPO,"busca_indice",aPICT,aHEAD,chr(196),chr(179),"","")

setcolor(COR_TEXT+","+COR_GETS+",,,"+COR_TEXT)

restscreen(06,09,21,70,SC_HELP)

if ! empty(WVAR)
   &WVAR     = CODIGO
else
   WCATEGORIA = CODIGO
endif

select &AREA

return .f.


*******************
function HELP_PASTO
*******************
private AREA, WVAR

AREA    = alias()
WVAR    = readvar()
WWCHAVE = ""

select PASTO
ordsetfocus('SINC001')
Go Top

private aCAMPO[2], aPICT[2], aHEAD[2]

aCAMPO[1] = "PASTO"           
aCAMPO[2] = "DESCRICAO"       

aPICT[1]  = "@!"
aPICT[2]  = ""

aHEAD[1]  = "Pasto"
aHEAD[2]  = "Descri"+Chr(135)+Chr(132)+"o"
                     
SC_HELP   = savescreen(06,09,21,70)

setcolor(COR_CONS+","+COR_TEXT+",,,"+COR_CONS)
@ 06,09 clear to 21,70   
@ 07,09 to 20,70 double

@ 06,10 say chr(16)

@ 09,09 say chr(199)
@ 09,70 say chr(182)

@ 21,11 say "Help-PASTOS" 
@ 21,53 say "PgDw/PgUp - "+chr(24)+chr(26)+chr(25)+chr(27)

dbedit(08,10,19,69,aCAMPO,"busca_indice",aPICT,aHEAD,chr(196),chr(179),"","")

setcolor(COR_TEXT+","+COR_GETS+",,,"+COR_TEXT)

restscreen(06,09,21,70,SC_HELP)

if ! empty(WVAR)
   &WVAR  = PASTO
else
   WPASTO = PASTO 
endif

select &AREA

return .f.

*********************
function HELP_SINTOMA
*********************
private AREA, WVAR

AREA    = alias()
WVAR    = readvar()
WWCHAVE = ""

select CSINTOMA
ordsetfocus('SINC002')
Go Top

private aCAMPO[2], aPICT[2], aHEAD[2]

aCAMPO[1]  = "CODIGO"     
aCAMPO[2]  = "SINTOMA"           

aPICT[1]  = "@!"
aPICT[2]  = ""

aHEAD[1]  = "C"+chr(162)+"digo"
aHEAD[2]  = "Sintoma" 
                     
SC_HELP = savescreen(06,09,21,70)

setcolor(COR_CONS+","+COR_TEXT+",,,"+COR_CONS)
@ 06,09 clear to 21,70   
@ 07,09 to 20,70 double

@ 06,10 say chr(16)

@ 09,09 say chr(199)
@ 09,70 say chr(182)

@ 21,11 say "Help - SINTOMA" 
@ 21,53 say "PgDw/PgUp - "+chr(24)+chr(26)+chr(25)+chr(27)

dbedit(08,10,19,69,aCAMPO,"busca_indice",aPICT,aHEAD,chr(196),chr(179),"","")

setcolor(COR_TEXT+","+COR_GETS+",,,"+COR_TEXT)

restscreen(06,09,21,70,SC_HELP)

if ! empty(WVAR)
   &WVAR     = CODIGO
else
   WSINTOMA  = CODIGO
endif

select &AREA

return .f.

********************
function HELP_VACINA
********************
private AREA, WVAR

AREA    = alias()
WVAR    = readvar()
WWCHAVE = ""

select CVACINA
ordsetfocus('SINC002')
Go Top

private aCAMPO[2], aPICT[2], aHEAD[2]

aCAMPO[1]  = "CODIGO"     
aCAMPO[2]  = "VACINA"           

aPICT[1]  = "@!"
aPICT[2]  = ""

aHEAD[1]  = "C"+chr(162)+"digo"
aHEAD[2]  = "Vacina" 
                     
SC_HELP = savescreen(06,09,21,70)

setcolor(COR_CONS+","+COR_TEXT+",,,"+COR_CONS)
@ 06,09 clear to 21,70   
@ 07,09 to 20,70 double

@ 06,10 say chr(16)

@ 09,09 say chr(199)
@ 09,70 say chr(182)

@ 21,11 say "Help - VACINA" 
@ 21,53 say "PgDw/PgUp - "+chr(24)+chr(26)+chr(25)+chr(27)

dbedit(08,10,19,69,aCAMPO,"busca_indice",aPICT,aHEAD,chr(196),chr(179),"","")

setcolor(COR_TEXT+","+COR_GETS+",,,"+COR_TEXT)

restscreen(06,09,21,70,SC_HELP)

if ! empty(WVAR)
   &WVAR     = CODIGO
else
   WVACINA   = CODIGO
endif

select &AREA

return .f.

******************
function HELP_RACA
******************
private AREA, WVAR

AREA    = alias()
WVAR    = readvar()
WWCHAVE = ""

select RACA
ordsetfocus('SINC002')

private aCAMPO[2], aPICT[2], aHEAD[2]

aCAMPO[1]  = "CODIGO"     
aCAMPO[2]  = "RACA"           

aPICT[1]  = "@!"
aPICT[2]  = ""

aHEAD[1]  = "C"+chr(162)+"digo"
aHEAD[2]  = "Ra"+chr(135)+"a" 
                     
SC_HELP = savescreen(06,09,21,70)

setcolor(COR_CONS+","+COR_TEXT+",,,"+COR_CONS)
@ 06,09 clear to 21,70   
@ 07,09 to 20,70 double

@ 06,10 say chr(16)

@ 09,09 say chr(199)
@ 09,70 say chr(182)

@ 21,11 say "Help - RA"+chr(135)+"A" 
@ 21,53 say "PgDw/PgUp - "+chr(24)+chr(26)+chr(25)+chr(27)

dbedit(08,10,19,69,aCAMPO,"busca_indice",aPICT,aHEAD,chr(196),chr(179),"","")

setcolor(COR_TEXT+","+COR_GETS+",,,"+COR_TEXT)

restscreen(06,09,21,70,SC_HELP)

if ! empty(WVAR)
   &WVAR     = CODIGO
else
   WRACA     = CODIGO
endif

select &AREA

return .f.


******************************
Function ERROR(MESSAGE, TEMPO)
******************************

KEY = "pressione qualquer tecla..."
TAM = Len(MESSAGE)
NCI = Int((79 - TAM) / 2) 
NCF = Int(NCI + TAM)
US  = Savescreen(12,NCI-1,18,NCF+12)
CRR = Setcursor()
COR = Setcolor()

Setcursor(0)

Tone(1001,0.7)
Tone(1002,0.4)

SOMBRA(13,NCI+1,18,NCF+3,8)

Setcolor("W+/R")  

@ 12,NCI-1 Clear To 17,NCF+2
@ 12,NCI-1 To 16,NCF+2

@ 17,(NCF+2 - Len(KEY)) Say KEY

Setcolor("W+*/R")
@ 14,NCI+1 Say MESSAGE

Inkey(TEMPO)

Restscreen(12,NCI-1,18,NCF+12,US)

Setcursor(CRR)
Setcolor(COR)

Return .T.


**********************************************************************
Function edit_REL(ARQ_, L_SUP_, C_SUP_,L_INF_, C_INF_,COR, MOLD, STEL)
**********************************************************************
* arq_ = ARQUIVO TEXTO
* l_sup_, c_sup_, l_inf_, c_inf_ = COORDENADAS DE LINHA E COLUNA
* cor  = QUAL A COR                                (DEFAULT = W+/B)
* mold = SE VAI TER MOLDURA                        (DEFAULT = .T.)
* stel = SE VAI SALVAR E RESTAURAR A TELA NO FINAL (DEFAULT = .T.)

LOCAL cur_atual, cor_atual
mold:=IF(mold=NIL,.T.,mold)
stel:=IF(stel=NIL,.T.,stel)
IF mold
   PRIV l_arq[l_inf_-l_sup_+1], maxlin_, area_, tablin_, fim_arq_, offset_ini,;
        offset_fim, lsup_:=l_sup_+1, csup_:=c_sup_+1,linf_:=l_inf_-1, t_w, t_r, ;
        cinf_:= c_inf_-1, cod_sos:=55, t_c, t_s, t_d
ELSE
   PRIV l_arq[l_inf_-l_sup_+1], maxlin_, area_, tablin_, fim_arq_, offset_ini,;
        offset_fim, lsup_:=l_sup_, csup_:=c_sup_,linf_:=l_inf_, t_w, t_r, ;
        cinf_:= c_inf_, cod_sos:=55, t_c, t_s, t_d
ENDIF
t_w:=SETKEY(K_CTRL_W,NIL); t_r:=SETKEY(K_CTRL_R,NIL)
t_c:=SETKEY(K_CTRL_C,NIL); t_s:=SETKEY(K_CTRL_S,NIL)
t_d:=SETKEY(K_CTRL_D,NIL)
IF stel
   t_t:=SAVESCREEN(l_sup_, c_sup_, l_inf_, c_inf_)
ENDIF
IF AT(".",arq_)=0
   arq_+=".prn"
ENDI
IF (area_:=FOPEN(arq_))<0
   RETURN .F.     // ERRO AO ABRIR ARQUIVO
ENDI
cor=IF(cor=NIL,"W+/B",cor)
cor_atual=SETCOLOR(cor)
cur_atual:=SETCURSOR(0)

SETCOLOR(cor)

@ l_sup_,c_sup_ CLEAR TO l_inf_,c_inf_
IF mold
   @ l_sup_,c_sup_ TO l_inf_,c_inf_
ENDIF
maxlin_=linf_-lsup_+1
AFILL(l_arq,"")
offset_ini:=offset_fim := 0
tablin_ = 1
fim_arq_=FSEEK(area_,0,2)
FSEEK(area_,0)
MONTA_LIN(maxlin_,0,.F.)
offset_ini=0
DO WHILE .t.

   tecl_p=inkey_t(0)

   IF SETKEY(tecl_p)!=NIL
      EVAL(SETKEY(tecl_p))
      tecl_p=0
   ENDI
   DO CASE
      CASE tecl_p=K_ESC
         EXIT

      CASE tecl_p=K_DOWN
         MONTA_LIN(1,0)

      CASE tecl_p=K_UP
         MONTA_LIN(-1,0)

      CASE tecl_p=K_CTRL_PGUP
         offset_ini:=offset_fim := 0
         AFILL(l_arq,"")
         MONTA_LIN(maxlin_,0)
         offset_ini=0

      CASE tecl_p=K_CTRL_PGDN
         offset_ini:=offset_fim := fim_arq_
         AFILL(l_arq,CHR(0))
         @ l_sup_+1,c_sup_+1 CLEAR TO l_inf_-1,c_inf_-1
         MONTA_LIN(-1*(maxlin_),0)

      CASE tecl_p=K_RIGHT
         MONTA_LIN(0,10)   // ELE PULA 10 ESPACOS, PODE SER ALTERADO

      CASE tecl_p=K_LEFT
         MONTA_LIN(0,-10)  // ELE PULA 10 ESPACOS, PODE SER ALTERADO

      CASE tecl_p=K_PGDN
         MONTA_LIN(maxlin_-1,0)

      CASE tecl_p=K_PGUP
         MONTA_LIN(-1*(maxlin_-1),0)

   END CASE
ENDD
FCLOSE(area_)
SETKEY(K_CTRL_W,t_w); SETKEY(K_CTRL_R,t_r)
SETKEY(K_CTRL_C,t_c); SETKEY(K_CTRL_S,t_s)
SETKEY(K_CTRL_D,t_d)
IF stel
   RESTSCREEN(l_sup_, c_sup_, l_inf_, c_inf_, t_t)
ENDIF
SETCOLOR(COR_TEXT)
@ 24,00 CLEAR TO 24,79
SETCURSOR(cur_atual)
SETCOLOR(cor_atual)

Return .T.

STATIC PROC MONTA_LIN(qtlin_,qttab_)  // le/imprime linhas do arquivo binario
LOCAL t_, i_, x_, j_, lin_, buf_, tambuf_, ini_, fim_, qts_:=0
IF qtlin_!=0
   tambuf_=MIN(ABS(qtlin_)*2700,32000)
   IF qtlin_<0 .AND. offset_ini>1
      DO WHILE .t.
         IF offset_ini<tambuf_
            tambuf_=offset_ini
         ENDI
         FSEEK(area_,offset_ini-tambuf_)
         buffer_=SPAC(tambuf_)
         FREAD(area_,@buffer_,tambuf_)
         EXIT
      ENDD
      buffer_=STRTRAN(buffer_,CHR(12)+CHR(13),CHR(13)+CHR(10))
      tambuf_++
      buf_=tambuf_
      FOR t_ = qtlin_ TO -1
         IF tambuf_ > 1
            tambuf_-=3
            i_=tambuf_
            tambuf_=RAT(CHR(13)+CHR(10),SUBS(buffer_,1,tambuf_))
            tambuf_=IF(tambuf_>0,tambuf_+2,1)
            IF l_arq[maxlin_]!=CHR(0)
               offset_fim-=LEN(l_arq[maxlin_])+2
            ENDI
            AINS(l_arq,1)
            i_=i_-tambuf_+1
            l_arq[1]=SUBS(buffer_,tambuf_,i_)
            qts_--
         ELSE
            EXIT
         ENDI
      NEXT
      offset_ini-=buf_-tambuf_

   ELSEIF l_arq[2]!=CHR(0)
      FSEEK(area_,offset_fim)
      IF offset_fim+tambuf_>fim_arq_
         tambuf_=fim_arq_-offset_fim
      ENDI
      buffer_=SPAC(tambuf_)
      FREAD(area_,@buffer_,tambuf_)
      buffer_=STRTRAN(buffer_,CHR(12)+CHR(13),CHR(13)+CHR(10))
      FOR t_ = 1 TO qtlin_
         IF l_arq[1]!=CHR(0)
            offset_ini+=LEN(l_arq[1])+2
         ENDI
         ADEL(l_arq,1)
         qts_++
         IF LEN(buffer_)<3
            l_arq[maxlin_]=CHR(0)
            IF l_arq[2]=CHR(0)
               EXIT
            ENDI
         ELSE
            l_arq[maxlin_]=PARSE(@buffer_,CHR(13)+CHR(10))
            offset_fim+=LEN(l_arq[maxlin_])+2
         ENDI
      NEXT
   ENDI
ENDI
i_=0
FOR t_ = 1 TO maxlin_
   i_=MAX(i_,LEN(l_arq[t_]))
NEXT
IF (qttab_<0 .AND. tablin_>1) .OR.;
   (qttab_>0 .AND. tablin_<i_-10)
   tablin_+=qttab_
   qts_=maxlin_
ENDI
IF qts_!=0
   SCROLL(lsup_,csup_,linf_,cinf_,qts_)
   ini_=IF(qts_>0,maxlin_-qts_+1,1)
   fim_=IF(qts_>0,maxlin_,ABS(qts_))
   FOR t_=ini_ TO fim_
      lin_=l_arq[t_]
      IF !EMPTY(lin_)  //.AND.!OK_PRINT(lin_)
         IF AT(" "+CHR(8),lin_)>0
            lin_=SUBS(lin_,AT(" "+CHR(8),lin_)+2)
         ENDI
      ENDI
      XPUL=0
      IF SUBSTR(lin_,1,1)==CHR(13)
         XPUL=1
      ENDIF
      @ lsup_+t_-1,csup_ SAY SUBS(lin_,tablin_+XPUL,CINF_-CSUP_+1)
   NEXT
ENDI
buffer_=""
RETU

FUNC DESEMBARALHA(_lin)
LOCAL ret_val:="", i, j, x, k
x="17389280912340243"
k=0
FOR i=1 TO LEN(_lin)
   k++
   IF k>LEN(x)
      k=1
   ENDI
   j=ASC(SUBS(_lin,i,1))-VAL(SUBS(x,k,1))
   IF j<0
      j=255+j
   ENDI
   ret_val+=CHR(j)
NEXT
RETU ret_val

********************************
function PARSE(Arg1, Arg2)
********************************
   local Local1, Local2
   if (PCount() < 2)
      Arg2:= ","
   endif
   Local2:= At(Arg2, Arg1)
   if (Local2 > 0)
      Local1:= Left(Arg1, Local2 - 1)
      Arg1:= SubStr(Arg1, Local2 + Len(Arg2))
   else
      Local1:= Arg1
      Arg1:= ""
   endif

return Local1


***********************
function INKEY_T(delay)
***********************
local time_delay, time_inic, alerta_som

time_delay = time()
time_inic  = time()
alerta_som = .t.

do while inkey() == 0
   if delay <> 0 .and. (secs(time()) - secs(time_delay)) > delay
      return 0
   elseif (secs(time()) - secs(time_inic)) >= 600
      abordar()
      time_inic  = time()
      alerta_som = .t.
   elseif (secs(time()) - secs(time_inic)) >= 540 .and. alerta_som
      tone(400,8)
      alerta_som = .f.
   endif
enddo

return lastkey()


****************
function ABORDAR
****************
local WTT_EXIT, WTT_COR, WTT_CURSOR, WTT_ROW, WTT_COL

WTT_EXIT   = savescreen(00,00,24,79)
WTT_COR    = setcolor()
WTT_CURSOR = setcursor()
WTT_ROW    = row()
WTT_COL    = col()

setcolor("GR+/B")
setcursor(0)

@ 00,00 clear to 24,79

@ 09,37 say chr(218)+chr(196)+chr(196)+chr(196)+chr(191) 
@ 10,37 say chr(179)+chr(032)+chr(032)+chr(032)+chr(032)  
@ 11,37 say chr(192)+chr(196)+chr(196)+chr(196)+chr(191)
@ 12,37 say chr(032)+chr(032)+chr(032)+chr(032)+chr(179)
@ 13,37 say chr(192)+chr(196)+chr(196)+chr(196)+chr(217)

if inkey(2) == 0
   @ 09,37 say chr(194)+chr(032)+chr(032)+chr(032)+chr(194)
   @ 10,37 say chr(179)+chr(032)+chr(032)+chr(032)+chr(179)
   @ 11,37 say chr(192)+chr(196)+chr(196)+chr(196)+chr(180)
   @ 12,37 say chr(032)+chr(032)+chr(032)+chr(032)+chr(179)
   @ 13,37 say chr(032)+chr(032)+chr(032)+chr(032)+chr(193)

   if inkey(2) == 0
      @ 09,37 say chr(196)+chr(196)+chr(196)+chr(196)+chr(191)
      @ 10,37 say chr(032)+chr(032)+chr(032)+chr(032)+chr(179)
      @ 11,37 say chr(032)+chr(196)+chr(196)+chr(196)+chr(180)
      @ 12,37 say chr(032)+chr(032)+chr(032)+chr(032)+chr(179)
      @ 13,37 say chr(196)+chr(196)+chr(196)+chr(196)+chr(217)

      if inkey(2) == 0
         @ 09,37 say chr(218)+chr(196)+chr(196)+chr(196)+chr(191)
         @ 10,37 say chr(032)+chr(032)+chr(032)+chr(032)+chr(179)
         @ 11,37 say chr(218)+chr(196)+chr(196)+chr(196)+chr(217)
         @ 12,37 say chr(179)+chr(032)+chr(032)+chr(032)+chr(032)
         @ 13,37 say chr(192)+chr(196)+chr(196)+chr(196)+chr(217)
  
         if inkey(2) == 0
            @ 09,37 say chr(032)+chr(032)+chr(191)+chr(032)+chr(032)
            @ 10,37 say chr(032)+chr(032)+chr(179)+chr(032)+chr(032)
            @ 11,37 say chr(032)+chr(032)+chr(179)+chr(032)+chr(032)
            @ 12,37 say chr(032)+chr(032)+chr(179)+chr(032)+chr(032)
            @ 13,37 say chr(032)+chr(032)+chr(193)+chr(032)+chr(032)

            tone(1500,3)
            fim()
         endif             
      endif             
   endif             
endif             

setcolor(WTT_COR)
setcursor(WTT_CURSOR)
@ WTT_ROW, WTT_COL say ""   && posiciona cursor

restscreen(00,00,24,79,WTT_EXIT)

return .t.


******************
Function help_PEND 
******************
private AREA, WVAR

AREA    = Alias()
WVAR    = READVAR()
WWCHAVE = ""

Select PENDENC
ordsetfocus('SINC002')
Go Top

private aCAMPO[2], aPICT[2], aHEAD[2]

aCAMPO[1] = "CODIGO"     
aCAMPO[2] = "DESCRICAO"           

aPICT[1]  = "9999"
aPICT[2]  = ""

aHEAD[1]  = "C"+chr(162)+"digo"
aHEAD[2]  = "Descri"+Chr(135)+Chr(132)+"o" 
                     
SC_HELP   = savescreen(06,09,22,71)

SOMBRA(07,11,22,71,8)

setcolor(COR_CONS+","+COR_TEXT+",,,"+COR_CONS)
@ 06,09 clear to 21,70   
@ 07,09 to 20,70 double

@ 06,10 say chr(16)

@ 09,09 say chr(199)
@ 09,70 say chr(182)

@ 21,11 say "Help-PENDENCIAS" 
@ 21,53 say "PgDw/PgUp - "+chr(24)+chr(26)+chr(25)+chr(27)

dbedit(08,10,19,69,aCAMPO,"busca_indice",aPICT,aHEAD,chr(196),chr(179),"","")

setcolor(COR_TEXT+","+COR_GETS+",,,"+COR_TEXT)

restscreen(06,09,22,71,SC_HELP)

If ! empty(WVAR)
   &WVAR = CODIGO
Else
   WPEND = CODIGO
Endif

Select &AREA

Return .F.


*******************
Function help_GRUPO
*******************
private AREA, WVAR

AREA    = Alias()
WVAR    = READVAR()
WWCHAVE = ""

Select GRUPO
ordsetfocus('SINC002')
Go Top

private aCAMPO[2], aPICT[2], aHEAD[2]

aCAMPO[1] = "CODIGO"     
aCAMPO[2] = "DESCRICAO"           

aPICT[1]  = ""
aPICT[2]  = ""

aHEAD[1]  = "C"+chr(162)+"digo"
aHEAD[2]  = "Descri"+Chr(135)+Chr(132)+"o" 
                     
SC_HELP   = savescreen(06,09,22,71)

SOMBRA(07,11,22,71,8)

setcolor(COR_CONS+","+COR_TEXT+",,,"+COR_CONS)
@ 06,09 clear to 21,70   
@ 07,09 to 20,70 double

@ 06,10 say chr(16)

@ 09,09 say chr(199)
@ 09,70 say chr(182)

@ 21,11 say "Help-GRUPOS" 
@ 21,53 say "PgDw/PgUp - "+chr(24)+chr(26)+chr(25)+chr(27)

dbedit(08,10,19,69,aCAMPO,"busca_indice",aPICT,aHEAD,chr(196),chr(179),"","")

setcolor(COR_TEXT+","+COR_GETS+",,,"+COR_TEXT)

restscreen(06,09,22,71,SC_HELP)

If ! empty(WVAR)
   &WVAR  = CODIGO
Else
   WGRUPO = CODIGO
Endif

Select &AREA

Return .F.


******************
Function help_LOTE
******************
private AREA, WVAR

AREA    = Alias()
WVAR    = READVAR()
WWCHAVE = ""

Select LOTE
ordsetfocus('SINC002')
Go Top

private aCAMPO[2], aPICT[2], aHEAD[2]

aCAMPO[1] = "CODIGO"     
aCAMPO[2] = "DESCRICAO"           

aPICT[1]  = ""
aPICT[2]  = ""

aHEAD[1]  = "C"+chr(162)+"digo"
aHEAD[2]  = "Descri"+Chr(135)+Chr(132)+"o" 
                     
SC_HELP   = savescreen(06,09,22,71)

SOMBRA(07,11,22,71,8)

setcolor(COR_CONS+","+COR_TEXT+",,,"+COR_CONS)
@ 06,09 clear to 21,70   
@ 07,09 to 20,70 double

@ 06,10 say chr(16)

@ 09,09 say chr(199)
@ 09,70 say chr(182)

@ 21,11 say "Help-SUB-GRUPO" 
@ 21,53 say "PgDw/PgUp - "+chr(24)+chr(26)+chr(25)+chr(27)

dbedit(08,10,19,69,aCAMPO,"busca_indice",aPICT,aHEAD,chr(196),chr(179),"","")

setcolor(COR_TEXT+","+COR_GETS+",,,"+COR_TEXT)

restscreen(06,09,22,71,SC_HELP)

If ! empty(WVAR)
   &WVAR = CODIGO
Else
   WLOTE = CODIGO
Endif

Select &AREA

Return .F.


************************************************************************
Function browse_REL(ARQ_, L_SUP_, C_SUP_,L_INF_, C_INF_,COR, MOLD, STEL)
************************************************************************
* arq_ = ARQUIVO TEXTO
* l_sup_, c_sup_, l_inf_, c_inf_ = COORDENADAS DE LINHA E COLUNA
* cor  = QUAL A COR                                (DEFAULT = W+/B)
* mold = SE VAI TER MOLDURA                        (DEFAULT = .T.)
* stel = SE VAI SALVAR E RESTAURAR A TELA NO FINAL (DEFAULT = .T.)

LOCAL cur_atual, cor_atual
mold:=IF(mold=NIL,.T.,mold)
stel:=IF(stel=NIL,.T.,stel)
IF mold
   PRIV l_arq[l_inf_-l_sup_+1], maxlin_, area_, tablin_, fim_arq_, offset_ini,;
        offset_fim, lsup_:=l_sup_+1, csup_:=c_sup_+1,linf_:=l_inf_-1, t_w, t_r, ;
        cinf_:= c_inf_-1, cod_sos:=55, t_c, t_s, t_d
ELSE
   PRIV l_arq[l_inf_-l_sup_+1], maxlin_, area_, tablin_, fim_arq_, offset_ini,;
        offset_fim, lsup_:=l_sup_, csup_:=c_sup_,linf_:=l_inf_, t_w, t_r, ;
        cinf_:= c_inf_, cod_sos:=55, t_c, t_s, t_d
ENDIF
t_w:=SETKEY(K_CTRL_W,NIL); t_r:=SETKEY(K_CTRL_R,NIL)
t_c:=SETKEY(K_CTRL_C,NIL); t_s:=SETKEY(K_CTRL_S,NIL)
t_d:=SETKEY(K_CTRL_D,NIL)
IF stel
   t_t:=SAVESCREEN(l_sup_, c_sup_, l_inf_, c_inf_)
ENDIF
IF AT(".",arq_)=0
   arq_+=".prn"
ENDI
IF (area_:=FOPEN(arq_))<0
   RETURN .F.     // ERRO AO ABRIR ARQUIVO
ENDI
cor=IF(cor=NIL,"W+/B",cor)
cor_atual=SETCOLOR(cor)
cur_atual:=SETCURSOR(0)

setcolor(COR_TEXT)
@ 24,00 CLEAR TO 24,79                    && LIMPA O AGUARDE NA LINHA 24
centro(24,"Arquivo Gerado : "+ARQ_)

SETCOLOR(cor)

@ l_sup_,c_sup_ CLEAR TO l_inf_,c_inf_
IF mold
   @ l_sup_,c_sup_ TO l_inf_,c_inf_
ENDIF
maxlin_=linf_-lsup_+1
AFILL(l_arq,"")
offset_ini:=offset_fim := 0
tablin_ = 1
fim_arq_=FSEEK(area_,0,2)
FSEEK(area_,0)
MONTA_LIN(maxlin_,0,.F.)
offset_ini=0
DO WHILE .t.

   tecl_p=inkey_t(0)

   IF SETKEY(tecl_p)!=NIL
      EVAL(SETKEY(tecl_p))
      tecl_p=0
   ENDI
   DO CASE
      CASE tecl_p=K_ESC
         EXIT

      CASE tecl_p=K_DOWN
         MONTA_LIN(1,0)

      CASE tecl_p=K_UP
         MONTA_LIN(-1,0)

      CASE tecl_p=K_CTRL_PGUP
         offset_ini:=offset_fim := 0
         AFILL(l_arq,"")
         MONTA_LIN(maxlin_,0)
         offset_ini=0

      CASE tecl_p=K_CTRL_PGDN
         offset_ini:=offset_fim := fim_arq_
         AFILL(l_arq,CHR(0))
         @ l_sup_+1,c_sup_+1 CLEAR TO l_inf_-1,c_inf_-1
         MONTA_LIN(-1*(maxlin_),0)

      CASE tecl_p=K_RIGHT
         MONTA_LIN(0,10)   // ELE PULA 10 ESPACOS, PODE SER ALTERADO

      CASE tecl_p=K_LEFT
         MONTA_LIN(0,-10)  // ELE PULA 10 ESPACOS, PODE SER ALTERADO

      CASE tecl_p=K_PGDN
         MONTA_LIN(maxlin_-1,0)

      CASE tecl_p=K_PGUP
         MONTA_LIN(-1*(maxlin_-1),0)

   END CASE
ENDD
FCLOSE(area_)
SETKEY(K_CTRL_W,t_w); SETKEY(K_CTRL_R,t_r)
SETKEY(K_CTRL_C,t_c); SETKEY(K_CTRL_S,t_s)
SETKEY(K_CTRL_D,t_d)
IF stel
   RESTSCREEN(l_sup_, c_sup_, l_inf_, c_inf_, t_t)
ENDIF
SETCOLOR(COR_TEXT)
@ 24,00 CLEAR TO 24,79
SETCURSOR(cur_atual)
SETCOLOR(cor_atual)

RETURN


********************
Function iSEQUENCIAL
********************
Local IQ, CR

DBAPPEND()

If NETERR()

   IQ = Savescreen(12,18,17,63)
   CR = Setcolor()

   SOMBRA(13,20,17,63,8)

   Setcolor(COR_CONS) 
   @ 12,18 Clear To 16,62
   @ 12,18 To 16,62 

   Setcolor(COR_CONS+"*") 
   @ 13,25 Say "         Aten"+Chr(135)+Chr(132)+"o...           "

   Setcolor(COR_CONS) 
   @ 15,25 Say "  Erro na inclus"+Chr(132)+"o do registro!  "

   Inkey(4)

   Restscreen(12,18,17,63,IQ)
   Setcolor(CR)

   Return .F.

Endif

Return .T.


*****************
Function help_GRP
*****************
Local COR
Local SC_HELP

COR     = Setcolor()
SC_HELP = Savescreen(06,09,22,71)

Select GRUPOS
ordsetfocus('SINC002')
Go Top

Private aCAMPO[2], aPICT[2], aHEAD[2]

aCAMPO[1] = "CODIGO"
aCAMPO[2] = "GRUPO"

aPICT[1]  = "999"
aPICT[2]  = ""

aHEAD[1]  = "C"+Chr(162)+"digo"
aHEAD[2]  = "Grupos"

SOMBRA(07,16,22,66,8)

Setcolor("N/W,W+/R,,,N/W")
@ 06,14 Clear To 21,65   
@ 06,14 To 20,65 double

@ 08,14 Say Chr(199)
@ 08,65 Say Chr(182)

@ 21,16 Say "Help-GRUPOS   P-Pesquisa"
@ 21,49 Say "PgDw/PgUp - "+Chr(24)+Chr(26)+Chr(25)+Chr(27)

DBEDIT(07,15,19,64,aCAMPO,"GRO",aPICT,aHEAD,Chr(196),Chr(179),"","")

Restscreen(06,09,22,71,SC_HELP)

Setcolor(COR)

Return .T.


************
Function GRO
************
Local Getlist :={}

Parameters MODO

If MODO >= 4

   If Lastkey() = 27 
      Return 0
   Endif

   If Lastkey() = 13

      WVARR = READVAR()

      If ! Empty(WVARR)
         If VALTYPE(&WVARR) == "C" 
            &WVARR = Strzero(Val(CODIGO),3)
         Endif
      Endif

      Return 0

   Endif

Endif

Return 1


**********************
Function help_PRODUTOS
**********************
Local COR
Local SC_HELP

COR     = Setcolor()
SC_HELP = Savescreen(06,00,22,79)

Select PRODUTOS
ordsetfocus('SINC002')
Go Top

Private aCAMPO[5], aPICT[5], aHEAD[5]

aCAMPO[1] = "CODIGO"
aCAMPO[2] = "PRODUTO"
aCAMPO[3] = "UN"
aCAMPO[4] = "((SDANT + AQUIS) - BAIXAS)"
aCAMPO[5] = "CUSTO"

aPICT[1]  = "9999"
aPICT[2]  = ""
aPICT[3]  = ""
aPICT[4]  = "99999"
aPICT[5]  = "@R@E 9999.9999"

aHEAD[1]  = "C"+Chr(162)+"digo"
aHEAD[2]  = "Produto"
aHEAD[3]  = "UN"
aHEAD[4]  = "Sd.Atual"
aHEAD[5]  = "Custo"

SOMBRA(07,01,22,79,8)

Setcolor("N/W,W+/R,,,N/W")
@ 06,00 Clear To 21,78   
@ 06,00 To 20,78 Double

@ 08,00 Say Chr(199)
@ 08,78 Say Chr(182)

@ 21,01 Say "Help-PRODUTOS   P-Pesquisa"
@ 21,62 Say "PgDw/PgUp - "+Chr(24)+Chr(26)+Chr(25)+Chr(27)

DBEDIT(07,01,19,77,aCAMPO,"FC",aPICT,aHEAD,Chr(196),Chr(179),"","")

Restscreen(06,00,22,79,SC_HELP)

Setcolor(COR)

Return .T.


***********
Function FC
***********
Local Getlist :={}

Parameters MODO

If MODO >= 4

   If Lastkey() = 27 
      Return 0
   Endif

   If Lastkey() = 13

      WVARR = READVAR()

      If ! Empty(WVARR)
         If VALTYPE(&WVARR) == "C" 
            &WVARR = CODIGO
         Endif
      Endif

      Return 0

   Endif

   If Lastkey() = 80 .Or. Lastkey() = 112

      WWPRODUTO = Space(40)
      CURSOR    = Setcursor()

      Setcursor(1)

      @ ROW(),11 Get WWPRODUTO
      READ SAVE

      Set Softseek On
      Seek Upper(WWPRODUTO)
      Set Softseek Off

      Setcursor(CURSOR)

      Return 2

   Endif

Endif

Return 1


*********************************************
Function CONFIRMA2(LIN, COL, STRING, DEFAULT)
*********************************************
Local TECLA

Setcursor(1)  

COLUNA = 0
TECLA  = 0

@ LIN,COL Say STRING

If DEFAULT = "S"
   @ LIN,COL+Len(STRING)+1 Say "Sim"
   @ LIN,COL+Len(STRING)+1 Say ""
   RESP = "S"
Else
   @ LIN,COL+Len(STRING)+1 Say "N"+Chr(132)+"o"
   @ LIN,COL+Len(STRING)+1 Say ""
   RESP = "N"
Endif

While .T.

   TECLA = Inkey(0)

   If Lastkey() = 27 .Or. Lastkey() = 13
      Exit
   Endif

   If TECLA = 83 .Or. TECLA = 115
      RESP = "S"                       
      @ LIN,COL+Len(STRING)+1 Say "Sim"
      @ LIN,COL+Len(STRING)+1 Say ""
      Exit
   Elseif TECLA = 78 .Or. TECLA = 110
      RESP = "N"                       
      @ LIN,COL+Len(STRING)+1 Say "N"+Chr(132)+"o"
      @ LIN,COL+Len(STRING)+1 Say ""
      Exit
   Endif 

Enddo  

Setcursor(0)  

Return RESP


******************
Function CHECK_PRN
******************

CL  = Setcursor()
COR = Setcolor()
SCR = Savescreen(17,29,22,78) 
WA  = 0
WI  = 0
WM  = "Impressora Desativada! Pressione ESC para Cancelar."+Space(45)

Setcursor(0)

SOMBRA(18,31,22,78,8)

Setcolor("W+/R")
@ 17,29 Clear To 21,77
@ 17,29 To 21,77 

While ! ISPRINTER() .And. Lastkey() <> 27

    For I=1 To 46 Step 2

        Setcolor("W+/R")
        @ 18,30+I Say Chr(219)
  
        Inkey(.1)
 
        @ 20,75-WA Say Right(Left(WM,WI),45)

        If Lastkey() = 27
           Exit
        Endif

        WI++
        WA++

        If WA > 44
           WA = 44
        Endif         
 
        If WI > Len(WM)
           WI = 1
           WA = 0
        Endif
        
    Next I

    For I=1 To 46 Step 2

        Setcolor("GR+/R")
        @ 18,30+I Say Chr(219)

        Inkey(.1)

        Setcolor("W+/R")
        @ 20,75-WA Say Right(Left(WM,WI),45)

        If Lastkey() = 27
           Exit
        Endif

        WI++
        WA++

        If WA > 44
           WA = 44
        Endif         
 
        If WI > Len(WM)
           WI = 1
           WA = 0
        Endif

    Next I

Enddo

Restscreen(17,29,22,78,SCR) 

Setcursor(CL)

Setcolor(COR) 

Return Lastkey()

