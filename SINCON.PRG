*********************************************************************
****  Sistema......: SINCON - Sistema Integrado de Controle      ****
****  Programa.....: SINCON.PRG                                  ****
****  Finalidade...: Programa Principal                          ****
****  Programador..: Juliano Ribeiro                             ****
****  Fazenda......: Fazenda & Haras Harmonia                    ****
****                                                             ****
****                 Copyright(C) SERIAL SOFTWARE                ****
****                                                             ****
****  Compilador...: CLIPPER(R) Versao 5.02                      ****
****  Linkeditor...: Blinker(R) Versao 6.00                      ****
*********************************************************************
REQUEST DBFCDX     // Indice CDX 
REQUEST HB_LANG_PT // Portuguese language

PROCEDURE Main()

rddSetDefault( "DBFCDX" )
HB_LangSelect( "PT" )
HB_SetCodePage( "PT850" )

CRI  = ""        
ACDX = 0
TCDX = 0
AOF  = 0

SET EXCLUSIVE OFF
set key -7 to SCALC
set key -6 to CALENDARIO
set intens on
set deleted on
set talk off 
set echo off
set cons off
set bell off  
set scoreboard off
set date BRITISH
set softseek on
set decimals to 2
set fixed on
setcursor(0)

SET ERRORLOG TO Error.log ADDITIVE
SET CENTURY ON
SET EPOCH TO 1950

PPRINTER = .t.

if ! file("AUXILIAR\CCONFIG.CFG")
   COR_MENU2 = "08/05"
   COR_OPC2  = "02/08"
   COR_DEST2 = "15/05"
   COR_CAB2  = "01/08"
   COR_CONS2 = "01/08"
   COR_TEXT2 = "08/02"
   COR_GETS2 = "15/05"

   COR_MENU  = "W/R"
   COR_OPC   = "B/W"
   COR_DEST  = "GR+/R"
   COR_CAB   = "N/W"
   COR_CONS  = "N/W"
   COR_TEXT  = "W/B"
   COR_GETS  = "GR+/R"

   save all like COR_* to AUXILIAR\CCONFIG.CFG
else
   restore from AUXILIAR\CCONFIG.CFG additive
endif

setcolor(COR_TEXT+","+COR_GETS+",,,"+COR_TEXT)

PAPEL()

****************************************** SENHA

if ! file("AUXILIAR\SENHA.CDX")
   barraCDX(ACDX,TCDX,"AUXILIAR\SENHA")
   abre(AOF,"AUXILIAR\SENHA","E")
   index on uncrp(USUARIO) tag SINC001
   DBCLOSEAREA()
endif

abre(000,"AUXILIAR\SENHA")

*************************************************

PAPEL()

public WPSENHA, WPUSUARIO
private I, A

WPSENHA     = ""
WPUSUARIO   = space(20)
I           = 1
A           = 0
M_CONTROLE  = .f.

setcolor(COR_CONS)

sombra(09,12,17,72,8)
@ 08,10 clear to 16,70
@ 08,10 to 16,70

@ 10,12 say "Usu"+chr(160)+"rio........:"

do while .t. 

   @ 10,29 get WPUSUARIO picture "@!"
   read_f()
   @ 10,29 say WPUSUARIO

   if lastkey() == 27
      fim()
   endif   

   select SENHA
   ordsetfocus('SINC001')
   seek WPUSUARIO

   if ! found()
      centro(14, "Usu"+chr(160)+"rio n"+chr(132)+"o Cadastrado...")
      inkey(1)
      @ 14,11 clear to 14,69
      loop
   endif

   exit
enddo

if ! empty(uncrp(SENHA))
   @ 11,12 say "Senha de Acesso:"

   do while I <= 3
      do while A <= 10
         inkey(0)
     
         @ 11,29+A say Chr(254)
      
         A = A + 1
       
         if lastkey() == 27 
            fim()
         elseif lastkey() == 13
            exit
         endif
   
         WPSENHA = WPSENHA + chr(lastkey())  
      enddo
   
      if ! (alltrim(WPSENHA) == alltrim(uncrp(SENHA)))
         if I == 3
            centro(14, "Senha inv"+chr(160)+"lida...!  ")
            centro(15,"Desculpe..! Acesso n"+chr(132)+"o autorizado!")
            inkey(0)
   
            fim()
         endif
   
         centro(14, "Senha inv"+chr(160)+"lida...")
         inkey(1)
         @ 14,11 clear to 14,69
   
         @ 11,29 say space(11)
      else    
         exit
      endif
   
      I = I + 1
      A = 0
      WPSENHA = ""
   enddo
else
   WSENHA_N = ""
   I        = 1
   A        = 0

   @ 11,29 say space(11)
   @ 11,12 say "Nova Senha.....:"

   do while A <= 10
      inkey(0)
  
      @ 11,29+A say Chr(02)
   
      A = A + 1
     
      if lastkey() == 27 
         fim()
      elseif lastkey() == 13
         exit
      endif

      WSENHA_N = WSENHA_N + chr(lastkey())  
   enddo
   
   WSENHA   = ""
   I        = 1
   A        = 0
   
   @ 12,12 say "Confirme Senha.:"
   
   do while I <= 3
      do while A <= 10
        inkey(0)
   
        @ 12,29+A say Chr(254)
      
         A = A + 1
       
         if lastkey() == 27 
            fim()
         elseif lastkey() == 13
            exit
         endif
   
         WSENHA = WSENHA + chr(lastkey())  
      enddo

      if ! (alltrim(WSENHA) == alltrim(WSENHA_N))
         if I == 3
            centro(14, "Senha n"+chr(132)+"o confere..!  ")
            centro(15,"Desculpe...! Altera"+chr(135)+chr(132)+"o n"+chr(132)+"o realizada.")
            inkey(0)
   
            fim()
         endif
   
         centro(14, "Senha n"+chr(132)+"o confere..!  ")
         inkey(1)
         @ 14,11 clear to 14,69
   
         @ 12,29 say space(11)
      else    
         exit
      endif
   
      I = I + 1
      A = 0
      WSENHA = ""
   
   enddo
      
   select SENHA
   ordsetfocus('SINC001')
   seek WPUSUARIO
   
   if ! reglock()
      fim()
   endif
   
   replace SENHA with crp(left(WSENHA+space(10),10))
   
   grava()
   
   WPSENHA = uncrp(SENHA)
   
   centro(15,"Altera"+chr(135)+chr(132)+"o efetuada com sucesso!")
   
   inkey(2)
endif   
   
@ 09,11 clear to 15,69

if val(left(time(),2)) <= 12
   @ 09,13 say "Bom Dia...! "+WPUSUARIO
elseif val(left(time(),2)) <= 18
   @ 09,13 say "Boa Tarde...! "+WPUSUARIO
else
   @ 09,13 say "Boa Noite...! "+WPUSUARIO
endif

centro(12,upper(alltrim(semana(date())))+", "+strzero(day(date()),2)+" de "+alltrim(mes(month(date())))+" de "+strzero(year(date()),4))

@ 15,13 say "Inicio do Trabalho : "+time()

if recno() == 1
   tone(1001,0.7)
   tone(1002,0.4)

   SENHA_MESTRE = .t.
   
   @ 15,56 say "SENHA MESTRE"
else
   SENHA_MESTRE = .f.
endif

public WDIRE
WDIRE = "HARMONIA"

OPEN_ARQ()

setcolor(COR_TEXT)

for I = 1 to 80
    TELA = savescreen(00,01,24,80-I,24) 

    @ 00,80-I clear to 24,80-I

    setcolor(COR_CAB)
    @ 00,80-I say left(chr(201)+replicate(chr(205),39)+chr(203)+replicate(chr(205),38)+chr(187),I)
    @ 01,80-I say left(chr(186)+space(39)+chr(186)+space(01)+"SERIAL SOFTWARE"+space(22)+chr(186),I)
    @ 02,80-I say left(chr(186)+space(39)+chr(186)+space(38)+chr(186),I) 
    @ 03,80-I say left(chr(186)+space(01)+"SISTEMA INTEGRADO DE CONTROLE"+space(09)+chr(186)+space(38)+chr(186),I)
    @ 04,80-I say left(chr(200)+replicate(chr(205),39)+chr(202)+replicate(chr(205),38)+chr(188),I)
    setcolor(COR_TEXT)

    @ 23,80-I say left(replicate(chr(205),80),I)

    if I <> 80
       restscreen(00,00,24,79-I,TELA)
    endif
next

DIR = WDIRE+"\EMPRESA.CFG"
restore from &DIR additive

setcolor(COR_CAB)
@ 01,02 say CFG_EMPRES
setcolor(COR_TEXT)

setcursor(0)

OPC0 = 1

do while .t.

   setcolor(COR_CAB)   
   @ 03,42 say "MENU PRINCIPAL                 "
   setcolor(COR_TEXT)

   private OPCAO[5]
   OPCAO[1] = " Dados da Fazenda    " 
   OPCAO[2] = " Cadastro de Animais " 
   OPCAO[3] = " Estoque             " 
   OPCAO[4] = " Utilit"+chr(160)+"rios         " 
   OPCAO[5] = " Sa"+chr(161)+"da               " 

   private ACESSO[5]
   ACESSO[1] = .T.                  
   ACESSO[2] = .T.
   ACESSO[3] = .T.
   ACESSO[4] = .T.                 
   ACESSO[5] = .T.                 

   private LETRA[5]
   LETRA[1] = "D"
   LETRA[2] = "C"
   LETRA[3] = "E"
   LETRA[4] = "U"
   LETRA[5] = "S"

   private POS[5]
   POS[1] = 1
   POS[2] = 1 
   POS[3] = 1
   POS[4] = 1
   POS[5] = 1

   private OBS_MENU[5]
   OBS_MENU[1] = "Cadastro dos dados da fazenda!" 
   OBS_MENU[2] = "Cadastro de animais!"          
   OBS_MENU[3] = "Movimenta"+Chr(135)+Chr(132)+"o de estoque!"      
   OBS_MENU[4] = "Utilit"+chr(160)+"rios!"
   OBS_MENU[5] = "Sa"+chr(161)+"da do sistema!"

   OPC0 = ACHOISE(OPC0,09,28,15,50,0,OPCAO,ACESSO,LETRA,POS,OBS_MENU,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)
   
   if OPC0 == 0
      OPC0 = 5
   elseif OPC0 == 1
      empresa()
   elseif OPC0 == 2
      cadastro()
   elseif OPC0 == 3
      ESTOQUE()
   elseif OPC0 == 4
      utilitario()
   elseif OPC0 == 5
      fim()
   endif    

Enddo

Return .T.

******************
function VER_SENHA
******************
parameters WMODULO
private WAREA, WMODULO

WAREA = alias()

select SENHA
ordsetfocus('SINC001')
seek WPUSUARIO

select &WAREA

if WMODULO <> "USUARIO"
   return if(uncrp(SENHA->&WMODULO) == ".t.",.t.,.f.)
else
   return uncrp(SENHA->&WMODULO)
endif

****************
function AGUARDE
****************

setcolor(COR_GETS)
@ 24,00 clear to 24,79
@ 24,70 say "Aguarde..."
setcolor(COR_TEXT)

return .t.
