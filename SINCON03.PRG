*********************************************************************
****  Sistema......: SINCON - Sistema Integrado de Controle      ****
****  Versao.......: 3.0 - RACOES PILOTO                         ****
****  Programa.....: SINCON03.PRG                                ****
****  Finalidade...: Utilitarios Diversos                        ****
****  Programador..: Sergio Alexandre de Carvalho                ****
****                                                             ****
****                 Copyright(C) SERIAL SOFTWARE                ****
****                                                             ****
****  Compilador...: CLIPPER (R) Versao 5.01                     ****
****  Linkeditor...: RTLink (R)  Versao 3.13                     ****
*********************************************************************

*******************
function UTILITARIO
*******************

OPC1 = 1

while .t.

   setcolor(COR_CAB)   
   @ 03,42 say "UTILITARIOS                        " 
   setcolor(COR_TEXT)

   private OPCAO[3]
   OPCAO[1] = " Cadastro de Usu"+chr(160)+"rios  " 
   OPCAO[2] = " Altera"+chr(135)+chr(132)+"o de Senha    " 
   OPCAO[3] = " Esc - Fim             " 

   private ACESSO[3]
   ACESSO[1] = SENHA_MESTRE 
   ACESSO[2] = .t.
   ACESSO[3] = .t.

   If Alltrim(WPUSUARIO) = "JULIANO" .Or. Alltrim(WPUSUARIO) = "SERGIO"
      ACESSO[1] = .t.
   Endif

   private LETRA[3]
   LETRA[1] = "U"
   LETRA[2] = "A"
   LETRA[3] = "F"

   private POS[3]
   POS[1]= 13
   POS[2] = 1
   POS[3] = 7
   
   private OBS_MENU[3]
   OBS_MENU[1] = "Cadastramento de Usu"+chr(160)+"rios..."
   OBS_MENU[2] = "Altera"+chr(135)+chr(132)+"o de Senha..."
   OBS_MENU[3] = "Retornar ao Menu Principal..."                  
            
   OPC1 = achoise(OPC1,09,27,13,51,3,OPCAO,ACESSO,LETRA,POS,OBS_MENU,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)
   
   If OPC1 == 1 
      senha()      
   elseif OPC1 == 2
      altsenha()
   elseif OPC1 == 3 .Or. OPC1 = 0
      return .T.
   endif

enddo

return .t.

*****************
function ALTSENHA
*****************

@ 05,00 clear to 22,79
@ 24,00 clear to 24,79

setcolor(COR_CONS)
@ 24,00 say "COPIA"
@ 24,73 say "ESC-FIM"
setcolor(COR_TEXT)

setcolor(COR_CONS)

sombra(09,12,17,72,8)
@ 08,10 clear to 16,70
@ 08,10 to 16,70

@ 10,12 say "Usu"+chr(160)+"rio........: "+WPUSUARIO

WSENHA   = ""
I        = 1
A        = 0

select SENHA
ordsetfocus('SINC001')
seek WPUSUARIO

@ 11,12 say "Senha de Acesso:"

do while I <= 3
   do while A <= 10
     inkey(0)

     @ 11,30+A say "*"
   
      A = A + 1
    
      if lastkey() == 27 
         return .t.
      elseif lastkey() == 13
         exit
      endif

      WSENHA = WSENHA + chr(lastkey())  
   enddo

   if ! (alltrim(WSENHA) == alltrim(uncrp(SENHA)))
      if I == 3
         centro(14, "Senha Inv"+chr(160)+"lida...!  ")
         centro(15,"Desculpe...! Acesso N"+chr(132)+"o Autorizado.")
         inkey(0)

         return .t.
      endif

      centro(14, "Senha Inv"+chr(160)+"lida...")
      inkey(1)
      @ 14,11 clear to 14,69

      @ 11,30 say space(11)
   else    
      exit
   endif

   I = I + 1
   A = 0
   WSENHA = ""
enddo

WSENHA_N = ""
I        = 1
A        = 0

@ 11,30 say space(11)

@ 11,12 say "Nova Senha.....:"

do while A <= 10
   inkey(0)
  
   @ 11,30+A say "*"
   
   A = A + 1
     
   if lastkey() == 27 
      return .t.
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

     @ 12,30+A say "*"
   
      A = A + 1
    
      if lastkey() == 27 
         return .t.
      elseif lastkey() == 13
         exit
      endif

      WSENHA = WSENHA + chr(lastkey())  
   enddo

   if ! (alltrim(WSENHA) == alltrim(WSENHA_N))
      if I == 3
         centro(14, "Senha N"+chr(132)+"o Confere...!  ")
         centro(15,"Desculpe...! Altera"+chr(135)+chr(132)+"o N"+chr(132)+"o Efetuada.")
         inkey(0)

         return .t.
      endif

      centro(14, "Senha N"+chr(132)+"o Confere...!  ")
      inkey(1)
      @ 14,11 clear to 14,69

      @ 12,30 say space(11)
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
   return .f.
endif

replace SENHA with crp(left(WSENHA+space(10),10))

grava()

WPSENHA = uncrp(SENHA)

centro(15,"Altera"+chr(135)+chr(132)+"o Efetuada Com Sucesso.")

inkey(0)

return .t.


**************
function SENHA
**************

select SENHA
ordsetfocus('SINC001')

@ 05,00 clear to 22,79
@ 24,00 clear to 24,79

setcolor(COR_CONS)
@ 24,00 say "SENHA"
@ 24,73 say "ESC-FIM"
setcolor(COR_TEXT)

private CAMPO[1]
CAMPO[1] = "space(01)+uncrp(USUARIO)+space(01)"

do while .t.
   select SENHA
   ordsetfocus('SINC001')
   go top

   @ 05,00 clear to 22,79

   keyboard(chr(65))

   setcolor(COR_MENU+","+COR_OPC)
   sombra(07,29,18,53,8)
   @ 06,27 to 17,51 
   dbedit(07,28,16,50,CAMPO,"UDF_SENHA","","","","","","")
   setcolor(COR_TEXT+","+COR_GETS+",,,"+COR_TEXT)

   if lastkey() == 27
      close all

      setcolor(COR_TEXT+"+*")  
      @ 24,69 say "Aguarde...!"
      setcolor(COR_TEXT)

       WDIRE = "HARMONIA"
       open_arq()

      return .t.
   endif
enddo 

******************
function UDF_SENHA
******************

if lastkey() == 13 
   return 0
elseif lastkey() == 27 
   return 0
elseif lastkey() == 22
   WT_UDF = savescreen(09,40,13,78)

   setcolor(COR_CONS+","+COR_GETS+",,,"+COR_CONS)
   @ 09,40 clear to 13,78 
   @ 09,40 to 13,78 double

   WUSUARIO = space(20)   

   @ 11,42 say "Usuario.: " get WUSUARIO    picture "@!"
   read_f()
   setcolor(COR_MENU+","+COR_OPC+",,,"+COR_TEXT)
   
   if lastkey() == 27
      restscreen(09,40,13,78,WT_UDF)
      return 1
   endif

   select SENHA
   ordsetfocus('SINC001')
   seek WUSUARIO

   if found()
      @ 24,10 clear to 24,70
      centro(24, "Usu"+chr(160)+"rio J"+chr(160)+" Cadastrado...")
      inkey(1)
      @ 24,10 clear to 24,70
      restscreen(09,40,13,78,WT_UDF)

      return 1
   endif

   set deleted off
   go top
   if deleted()
      if ! reglock()
         return 1
      endif

      recall
   else
      if ! inc_reg()
         return 1
      endif
   endif
   set deleted on
   
   replace USUARIO   with crp(WUSUARIO),;
           SENHA     with crp(space(10))
 
   grava()
   
   restscreen(09,40,13,78,WT_UDF)

   return 2   

elseif lastkey() == 7
   if recno() == 1
      return 1
   endif

   WT_UDF = savescreen(09,40,13,78)

   setcolor(COR_CONS+","+COR_GETS+",,,"+COR_CONS)
   @ 09,40 clear to 13,78 
   @ 09,40 to 13,78 double

   if reducao(11,42,"Confirma Exlus"+chr(132)+"o...:","N") == "S"
      setcolor(COR_MENU+","+COR_OPC+",,,"+COR_TEXT)
 
      if ! reglock()
         return 1
      endif
   
      replace USUARIO   with crp(space(20)),;
              SENHA     with crp(space(10))

      delete
   
      grava()

      go top

      restscreen(09,40,13,78,WT_UDF)
      return 2
   else
      setcolor(COR_MENU+","+COR_OPC+",,,"+COR_TEXT)

      restscreen(09,40,13,78,WT_UDF)
      return 1
   endif
endif

return 1

