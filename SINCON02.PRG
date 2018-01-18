*********************************************************************
****  Sistema......: SINCON - Sistema Integrado de Controle      ****
****  Versao.......: FAZENDA & HARAS HARMONIA                    ****
****  Programa.....: SINCON02.PRG                                ****
****  Finalidade...: Controle de Animais                         ****
****  Programador..: Juliano Ribeiro                             ****
****                                                             ****
****                 Copyright(C) SERIAL SOFTWARE                ****
****                                                             ****
****  Compilador...: CLIPPER(R) Versao 5.02                      ****
****  Linkeditor...: Blinker(R) Versao 6.00                      ****
*********************************************************************

*****************
Function CADASTRO
*****************
OPC1 = 1

while .t.

   setcolor(COR_CAB)   
   @ 03,42 say "CADASTRO DE ANIMAIS                " 
   setcolor(COR_TEXT)

   private OPCAO[11]
   OPCAO[1]  = " Dados Individuais              " 
   OPCAO[2]  = " Dados Reprodutivos             " 
   OPCAO[3]  = " Sintomas Cl"+Chr(161)+"nicos e Tratamento "
   OPCAO[4]  = " Peso e Condi"+Chr(135)+Chr(132)+"o Corp"+Chr(162)+"rea       " 
   OPCAO[5]  = " Vacina"+Chr(135)+Chr(148)+"es                     " 
   OPCAO[6]  = " Produ"+Chr(135)+Chr(132)+"o de Leite              " 
   OPCAO[7]  = " Troca de Lote                  " 
   OPCAO[8]  = " Baixa de Animais               " 
   OPCAO[9]  = " Cadastros Diversos             " 
   OPCAO[10] = " Relat"+chr(162)+"rios Diversos            " 
   OPCAO[11] = " Esc - Fim                      " 

   private ACESSO[11]
   ACESSO[1]  = .T.
   ACESSO[2]  = .T.
   ACESSO[3]  = .T.
   ACESSO[4]  = .T.
   ACESSO[5]  = .T.
   ACESSO[6]  = .T.
   ACESSO[7]  = .T.
   ACESSO[8]  = .T.
   ACESSO[9]  = .T.
   ACESSO[10] = .T.
   ACESSO[11] = .T.
 
   private LETRA[11]
   LETRA[1]  = "D"  
   LETRA[2]  = "A"  
   LETRA[3]  = "S"  
   LETRA[4]  = "P"  
   LETRA[5]  = "V"  
   LETRA[6]  = "R"  
   LETRA[7]  = "O"  
   LETRA[8]  = "B"  
   LETRA[9]  = "T"  
   LETRA[10] = "L"  
   LETRA[11] = "F"  

   private POS[11]
   POS[1]  = 1
   POS[2]  = 2
   POS[3]  = 1 
   POS[4]  = 1 
   POS[5]  = 1 
   POS[6]  = 2
   POS[7]  = 3
   POS[8]  = 1
   POS[9]  = 6
   POS[10] = 3
   POS[11] = 7
 
   private OBS_MENU[11]
   OBS_MENU[1]  = "Cadastro individual de animais!"        
   OBS_MENU[2]  = "Cadastro de dados reprodutivos!"
   OBS_MENU[3]  = "Cadastro de sintomas cl"+chr(161)+"nicos/tratamentos!"
   OBS_MENU[4]  = "Cadastro de peso/condi"+chr(135)+"o cop"+chr(162)+"rea!"
   OBS_MENU[5]  = "Cadastro de vacina"+chr(135)+chr(148)+"es!" 
   OBS_MENU[6]  = "cadastro de produ"+chr(135)+chr(132)+"o de leite!"
   OBS_MENU[7]  = "Cadastro de troca de lote!"
   OBS_MENU[8]  = "Cadastro de baixa de animais!" 
   OBS_MENU[9]  = "Cadastros diversos!"          
   OBS_MENU[10] = "Relat"+chr(162)+"rios!"
   OBS_MENU[11] = "Retornar ao menu principal!"                  
 
   OPC1 = achoise(OPC1,07,22,19,55,1,OPCAO,ACESSO,LETRA,POS,OBS_MENU,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)

   if OPC1 == 0 .or. OPC1 == 11 
      Return .T.
   elseif OPC1 == 1
      ANIMAL()  
   elseif OPC1 == 2
      REPRODUCAO()
   elseif OPC1 == 3
      SINTOMA()      
   elseif OPC1 == 4
      PESO()
   elseif OPC1 == 5
      VACINA()
   elseif OPC1 == 6
      PROD_LEITE()
   elseif OPC1 == 7
      TROCA()
   elseif OPC1 == 8 
      BAIXA()
   elseif OPC1 == 9 
      cad_DIV()
   elseif OPC1 == 10
      rpr_DIV()
   endif

Enddo

Return .T.


***************
Function ANIMAL
***************
OPC2 = 1

while .t.

   setcolor(COR_CAB) 
   @ 03,42 say "CADASTRO DE ANIMAIS                " 
   setcolor(COR_TEXT)

   private OPCAO[5]
   OPCAO[1] = " Inclus"+chr(132)+"o   " 
   OPCAO[2] = " Altera"+chr(135)+chr(132)+"o  " 
   OPCAO[3] = " Consulta   " 
   OPCAO[4] = " Exclus"+chr(132)+"o   " 
   OPCAO[5] = " Esc - Fim  " 

   private ACESSO[5]
   ACESSO[1] = .t.
   ACESSO[2] = .t.
   ACESSO[3] = .t.
   ACESSO[4] = .t.
   ACESSO[5] = .t.

   private LETRA[5]
   LETRA[1] = "I"  
   LETRA[2] = "A"
   LETRA[3] = "C"
   LETRA[4] = "E"
   LETRA[5] = "F"

   private POS[5] 
   POS[1] = 1
   POS[2] = 1
   POS[3] = 1
   POS[4] = 1
   POS[5] = 7

   private OBS_MENU[5] 
   OBS_MENU[1] = "Inclus"+chr(132)+"o de animais!"  
   OBS_MENU[2] = "Altera"+chr(135)+chr(132)+"o de animais!"                               
   OBS_MENU[3] = "Consulta aos animais!"                                                       
   OBS_MENU[4] = "Exclus"+chr(132)+"o de animais!"                                                                                            
   OBS_MENU[5] = "Retorno ao menu principal!"                                                                                            

   OPC2 = achoise(OPC2,09,32,15,45,2,OPCAO,ACESSO,LETRA,POS,OBS_MENU,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)
   
   if OPC2 == 0 .or. OPC2 == 5
      return .t.
   elseif OPC2 == 1
      inc_animal()
   elseif OPC2 == 2
      alt_animal()
   elseif OPC2 == 3
      con_animal()
   elseif OPC2 == 4
      exc_animal()
   endif

enddo

return .t.


******************
Function SC_ANIMAL
******************

Setcolor(COR_TEXT)
@ 00,00 Clear To 22,79
@ 24,00 Clear To 24,79

Setcolor("N/W")
@ 00,00 Clear To 00,79
@ 00,01 Say CFG_EMPRES 
@ 00,53 Say "FICHA INDIVIDUAL DO ANIMAL"

Setcolor("GR+/B")
@ 02,01 Say "C"+Chr(162)+"digo....:"
@ 03,44 Say "Registro.:"

Setcolor(COR_TEXT)
@ 03,01 Say "Nome......:"
@ 04,01 Say "Ra"+Chr(135)+"a......:"
@ 05,01 Say "Pelagem...:"
@ 06,01 Say "Sexo......:"
@ 07,01 Say "Nascimento:" 

@ 02,44 Say "Categoria:    -"                            
@ 05,44 Say "Pasto....:"

Setcolor("GR+/B")
@ 08,01 Say "Mae.......:"
@ 08,44 Say "Pai......:"
@ 10,01 Say "Av"+Chr(162)+".......:"
@ 10,44 Say "Av"+Chr(147)+"......:"

Setcolor(COR_TEXT)   
@ 09,01 Say "Ra"+chr(135)+"a......:" 
@ 09,44 Say "Ra"+chr(135)+"a.....:" 

@ 11,01 Say "Ra"+chr(135)+"a......:" 
@ 11,44 Say "Ra"+chr(135)+"a.....:" 

Setcolor("BG/B")
@ 13,01 Say "Ult. Parto:"
@ 14,01 Say "Bezerro...:                                Sexo.....:"
@ 15,01 Say "Resenha...:                                Desmame..:"
@ 16,01 Say "Pai.......:"

Setcolor(COR_TEXT)

@ 18,01 Say "Grupo.....:    -"
@ 19,01 Say "Sub-Grupo.:    -"

@ 18,44 Say "ENTRADA..:"
@ 19,44 Say "SAIDA....:"

@ 20,01 Say "Pend"+Chr(136)+"ncia.:    -" 
@ 21,01 Say "Observa"+Chr(135)+Chr(132)+"o:" 

Return .T.


*******************
Function inc_ANIMAL
*******************
Save Screen

while .t. 

    select ANIMAL
    ordsetfocus('SINC001')
    Go Bottom

    WCODIGO     = Strzero(Val(CODIGO)+1,6)
    WCATEGORIA  = space(03)
    WNOME       = space(30)
    WDTNASC     = ctod(space(08))
    WRACA       = space(16)
    WCOR        = space(10)
    WNREG       = space(20)
    WSEXO       = space(01)

    WPASTO      = space(10)

    WMAE        = space(30)
    WMRACA      = space(16)
    WPAI        = space(30)
    WPRACA      = space(16)
    WAVO_1      = space(30)
    WRACA_AVO1  = space(16)
    WAVO_2      = space(30)
    WRACA_AVO2  = space(16)

    WGRUPO      = space(03)
    WLOTE       = space(03)

    WENTRADA    = ctod(space(08))
    WSAIDA      = ctod(space(08))

    WPEND       = 0
    WOBS1       = space(60)
    WOBS2       = space(60)

    sc_ANIMAL()

    setcolor(COR_CONS)
    @ 24,00 say "INCLUS"+chr(142)+"O"
    @ 24,73 say "ESC-FIM"

    setcolor(COR_TEXT)
    @ 02,13 Say WCODIGO           Picture "999999"

    While .T.

        @ 02,55 get WCATEGORIA    Picture "@!"       Valid busca_CATEG()

        @ 03,13 get WNOME         Picture "@S29"     Valid chk_NA()
        @ 04,13 get WRACA         
        @ 05,13 get WCOR          
        @ 06,13 get WSEXO         picture "!"        valid busca_SEXO(06,14)
        @ 07,13 get WDTNASC                          

        @ 03,55 get WNREG                            Valid chk_NR()
        @ 05,55 get WPASTO        picture "@!"       valid busca_PASTO()

        @ 08,13 get WMAE          Picture "@S29" 
        @ 09,13 get WMRACA        
        @ 08,55 get WPAI          Picture "@S24"        
        @ 09,55 get WPRACA        

        @ 10,13 get WAVO_1        Picture "@S29" 
        @ 11,13 get WRACA_AVO1    
        @ 10,55 get WAVO_2        Picture "@S24" 
        @ 11,55 get WRACA_AVO2    

        @ 18,13 get WGRUPO        Picture "@!"       Valid busca_GRUPO()
        @ 19,13 get WLOTE         Picture "@!"       Valid busca_LOTE()

        @ 18,55 get WENTRADA     
        @ 19,55 get WSAIDA       

        @ 20,13 get WPEND         Picture "@Z 999"   Valid busca_PEND()

        @ 21,13 get WOBS1        
        @ 22,13 get WOBS2        

        Select CATEG
        ordsetfocus('SINC001')
        Seek Upper(WCATEGORIA)

        If Found()
           @ 02,59 Say CATEGORIA
        Else
           @ 02,59 Say Space(20)
        Endif

        If WSEXO = "M"
           @ 06,14 Say "-Macho"
        Elseif WSEXO = "F"
           @ 06,14 Say "-Femea"
        Endif

        Select GRUPO
        ordsetfocus('SINC001')
        Seek WGRUPO

        If Found()
           @ 18,17 Say Left(DESCRICAO,25) 
        Else
           @ 18,17 Say Space(25)
        Endif

        Select LOTE
        ordsetfocus('SINC001')
        Seek WLOTE

        If Found()
           @ 19,17 Say Left(DESCRICAO,25) 
        Else
           @ 19,17 Say Space(25)
        Endif

        Select PENDENC
        ordsetfocus('SINC001')
        Seek WPEND             

        If Found()
           @ 20,17 Say DESCRICAO 
        Else
           @ 20,17 Say Space(30)
        Endif

        READ_F()

        Select ANIMAL

        If Lastkey() == 27
           Restore Screen
           Return .T.
        Endif

        if confirma(24,"Dados Corretos...(S/N)?","S") == "S"

           select ANIMAL

           set deleted off
           go top
           if deleted()
              if ! reglock()
                 loop
              endif

              recall
           else
              if ! inc_reg()
                 loop
              endif
           endif
           set deleted on

           save_ANIMAL()

           EXIT

        endif

    enddo

Enddo

Return .T.


***************
Function chk_NA
***************

If Lastkey() = 27
   Return .T.
Endif

If Empty(WNOME)
   Return .F.
Endif

Select ANIMAL
ordsetfocus('SINC002')
Seek Upper(WNOME)

If Found()
   ERROR("Nome de animal j"+Chr(160)+" cadastrado!",0)
   Return .F.
Endif

Return .T.


***************
Function chk_NR
***************

If Lastkey() = 27
   Return .T.
Endif

If Empty(WNREG)
   Return .F.
Endif

Select ANIMAL
ordsetfocus('SINC005')
Seek Upper(WNREG)

If Found()
   ERROR("Registro de animal j"+Chr(160)+" cadastrado!",0)
   Return .F.
Endif

Return .T.


**************************
Function busca_SEXO(LL,CC)
**************************

If WSEXO = "M"
   Setcolor(COR_TEXT)
   @ LL,CC Say "-Macho"
Elseif WSEXO = "F"
   Setcolor(COR_TEXT)
   @ LL,CC Say "-Femea"
Else
   ERROR("Informe: M-Macho ou F-Femea  ",0)
   Return .F.
Endif

Return .T.


*******************
Function busca_PEND
*******************

If Lastkey() = 27
   Return .T.
Endif

If Empty(WPEND)
   help_PEND()
   Return .F.
Endif

Select PENDENC
ordsetfocus('SINC001')
Seek WPEND

If ! Found()
   ERROR("N"+Chr(163)+"mero de pend"+Chr(136)+"ncia n"+Chr(132)+"o cadastrada!",0)
   Return .F.
Endif

@ 20,17 Say DESCRICAO

Return .T.


********************
Function busca_GRUPO
********************

If Lastkey() = 27
   Return .T.
Endif

If Empty(WGRUPO)
   help_GRUPO()
   Return .F.
Endif

Select GRUPO
ordsetfocus('SINC001')
Seek WGRUPO

If ! Found()
   ERROR("C"+Chr(162)+"digo de grupo n"+Chr(132)+"o cadastrado!",0)
   Return .F.
Endif

@ 18,17 Say Left(DESCRICAO,25)

Return .T.


*******************
Function busca_LOTE
*******************

If Lastkey() = 27
   Return .T.
Endif

If Empty(WLOTE)
   help_LOTE()
   Return .F.
Endif

Select LOTE
ordsetfocus('SINC001')
Seek WLOTE

If ! Found()
   ERROR("N"+Chr(163)+"mero de sub-grupo n"+Chr(132)+"o cadastrado!",0)
   Return .F.
Endif

@ 19,17 Say Left(DESCRICAO,25)

Return .T.


*******************
Function alt_ANIMAL
*******************
Save Screen

while .t. 

   sc_ANIMAL()

   WCODIGO = space(06)

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "ALTERA"+chr(128)+chr(142)+"O"
   setcolor(COR_TEXT)

   @ 02,13 get WCODIGO picture "999999" 
   READ_F()

   if empty(WCODIGO) .or. lastkey() == 27
      Restore Screen
      return .t.
   endif

   WCODIGO = Strzero(Val(WCODIGO),6)

   select ANIMAL
   ordsetfocus('SINC001')
   seek WCODIGO

   if ! found()
      centro(24,"C"+Chr(162)+"digo de animal n"+chr(132)+"o cadastrado...")
      inkey(2)
      @ 24,10 clear to 24,79
      loop
   endif

   WCODIGO      = CODIGO      
   WCATEGORIA   = CATEGORIA   
   WNOME        = NOME        
   WDTNASC      = DTNASC      
   WRACA        = RACA        
   WCOR         = COR         
   WSEXO        = SEXO        

   WNREG        = NREG
   WPASTO       = PASTO

   WMAE         = MAE
   WMRACA       = MRACA
   WPAI         = PAI
   WPRACA       = PRACA
   WAVO_1       = AVO_1      
   WRACA_AVO1   = RACA_AVO1  
   WAVO_2       = AVO_2      
   WRACA_AVO2   = RACA_AVO2  

   WENTRADA     = ENTRADA     
   WSAIDA       = SAIDA       

   WGRUPO       = GRUPO       
   WLOTE        = LOTE        
                              
   WPEND        = PEND
   WOBS1        = OBS1        
   WOBS2        = OBS2        

   While .T.

       sc_ANIMAL()

       setcolor(COR_CONS)
       @ 24,00 say "ALTERA"+chr(128)+chr(142)+"O"
       @ 24,73 say "ESC-FIM"

       setcolor(COR_TEXT)

       mostra_UP(WCODIGO)

       @ 02,13 Say WCODIGO                       

       @ 02,55 get WCATEGORIA    Picture "@!"       Valid busca_CATEG()

       @ 03,13 get WNOME         Picture "@S29"     
       @ 04,13 get WRACA         
       @ 05,13 get WCOR          
       @ 06,13 get WSEXO         picture "!"        valid busca_SEXO(06,14)
       @ 07,13 get WDTNASC                          

       @ 03,55 get WNREG                            
       @ 05,55 get WPASTO        picture "@!"       valid busca_PASTO()

       @ 08,13 get WMAE          Picture "@S29" 
       @ 09,13 get WMRACA        
       @ 08,55 get WPAI          Picture "@S24"        
       @ 09,55 get WPRACA        

       @ 10,13 get WAVO_1        Picture "@S29" 
       @ 11,13 get WRACA_AVO1    
       @ 10,55 get WAVO_2        Picture "@S24" 
       @ 11,55 get WRACA_AVO2    

       @ 18,13 get WGRUPO        Picture "@!"       Valid busca_GRUPO()
       @ 19,13 get WLOTE         Picture "@!"       Valid busca_LOTE()

       @ 18,55 get WENTRADA     
       @ 19,55 get WSAIDA       

       @ 20,13 get WPEND         Picture "@Z 999"    Valid busca_PEND()

       @ 21,13 get WOBS1        
       @ 22,13 get WOBS2        

       Select CATEG
       ordsetfocus('SINC001')
       Seek Upper(WCATEGORIA)

       If Found()
          @ 02,59 Say CATEGORIA
       Else
          @ 02,59 Say Space(20)
       Endif

       If WSEXO = "M"
          @ 06,14 Say "-Macho"
       Elseif WSEXO = "F"
          @ 06,14 Say "-Femea"
       Endif

       Select GRUPO
       ordsetfocus('SINC001')
       Seek WGRUPO

       If Found()
          @ 18,17 Say Left(DESCRICAO,25) 
       Else
          @ 18,17 Say Space(25)
       Endif

       Select LOTE
       ordsetfocus('SINC001')
       Seek WLOTE

       If Found()
          @ 19,17 Say Left(DESCRICAO,25) 
       Else
          @ 19,17 Say Space(25)
       Endif

       Select PENDENC
       ordsetfocus('SINC001')
       Seek WPEND             

       If Found()
          @ 20,17 Say DESCRICAO 
       Else
          @ 20,17 Say Space(30)
       Endif

       READ_F()

       Select ANIMAL

       If Lastkey() == 27
          Restore Screen
          Return .T.
       Endif

       if confirma(24,"Dados Corretos...(S/N)?","S") == "S"
          save_ANIMAL()
          exit
       endif

   Enddo

Enddo

Return .T.


************************
Function mostra_UP(WCOD)
************************

COR_UP = Setcolor()

Select ANIMAL

WREC = RECNO()
WORD = Indexord()

Select PARTO
ordsetfocus('SINC002')
Seek WCOD

If Found()

   Setcolor("BG/B")
   
   @ 13,13 Say DATA
   @ 14,13 Say BEZERRO

   If SEXO = "M"
      @ 14,55 Say "M-Macho"
   Elseif SEXO = "F"
      @ 14,55 Say "F-Femea"
   Endif

   @ 15,13 Say RESENHA
   @ 15,55 Say DESMAME

   WPAI = PAI

   Select ANIMAL
   ordsetfocus('SINC001')
   Seek WPAI
 
   If Found()
      @ 16,13 Say WPAI+"-"+NOME
   Else
      @ 16,13 Say Space(40)
   Endif

Else

   Setcolor("BG/B")
   
   @ 13,13 Say Space(10)
   @ 14,13 Say Space(30)
   @ 14,55 Say Space(07)
   @ 15,13 Say Space(20)
   @ 15,55 Say Space(08)
   @ 16,13 Say Space(40)

Endif

Setcolor(COR_UP)

Selec ANIMAL
ordsetfocus(WORD)
Go WREC

Return .T.


*******************
Function con_ANIMAL
*******************
Save Screen

While .T.

   OPC3 = 6

   select ANIMAL

   sc_ANIMAL()

   setcolor(COR_CONS)
   @ 24,00 say "CONSULTA"
   @ 24,73 say "ESC-FIM"

   SOMBRA(09,14,18,67,8)

   setcolor(COR_CONS)
   @ 08,12 clear to 17,66
   @ 08,12 to 17,66 

   @ 09,14 say "Consulta a partir do in"+chr(161)+"cio do arquivo..........[1]"
   @ 10,14 say "Consulta a partir do final do arquivo...........[2]"
   @ 11,14 say "Consulta a partir do c"+chr(162)+"digo do animal...........[3]"
   @ 12,14 say "Consulta a partir do registro do animal.........[4]"
   @ 13,14 say "Consulta a partir do nome do animal.............[5]"
   @ 14,14 say "Retorno aO menu principal.......................[6]"

   setcolor(COR_TEXT)
   @ 16,14 say "Escolha sua op"+chr(135)+chr(132)+"o de consulta...................[ ]"
   @ 16,63 get OPC3 picture "9" range 1,6
   read_f()

   If OPC3 == 6
      Restore Screen
      Return .T.
   Endif

   while .t.

      sc_ANIMAL()

      if OPC3 == 1
         select ANIMAL
         ordsetfocus('SINC001')
         go top

      elseif OPC3 == 2
         select ANIMAL
         ordsetfocus('SINC001')
         go bottom

      elseif OPC3 == 3

         WCODIGO = space(06)

         @ 02,13 get WCODIGO picture "999999"       
         read_f()

         WCODIGO = Strzero(Val(WCODIGO),6)

         if lastkey() == 27
            exit
         endif

         select ANIMAL
         ordsetfocus('SINC001')
         seek WCODIGO       

         if ! found()
            centro(24,"C"+Chr(162)+"digo de animal n"+chr(132)+"o cadastrado...")
            inkey(5)
            @ 24,10 clear to 24,79
            loop
         endif

      elseif OPC3 == 4

         WNREG = space(20)

         @ 03,55 get WNREG                          
         read_f()

         if lastkey() == 27
            exit
         endif

         select ANIMAL
         ordsetfocus('SINC005')
         seek WNREG 

         if ! found()
            centro(24,"N"+Chr(163)+"mero de registro n"+chr(132)+"o cadastrado...")
            inkey(5)
            @ 24,10 clear to 24,79
            loop
         endif

      elseif OPC3 == 5

         WNOME = space(30)          

         @ 03,13 get WNOME picture "@S29"
         read_f()

         if lastkey() == 27
            exit
         endif

         select ANIMAL
         ordsetfocus('SINC002')
         seek upper(WNOME)

         If EOF()
            Skip -1
         Endif

      endif

      Setcolor(COR_TEXT)
      centro(24,"PgDw-Pr"+chr(162)+"ximo   PgUp-Anterior   H-Hist"+Chr(162)+"rico")

      setcolor(COR_CONS)
      @ 24,00 say "CONSULTA"
      @ 24,73 say "ESC-FIM"

      KEY = 0

      while KEY  <> 27

          setcolor(COR_TEXT)

          @ 02,13 Say CODIGO                       
          @ 02,55 Say CATEGORIA    picture "@!"       
          @ 03,13 Say NOME         picture "@S29"
          @ 04,13 Say RACA         
          @ 05,13 Say COR          
          @ 06,13 Say SEXO         picture "!"        
          @ 07,13 Say DTNASC      

          @ 08,13 Say MAE          Picture "@S29" 
          @ 09,13 Say MRACA        
          @ 08,55 Say PAI          Picture "@S24"        
          @ 09,55 Say PRACA        

          @ 10,13 Say AVO_1        Picture "@S29" 
          @ 11,13 Say RACA_AVO1    
          @ 10,55 Say AVO_2        Picture "@S24" 
          @ 11,55 Say RACA_AVO2    

          @ 03,55 Say NREG
          @ 05,55 Say PASTO        Picture "@!"       

          @ 18,13 Say GRUPO       
          @ 19,13 Say LOTE

          @ 18,55 Say ENTRADA     
          @ 19,55 Say SAIDA       

          @ 20,13 Say PEND         Picture "999"

          @ 21,13 Say OBS1        
          @ 22,13 Say OBS2        

          WCODIGO    = CODIGO
          WCATEGORIA = CATEGORIA
          WSEXO      = SEXO
          WPEND      = PEND
          WGRUPO     = GRUPO 
          WLOTE      = LOTE

          mostra_UP(WCODIGO)

          Select CATEG
          ordsetfocus('SINC001')
          Seek Upper(WCATEGORIA)

          If Found()
             @ 02,59 Say CATEGORIA
          Else
             @ 02,59 Say Space(20)
          Endif

          If WSEXO = "M"
             @ 06,14 Say "-Macho"
          Elseif WSEXO = "F"
             @ 06,14 Say "-Femea"
          Endif

          Select GRUPO
          ordsetfocus('SINC001')
          Seek WGRUPO

          If Found()
             @ 18,17 Say Left(DESCRICAO,25) 
          Else
             @ 18,17 Say Space(25)
          Endif

          Select LOTE
          ordsetfocus('SINC001')
          Seek WLOTE

          If Found()
             @ 19,17 Say Left(DESCRICAO,25) 
          Else
             @ 19,17 Say Space(25)
          Endif

          Select PENDENC
          ordsetfocus('SINC001')
          Seek WPEND             

          If Found()
             @ 20,17 Say DESCRICAO 
          Else
             @ 20,17 Say Space(30)
          Endif

          Select ANIMAL

          KEY = inkey(0)

          if KEY == -7
             scalc()
          elseif KEY == -6
             calendario()
          elseif KEY == 3
             skip 
             if eof()
                skip -1
             endif
          elseif KEY == 18
             skip -1

          elseif KEY == 72 .Or. KEY = 104   // Hh - Painel

             BOX_PAINEL := Savescreen(11,55,21,79)
             COR_PAINEL := Setcolor()
  
             While .T.

                 SOMBRA(12,57,21,79,8)

                 Setcolor("N/W")
                 @ 11,55 Clear To 20,78
                 @ 12,55 To 12,78
                 @ 11,55 Say "   H i s t "+Chr(162)+" r i c o    "

                 Setcolor("N/W,W+/G,,,N/W")

                 @ 13,55 Prompt " Cio                    " 
                 @ 14,55 Prompt " Cobertura/Insemina"+Chr(135)+Chr(132)+"o  "
                 @ 15,55 Prompt " Prenhez                " 
                 @ 16,55 Prompt " Abortos                " 
                 @ 17,55 Prompt " Partos/Desmame         " 
                 @ 18,55 Prompt " Sintomas/Tratamento    "
                 @ 19,55 Prompt " Peso/Condi"+Chr(135)+Chr(132)+"o          "
                 @ 20,55 Prompt " Vacina"+Chr(135)+Chr(148)+"es             "

                 MENU TO OP

                 If OP = 0  
                    Exit
                 Elseif OP = 1  // cio
                    PAINEL(1)
                 Elseif OP = 2  // cobertura/inseminacao
                    PAINEL(2)
                 Elseif OP = 3  // prenhez
                    PAINEL(3)
                 Elseif OP = 4  // abortos
                    PAINEL(4)
                 Elseif OP = 5  // parto/desmame
                    PAINEL(5)
                 Elseif OP = 6  // sintomas/tratamentos
                    PAINEL(6)
                 Elseif OP = 7  // peso/cond. corporea 
                    PAINEL(7)
                 Elseif OP = 8  // vacinacoes           
                    PAINEL(8)
                 Endif

             Enddo

             Restscreen(11,55,21,79,BOX_PAINEL)
             
             Setcolor(COR_PAINEL)

          elseif KEY == 27
             exit
          endif

       enddo

       if OPC3 == 1 .or. OPC3 == 2 
          exit
       endif

    enddo

Enddo

Return .T.


********************
Function PAINEL(RPR)
********************

If RPR = 1                                     // CIO

   Select ANIMAL

   WANIMAL = CODIGO
   WARQRPR = Alltrim(Str(RANDOM()%99999999,8))
   WARQRPR = "RELAT\"+WARQRPR+".TXT"

   Set Print To &WARQRPR
   Set Device To Print
   Set Console Off

   cab_PAINEL("Historico: CIO")

   @ Prow()+1,0 Say "CTRL    CIO"

   Select CIO
   ordsetfocus('SINC002')
   Seek WANIMAL

   While ANIMAL == WANIMAL .And. ! EOF()

       @ Prow()+1,0 Say Strzero(SEQ,6)
       @ Prow(),08  Say DATA

       Skip

   Enddo

   @ Prow()+1,0 Say Replicate(Chr(196),54)

   Set Print To 
   Set Device To Screen
   Set Console On

   BOX_P := Savescreen(00,00,24,79)
   COR_P := Setcolor()

   Setcolor("W+/N")
   @ 00,00 Clear To 24,79

   Setcolor("N/W")
   @ 00,11 Clear To 24,68
   Setcolor("N/W+")

   For L=0 To 24
       @ L,11 Say Chr(09)
       @ L,68 Say Chr(09)
   Next L

   Setcolor("R/W")     
   @ 24,12 Clear To 24,67
   @ 24,13 Say "PgDw/PgUp/"+Chr(25)+Chr(24)
   @ 24,60 Say "ESC-FIM"

   Setcolor("N/W")
   edit_REL(WARQRPR,00,13,23,67,"N/W",.F.,.T.)

   Restscreen(00,00,24,79,BOX_P)

   Setcolor(COR_P)

   Selec ANIMAL

Elseif RPR = 2                                 // COBERTURA/INSEMINACAO

   Select ANIMAL

   WREC = RECNO()
   WORD = Indexord()

   If SEXO = "F"

      WANIMAL = CODIGO
      WARQRPR = Alltrim(Str(RANDOM()%99999999,8))
      WARQRPR = "RELAT\"+WARQRPR+".TXT"

      Set Print To &WARQRPR
      Set Device To Print
      Set Console Off

      cab_PAINEL("Historico: COBERTURA/INSEMINA"+Chr(128)+Chr(142)+"O")

      CAB = .T.
      IMP = .F.

      Select SERVICO
      ordsetfocus('SINC002')
      Seek WANIMAL

      While ANIMAL == WANIMAL .And. ! EOF()

          If TIPO <> "C"
             If TIPO <> "N"
                Skip
                Loop
             Endif
          Endif

          If CAB
             @ Prow()+1,0 Say "CTRL   Data       Touro (Cobertura/Normal)"                      
             CAB = .F.
             IMP = .T.
          Endif

          @ Prow()+1,0 Say Strzero(SEQ,6)
          @ Prow(),07  Say DATA

          WTOURO = TOURO
          WTIPO  = TIPO

          Select ANIMAL
          ordsetfocus('SINC001')
          Seek WTOURO
 
          If Found()
             @ Prow(),18 Say WTOURO+"-"+Alltrim(NOME)+" <"+WTIPO+">"
          Else
             @ Prow(),18 Say "-"
          Endif

          Select SERVICO  

          Skip

      Enddo

      If IMP
         @ Prow()+1,0 Say Replicate(Chr(196),54)
      Endif

      CAB = .T.
      IMP = .F.

      Select SERVICO
      ordsetfocus('SINC002')
      Seek WANIMAL

      While ANIMAL == WANIMAL .And. ! EOF()

          If TIPO <> "I"
             Skip
             Loop
          Endif

          If CAB
             @ Prow()+1,0 Say "                  INSEMINACAO      "                      
             @ Prow()+1,0 Say "CTRL   Data       Dose  Lote       "                      
             CAB = .F.
             IMP = .T.
          Endif

          @ Prow()+1,0 Say Strzero(SEQ,6)
          @ Prow(),07  Say DATA
          @ Prow(),19  Say DOSE Picture "99"
          @ Prow(),24  Say LOTE                 

          Skip

      Enddo

      If IMP
         @ Prow()+1,0 Say Replicate(Chr(196),54)
      Endif

      Set Print To 
      Set Device To Screen
      Set Console On

      BOX_P := Savescreen(00,00,24,79)
      COR_P := Setcolor()

      Setcolor("W+/N")
      @ 00,00 Clear To 24,79

      Setcolor("N/W")
      @ 00,11 Clear To 24,68
      Setcolor("N/W+")

      For L=0 To 24
          @ L,11 Say Chr(09)
          @ L,68 Say Chr(09)
      Next L

      Setcolor("R/W")     
      @ 24,12 Clear To 24,67
      @ 24,13 Say "PgDw/PgUp/"+Chr(25)+Chr(24)
      @ 24,60 Say "ESC-FIM"

      Setcolor("N/W")
      edit_REL(WARQRPR,00,13,23,67,"N/W",.F.,.T.)

      Restscreen(00,00,24,79,BOX_P)

      Setcolor(COR_P)

   Else

      WTOURO  = CODIGO
      WARQRPR = Alltrim(Str(RANDOM()%99999999,8))
      WARQRPR = "RELAT\"+WARQRPR+".TXT"

      Set Print To &WARQRPR
      Set Device To Print
      Set Console Off

      cab_PAINEL("Historico: COBERTURA")

      CAB = .T.

      Select SERVICO
      ordsetfocus('SINC003')
      Seek WTOURO  

      While TOURO == WTOURO .And. ! EOF()

          If CAB
             @ Prow()+1,0 Say "CTRL   Data       Animal"
             CAB = .F.
          Endif

          @ Prow()+1,0 Say Strzero(SEQ,6)
          @ Prow(),07  Say DATA

          WCOD = ANIMAL

          Select ANIMAL
          ordsetfocus('SINC001')
          Seek WCOD
 
          If Found()
             @ Prow(),18 Say WCOD+"-"+Alltrim(NOME)
          Else
             @ Prow(),18 Say "-"
          Endif

          Select SERVICO  

          Skip

      Enddo

      @ Prow()+1,0 Say Replicate(Chr(196),54)

      Set Print To 
      Set Device To Screen
      Set Console On

      BOX_P := Savescreen(00,00,24,79)
      COR_P := Setcolor()

      Setcolor("W+/N")
      @ 00,00 Clear To 24,79

      Setcolor("N/W")
      @ 00,11 Clear To 24,68
      Setcolor("N/W+")

      For L=0 To 24
          @ L,11 Say Chr(09)
          @ L,68 Say Chr(09)
      Next L

      Setcolor("R/W")     
      @ 24,12 Clear To 24,67
      @ 24,13 Say "PgDw/PgUp/"+Chr(25)+Chr(24)
      @ 24,60 Say "ESC-FIM"

      Setcolor("N/W")
      edit_REL(WARQRPR,00,13,23,67,"N/W",.F.,.T.)

      Restscreen(00,00,24,79,BOX_P)

      Setcolor(COR_P)

   Endif

   Selec ANIMAL
   ordsetfocus(WORD)
   Go WREC

Elseif RPR = 3                                 // PRENHEZ                 

   Select ANIMAL

   WREC = RECNO()
   WORD = Indexord()

   If SEXO = "F"

      WANIMAL = CODIGO
      WARQRPR = Alltrim(Str(RANDOM()%99999999,8))
      WARQRPR = "RELAT\"+WARQRPR+".TXT"

      Set Print To &WARQRPR
      Set Device To Print
      Set Console Off

      cab_PAINEL("Historico: PRENHEZ")
  
      @ Prow()+1,0 Say "CTRL   Data       Touro"

      Select EXAME
      ordsetfocus('SINC002')
      Seek WANIMAL

      While ANIMAL == WANIMAL .And. ! EOF()

          @ Prow()+1,0 Say Strzero(SEQ,6)
          @ Prow(),07  Say DATA

          WTOURO = TOURO

          Select ANIMAL
          ordsetfocus('SINC001')
          Seek WTOURO
 
          If Found()
             @ Prow(),18 Say WTOURO+"-"+Alltrim(NOME)
          Else
             @ Prow(),18 Say "-"
          Endif

          Select EXAME
          Skip

      Enddo

      @ Prow()+1,0 Say Replicate(Chr(196),54)

      Set Print To 
      Set Device To Screen
      Set Console On

      BOX_P := Savescreen(00,00,24,79)
      COR_P := Setcolor()

      Setcolor("W+/N")
      @ 00,00 Clear To 24,79

      Setcolor("N/W")
      @ 00,11 Clear To 24,68
      Setcolor("N/W+")

      For L=0 To 24
          @ L,11 Say Chr(09)
          @ L,68 Say Chr(09)
      Next L

      Setcolor("R/W")     
      @ 24,12 Clear To 24,67
      @ 24,13 Say "PgDw/PgUp/"+Chr(25)+Chr(24)
      @ 24,60 Say "ESC-FIM"

      Setcolor("N/W")
      edit_REL(WARQRPR,00,13,23,67,"N/W",.F.,.T.)

      Restscreen(00,00,24,79,BOX_P)

      Setcolor(COR_P)

   Else

      WTOURO  = CODIGO
      WARQRPR = Alltrim(Str(RANDOM()%99999999,8))
      WARQRPR = "RELAT\"+WARQRPR+".TXT"

      Set Print To &WARQRPR
      Set Device To Print
      Set Console Off

      cab_PAINEL("Historico: PRENHEZ")
  
      @ Prow()+1,0 Say "CTRL   Data       Animal"

      Select EXAME
      ordsetfocus('SINC003')
      Seek WTOURO

      While TOURO == WTOURO .And. ! EOF()

          @ Prow()+1,0 Say Strzero(SEQ,6)
          @ Prow(),07  Say DATA

          WCOD = ANIMAL

          Select ANIMAL
          ordsetfocus('SINC001')
          Seek WCOD
 
          If Found()
             @ Prow(),18 Say WCOD+"-"+Alltrim(NOME)
          Else
             @ Prow(),18 Say "-"
          Endif

          Select EXAME
          Skip

      Enddo

      @ Prow()+1,0 Say Replicate(Chr(196),54)

      Set Print To 
      Set Device To Screen
      Set Console On

      BOX_P := Savescreen(00,00,24,79)
      COR_P := Setcolor()

      Setcolor("W+/N")
      @ 00,00 Clear To 24,79

      Setcolor("N/W")
      @ 00,11 Clear To 24,68
      Setcolor("N/W+")

      For L=0 To 24
          @ L,11 Say Chr(09)
          @ L,68 Say Chr(09)
      Next L

      Setcolor("R/W")     
      @ 24,12 Clear To 24,67
      @ 24,13 Say "PgDw/PgUp/"+Chr(25)+Chr(24)
      @ 24,60 Say "ESC-FIM"

      Setcolor("N/W")
      edit_REL(WARQRPR,00,13,23,67,"N/W",.F.,.T.)

      Restscreen(00,00,24,79,BOX_P)

      Setcolor(COR_P)

   Endif

   Selec ANIMAL
   ordsetfocus(WORD)
   Go WREC

Elseif RPR = 4                                 // ABORTO                  

   Select ANIMAL

   WANIMAL = CODIGO
   WARQRPR = Alltrim(Str(RANDOM()%99999999,8))
   WARQRPR = "RELAT\"+WARQRPR+".TXT"

   Set Print To &WARQRPR
   Set Device To Print
   Set Console Off

   cab_PAINEL("Historico: ABORTO")
  
   @ Prow()+1,0 Say "CTRL   Data       Observa"+Chr(135)+Chr(132)+"o"

   Select ABORTO
   ordsetfocus('SINC002')
   Seek WANIMAL

   While ANIMAL == WANIMAL .And. ! EOF()

       @ Prow()+1,0 Say Strzero(SEQ,6)
       @ Prow(),07  Say DATA
       @ Prow(),18  Say Alltrim(OBS) 

       Skip

   Enddo

   @ Prow()+1,0 Say Replicate(Chr(196),54)

   Set Print To 
   Set Device To Screen
   Set Console On

   BOX_P := Savescreen(00,00,24,79)
   COR_P := Setcolor()

   Setcolor("W+/N")
   @ 00,00 Clear To 24,79

   Setcolor("N/W")
   @ 00,11 Clear To 24,68
   Setcolor("N/W+")

   For L=0 To 24
       @ L,11 Say Chr(09)
       @ L,68 Say Chr(09)
   Next L

   Setcolor("R/W")     
   @ 24,12 Clear To 24,67
   @ 24,13 Say "PgDw/PgUp/"+Chr(25)+Chr(24)
   @ 24,60 Say "ESC-FIM"

   Setcolor("N/W")
   edit_REL(WARQRPR,00,13,23,67,"N/W",.F.,.T.)

   Restscreen(00,00,24,79,BOX_P)

   Setcolor(COR_P)

   Selec ANIMAL

Elseif RPR = 5                                 // PARTO           

   Select ANIMAL

   WREC = RECNO()
   WORD = Indexord()

   If SEXO = "F"

      WMAE    = CODIGO
      WARQRPR = Alltrim(Str(RANDOM()%99999999,8))
      WARQRPR = "RELAT\"+WARQRPR+".TXT"

      Set Print To &WARQRPR
      Set Device To Print
      Set Console Off

      cab_PAINEL("Historico: PARTOS")
  
      @ Prow()+1,0 Say "CTRL   Data       Sexo  Bezerro/Pai"                    

      Select PARTO
      ordsetfocus('SINC002')
      Seek WMAE   

      While MAE == WMAE .And. ! EOF()

          @ Prow()+1,0 Say Strzero(SEQ,6)
          @ Prow(),07  Say DATA
          @ Prow(),18  Say If(SEXO = "F","Femea","Macho")
          @ Prow(),24  Say Alltrim(BEZERRO)

          WPAI = PAI

          Select ANIMAL
          ordsetfocus('SINC001')
          Seek WPAI
 
          If Found()
             @ Prow()+1,24 Say WPAI+"-"+Alltrim(NOME)
          Else
             @ Prow()+1,24 Say "-"
          Endif

          Select PARTO
          Skip

      Enddo

      @ Prow()+1,0 Say Replicate(Chr(196),54)

      Set Print To 
      Set Device To Screen
      Set Console On

      BOX_P := Savescreen(00,00,24,79)
      COR_P := Setcolor()

      Setcolor("W+/N")
      @ 00,00 Clear To 24,79

      Setcolor("N/W")
      @ 00,11 Clear To 24,68
      Setcolor("N/W+")

      For L=0 To 24
          @ L,11 Say Chr(09)
          @ L,68 Say Chr(09)
      Next L

      Setcolor("R/W")     
      @ 24,12 Clear To 24,67
      @ 24,13 Say "PgDw/PgUp/"+Chr(25)+Chr(24)
      @ 24,60 Say "ESC-FIM"

      Setcolor("N/W")
      edit_REL(WARQRPR,00,13,23,67,"N/W",.F.,.T.)

      Restscreen(00,00,24,79,BOX_P)

      Setcolor(COR_P)

   Else

      WPAI    = CODIGO
      WARQRPR = Alltrim(Str(RANDOM()%99999999,8))
      WARQRPR = "RELAT\"+WARQRPR+".TXT"

      Set Print To &WARQRPR
      Set Device To Print
      Set Console Off

      cab_PAINEL("Historico: PARTOS")
  
      @ Prow()+1,0 Say "CTRL   Data       Sexo  Bezerro/Mae"                    

      Select PARTO
      ordsetfocus('SINC003')
      Seek WPAI

      While PAI == WPAI .And. ! EOF()

          @ Prow()+1,0 Say Strzero(SEQ,6)
          @ Prow(),07  Say DATA
          @ Prow(),18  Say If(SEXO = "F","Femea","Macho")
          @ Prow(),24  Say Alltrim(BEZERRO)

          WMAE = MAE

          Select ANIMAL
          ordsetfocus('SINC001')
          Seek WMAE
 
          If Found()
             @ Prow()+1,24 Say WMAE+"-"+Alltrim(NOME)
          Else
             @ Prow()+1,24 Say "-"
          Endif

          Select PARTO
          Skip

      Enddo

      @ Prow()+1,0 Say Replicate(Chr(196),54)

      Set Print To 
      Set Device To Screen
      Set Console On

      BOX_P := Savescreen(00,00,24,79)
      COR_P := Setcolor()

      Setcolor("W+/N")
      @ 00,00 Clear To 24,79

      Setcolor("N/W")
      @ 00,11 Clear To 24,68
      Setcolor("N/W+")

      For L=0 To 24
          @ L,11 Say Chr(09)
          @ L,68 Say Chr(09)
      Next L

      Setcolor("R/W")     
      @ 24,12 Clear To 24,67
      @ 24,13 Say "PgDw/PgUp/"+Chr(25)+Chr(24)
      @ 24,60 Say "ESC-FIM"

      Setcolor("N/W")
      edit_REL(WARQRPR,00,13,23,67,"N/W",.F.,.T.)

      Restscreen(00,00,24,79,BOX_P)

      Setcolor(COR_P)

   Endif

   Selec ANIMAL
   ordsetfocus(WORD)
   Go WREC

Elseif RPR = 6                                 // SINTOMAS/TRATAMENTO

   Select ANIMAL

   WREC    = RECNO()
   WORD    = Indexord()
   WANIMAL = CODIGO
   WARQRPR = Alltrim(Str(RANDOM()%99999999,8))
   WARQRPR = "RELAT\"+WARQRPR+".TXT"

   Set Print To &WARQRPR
   Set Device To Print
   Set Console Off

   cab_PAINEL("Historico: SINTOMAS/TRATAMENTOS")

   Select SINTOMA
   ordsetfocus('SINC002')
   Seek WANIMAL

   While ANIMAL == WANIMAL .And. ! EOF()

       @ Prow()+1,0 Say "                        CTRL: "+Strzero(SEQ,6)+"  Data: "+Dtoc(DATA)
       @ Prow()+1,0 Say "S i n t o m a"

       WCOD  = SINTOMA 
       WDESC = ""

       Select CSINTOMA
       ordsetfocus('SINC001')
       Seek WCOD
 
       If Found()
          WDESC = WCOD+"-"+SINTOMA
       Else
          WDESC = "Sintoma nao cadastrado"
       Endif

       Select SINTOMA

       @ Prow()+1,0 Say WDESC

       If ! Empty(COMPLEM)
          @ Prow()+1,0 Say Left(COMPLEM,54)
       Endif

       @ Prow()+2,0 Say "T r a t a m e n t o"
       @ Prow()+1,0 Say Left(TRATA1,54)

       If ! Empty(TRATA2)
          @ Prow()+1,0 Say Left(TRATA3,54)
       Endif

       If ! Empty(TRATA3)
          @ Prow()+1,0 Say Left(TRATA3,54)
       Endif
   
       @ Prow()+1,0 Say "______________________________________________________"

       Skip

   Enddo

   Set Print To 
   Set Device To Screen
   Set Console On

   BOX_P := Savescreen(00,00,24,79)
   COR_P := Setcolor()

   Setcolor("W+/N")
   @ 00,00 Clear To 24,79

   Setcolor("N/W")
   @ 00,11 Clear To 24,68
   Setcolor("N/W+")

   For L=0 To 24
       @ L,11 Say Chr(09)
       @ L,68 Say Chr(09)
   Next L

   Setcolor("R/W")     
   @ 24,12 Clear To 24,67
   @ 24,13 Say "PgDw/PgUp/"+Chr(25)+Chr(24)
   @ 24,60 Say "ESC-FIM"

   Setcolor("N/W")
   edit_REL(WARQRPR,00,13,23,67,"N/W",.F.,.T.)

   Restscreen(00,00,24,79,BOX_P)

   Setcolor(COR_P)

   Selec ANIMAL
   ordsetfocus(WORD)
   Go WREC

Elseif RPR = 7                                 // PESO/COND.CORPOREA  

   Select ANIMAL

   WREC    = RECNO()
   WORD    = Indexord()
   WANIMAL = CODIGO
   WARQRPR = Alltrim(Str(RANDOM()%99999999,8))
   WARQRPR = "RELAT\"+WARQRPR+".TXT"

   Set Print To &WARQRPR
   Set Device To Print
   Set Console Off

   cab_PAINEL("Historico: PESO/COND.CORPOREA")
   @ Prow()+1,0 Say "CTRL   Data           Peso   Ganho  Cond.Corporea"

   WPESO_ANT = 0

   Select PESO
   ordsetfocus('SINC003') 
   Seek WANIMAL

   While ANIMAL == WANIMAL .And. ! EOF()

       @ Prow()+1,0 Say Strzero(SEQ,6)
       @ Prow(),07  Say DATA                
       @ Prow(),18  Say PESO   Picture "@R@E 999.99Kg"

       WGANHO = 100 - ((WPESO_ANT / PESO) * 100)

       If WPESO_ANT = 0
          WGANHO = 0
       Endif

       @ Prow(),27  Say WGANHO Picture "@R@E 999.99%"
       @ Prow(),36  Say Left(CONDICAO,18)

       WPESO_ANT = PESO

       Skip

   Enddo

   Set Print To 
   Set Device To Screen
   Set Console On

   BOX_P := Savescreen(00,00,24,79)
   COR_P := Setcolor()

   Setcolor("W+/N")
   @ 00,00 Clear To 24,79

   Setcolor("N/W")
   @ 00,11 Clear To 24,68
   Setcolor("N/W+")

   For L=0 To 24
       @ L,11 Say Chr(09)
       @ L,68 Say Chr(09)
   Next L

   Setcolor("R/W")     
   @ 24,12 Clear To 24,67
   @ 24,13 Say "PgDw/PgUp/"+Chr(25)+Chr(24)
   @ 24,60 Say "ESC-FIM"

   Setcolor("N/W")
   edit_REL(WARQRPR,00,13,23,67,"N/W",.F.,.T.)

   Restscreen(00,00,24,79,BOX_P)

   Setcolor(COR_P)

   Selec ANIMAL
   ordsetfocus(WORD)
   Go WREC

Elseif RPR = 8                                 // VACINACOES          

   Select ANIMAL

   WREC    = RECNO()
   WORD    = Indexord()
   WANIMAL = CODIGO
   WARQRPR = Alltrim(Str(RANDOM()%99999999,8))
   WARQRPR = "RELAT\"+WARQRPR+".TXT"

   Set Print To &WARQRPR
   Set Device To Print
   Set Console Off

   cab_PAINEL("Historico: VACINACOES")

   @ Prow()+1,0 Say "CTRL   Data       Vacina                         Freq."

   Select VACINA
   ordsetfocus('SINC002') 
   Seek WANIMAL

   While ANIMAL == WANIMAL .And. ! EOF()

       @ Prow()+1,0 Say Strzero(SEQ,6)
       @ Prow(),07  Say DATA

       WCOD  = VACINA
       WDESC = ""
       WFREQ = 0

       Select CVACINA
       ordsetfocus('SINC001')
       Seek WCOD
 
       If Found()
          WDESC = VACINA     
          WFREQ = FREQUENCIA
       Else
          WDESC = "Vacina nao cadastrada"
          WFREQ = 0
       Endif

       Select VACINA

       @ Prow(),18 Say WCOD+"-"+WDESC
       @ Prow(),48 Say WFREQ Picture "@R@E 9999dd"

       Skip

   Enddo

   Set Print To 
   Set Device To Screen
   Set Console On

   BOX_P := Savescreen(00,00,24,79)
   COR_P := Setcolor()

   Setcolor("W+/N")
   @ 00,00 Clear To 24,79

   Setcolor("N/W")
   @ 00,11 Clear To 24,68
   Setcolor("N/W+")

   For L=0 To 24
       @ L,11 Say Chr(09)
       @ L,68 Say Chr(09)
   Next L

   Setcolor("R/W")     
   @ 24,12 Clear To 24,67
   @ 24,13 Say "PgDw/PgUp/"+Chr(25)+Chr(24)
   @ 24,60 Say "ESC-FIM"

   Setcolor("N/W")
   edit_REL(WARQRPR,00,13,23,67,"N/W",.F.,.T.)

   Restscreen(00,00,24,79,BOX_P)

   Setcolor(COR_P)

   Selec ANIMAL
   ordsetfocus(WORD)
   Go WREC

Endif

Return .T.


***************************
Function cab_PAINEL(TITULO)
***************************

@ Prow()+1,0 Say ""
CENTRO_PRN(54,TITULO)

@ Prow()+1,0 Say Replicate(Chr(196),54)

@ Prow()+1,0 Say "C"+Chr(162)+"digo..: "+CODIGO                       
@ Prow()+1,0 Say "Animal..: "+NOME 
@ Prow()+1,0 Say "Ra"+Chr(135)+"a....: "+RACA         
@ Prow()+1,0 Say "Pelagem.: "+COR          
@ Prow()+1,0 Say "Pasto...: "+PASTO        
@ Prow()+1,0 Say "D.N.....: "+Dtoc(DTNASC)      
@ Prow()+1,0 Say "Registro: "+NREG
@ Prow()+1,0 Say "Mae.....: "+MAE
@ Prow()+1,0 Say "Ra"+Chr(135)+"a....: "+MRACA        
@ Prow()+1,0 Say "Pai.....: "+PAI
@ Prow()+1,0 Say "Ra"+Chr(135)+"a....: "+PRACA        
@ Prow()+1,0 Say Replicate(Chr(196),54)

Return .T.


*******************
Function exc_ANIMAL
*******************
Save Screen

while .t. 

   select ANIMAL

   sc_ANIMAL()

   WCODIGO = space(06)

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "EXCLUS"+chr(142)+"O"
   setcolor(COR_TEXT)

   @ 02,13 get WCODIGO picture "999999"   
   READ_F()

   if empty(WCODIGO) .or. lastkey() == 27
      return .t.
   endif

   WCODIGO = Strzero(Val(WCODIGO),6)

   select ANIMAL
   ordsetfocus('SINC001')
   seek WCODIGO

   if ! found()
      centro(24,"C"+Chr(162)+"digo de animal n"+chr(132)+"o cadastrado!")
      inkey(1)
      @ 24,10 clear to 24,79
      loop
   endif

   setcolor(COR_TEXT)

   @ 02,13 Say CODIGO                       
   @ 02,55 Say CATEGORIA    picture "@!"       
   @ 03,13 Say NOME         picture "@S29"
   @ 04,13 Say RACA         
   @ 05,13 Say COR          
   @ 06,13 Say SEXO         picture "!"        
   @ 07,13 Say DTNASC      

   @ 08,13 Say MAE          Picture "@S29" 
   @ 09,13 Say MRACA        
   @ 08,55 Say PAI          Picture "@S24"        
   @ 09,55 Say PRACA        

   @ 10,13 Say AVO_1        Picture "@S29" 
   @ 11,13 Say RACA_AVO1    
   @ 10,55 Say AVO_2        Picture "@S24" 
   @ 11,55 Say RACA_AVO2    

   @ 03,55 Say NREG
   @ 05,55 Say PASTO        Picture "@!"       

   @ 18,13 Say GRUPO       
   @ 19,13 Say LOTE

   @ 18,55 Say ENTRADA     
   @ 19,55 Say SAIDA       

   @ 20,13 Say PEND         Picture "999"

   @ 21,13 Say OBS1        
   @ 22,13 Say OBS2        

   WCODIGO    = CODIGO
   WCATEGORIA = CATEGORIA
   WSEXO      = SEXO
   WPEND      = PEND
   WGRUPO     = GRUPO
   WLOTE      = LOTE

   mostra_UP(WCODIGO)

   Select CATEG
   ordsetfocus('SINC001')
   Seek Upper(WCATEGORIA)

   If Found()
      @ 02,59 Say CATEGORIA
   Else
      @ 02,59 Say Space(20)
   Endif

   If WSEXO = "M"
      @ 06,14 Say "-Macho"
   Elseif WSEXO = "F"
      @ 06,14 Say "-Femea"
   Endif

   Select GRUPO
   ordsetfocus('SINC001')
   Seek WGRUPO

   If Found()
      @ 18,17 Say Left(DESCRICAO,25) 
   Else
      @ 18,17 Say Space(25)
   Endif

   Select LOTE
   ordsetfocus('SINC001')
   Seek WLOTE

   If Found()
      @ 19,17 Say Left(DESCRICAO,25) 
   Else
      @ 19,17 Say Space(25)
   Endif

   Select PENDENC
   ordsetfocus('SINC001')
   Seek WPEND             

   If Found()
      @ 22,17 Say DESCRICAO 
   Else
      @ 22,17 Say Space(30)
   Endif

   select ANIMAL

   if confirma(24,"Confirma a EXCLUS"+chr(142)+"O...(S/N)?","N") == "S"

      if ! reglock()
         loop
      endif                         

      replace CODIGO      with space(06)      ,;
              CATEGORIA   with space(03)      ,;                    
              NOME        with space(20)      ,;                    
              DTNASC      with ctod(space(08)),;              
              RACA        with space(16)      ,;                    
              COR         with space(10)      ,;                    
              SEXO        with space(01)      ,;                    
              NREG        with space(20)      ,;                    
              PASTO       with space(10)      ,;
              MAE         with space(30)      ,;
              MRACA       with space(16)      ,;
              PAI         with space(30)      ,;
              PRACA       with space(16)      ,;
              AVO_1       with space(30)      ,;
              RACA_AVO1   with space(16)      ,;
              AVO_2       with space(30)      ,;
              RACA_AVO2   with space(16)      ,;
              GRUPO       with space(03)      ,;                    
              LOTE        with space(03)      ,;                    
              ENTRADA     with ctod(space(08)),;              
              SAIDA       with ctod(space(08)),;              
              PEND        with 0              ,;                    
              OBS1        with space(60)      ,;                    
              OBS2        with space(60)      ,;                    
              USUARIO     with space(20)      ,;
              DATE        with ctod(space(08)),;
              TIME        with space(08)

      Delete

      GRAVA()

   Endif

Enddo

Return .T.


********************
Function save_ANIMAL
********************

Select ANIMAL

if ! reglock()
   return .f.
endif
   
replace CODIGO      with WCODIGO    ,;      
        CATEGORIA   with WCATEGORIA ,;   
        NOME        with WNOME      ,;        
        DTNASC      with WDTNASC    ,;      
        RACA        with WRACA      ,;        
        COR         with WCOR       ,;         
        SEXO        with WSEXO      ,;        
        NREG        with WNREG      ,;   
        PASTO       with WPASTO     ,;
        MAE         with WMAE       ,;  
        MRACA       with WMRACA     ,;
        PAI         with WPAI       ,;  
        PRACA       with WPRACA     ,;
        AVO_1       with WAVO_1     ,;  
        RACA_AVO1   with WRACA_AVO1 ,;
        AVO_2       with WAVO_2     ,;  
        RACA_AVO2   with WRACA_AVO2 ,;
        GRUPO       with WGRUPO     ,;       
        LOTE        with WLOTE      ,;        
        ENTRADA     with WENTRADA   ,;     
        SAIDA       with WSAIDA     ,;       
        PEND        with WPEND      ,;        
        OBS1        with WOBS1      ,;        
        OBS2        with WOBS2      ,;        
        USUARIO     with WPUSUARIO  ,;
        TIME        with TIME()     ,;
        DATE        with DATE()

GRAVA()

Return .T.


*******************
Function REPRODUCAO
*******************
OPC2 = 1

while .t.

   setcolor(COR_CAB)   
   @ 03,42 say "DADOS REPRODUTIVOS                 " 
   setcolor(COR_TEXT)

   private OPCAO[7]
   OPCAO[1] = " Cio                      " 
   OPCAO[2] = " Cobertura/Insemina"+Chr(135)+Chr(132)+"o/TE "
   OPCAO[3] = " Prenhez                  " 
   OPCAO[4] = " Abortos                  " 
   OPCAO[5] = " Partos/Desmame           " 
   OPCAO[6] = " Banco de S"+Chr(136)+"men           " 
   OPCAO[7] = " Esc - Fim                " 

   private ACESSO[7]
   ACESSO[1] = .T.
   ACESSO[2] = .T.
   ACESSO[3] = .T.
   ACESSO[4] = .T.
   ACESSO[5] = .T.
   ACESSO[6] = .T.
   ACESSO[7] = .T.

   private LETRA[7]
   LETRA[1] = "C"  
   LETRA[2] = "O"  
   LETRA[3] = "Z"  
   LETRA[4] = "A"  
   LETRA[5] = "P"  
   LETRA[6] = "B"  
   LETRA[7] = "F"  

   private POS[7]
   POS[1] = 1
   POS[2] = 2 
   POS[3] = 7 
   POS[4] = 1 
   POS[5] = 1
   POS[6] = 1
   POS[7] = 7
 
   private OBS_MENU[7]
   OBS_MENU[1] = "Cadastro de animais no cio!"        
   OBS_MENU[2] = "Cadastro de cobertura/insemina"+Chr(135)+Chr(132)+"o/TE!"
   OBS_MENU[3] = "Cadastro de prenhez!"
   OBS_MENU[4] = "Cadastro de abortos!" 
   OBS_MENU[5] = "Cadastro de partos/desmame!"
   OBS_MENU[6] = "Cadastro de banco de semem!"                  
   OBS_MENU[7] = "Retornar ao menu principal!"                  
 
   OPC2 = achoise(OPC2,08,25,16,52,1,OPCAO,ACESSO,LETRA,POS,OBS_MENU,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)

   if OPC2 == 0 .or. OPC2 == 7
      return .t.
   elseif OPC2 == 1
      CIO()      
   elseif OPC2 == 2
      COB_IA()       
   elseif OPC2 == 3
      PRENHEZ()
   elseif OPC2 == 4
      ABORTO()
   elseif OPC2 == 5
      PARTOS()
   elseif OPC2 == 6
      BANCO()
   endif

Enddo

Return .T.


************
Function CIO
************
OPC3 = 1

while .t.

   setcolor(COR_CAB) 
   @ 03,42 say "CIO                                " 
   setcolor(COR_TEXT)

   private OPCAO[5]
   OPCAO[1] = " Inclus"+chr(132)+"o   " 
   OPCAO[2] = " Altera"+chr(135)+chr(132)+"o  " 
   OPCAO[3] = " Consulta   " 
   OPCAO[4] = " Exclus"+chr(132)+"o   " 
   OPCAO[5] = " Esc - Fim  " 

   private ACESSO[5]
   ACESSO[1] = .t.
   ACESSO[2] = .t.
   ACESSO[3] = .t.
   ACESSO[4] = .t.
   ACESSO[5] = .t.

   private LETRA[5]
   LETRA[1] = "I"  
   LETRA[2] = "A"
   LETRA[3] = "C"
   LETRA[4] = "E"
   LETRA[5] = "F"

   private POS[5] 
   POS[1] = 1
   POS[2] = 1
   POS[3] = 1
   POS[4] = 1
   POS[5] = 7

   private OBS_MENU[5]  
   OBS_MENU[1] = "Inclus"+chr(132)+"o de animais no cio!" 
   OBS_MENU[2] = "Altera"+chr(135)+chr(132)+"o de animais no cio!"       
   OBS_MENU[3] = "Consulta de animais no cio!"                    
   OBS_MENU[4] = "Exclus"+chr(132)+"o animais no cio!"  
   OBS_MENU[5] = "Retorno ao menu principal!"            

   OPC3 = achoise(OPC3,10,32,16,45,2,OPCAO,ACESSO,LETRA,POS,OBS_MENU,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)
   
   if OPC3 == 0 .or. OPC3 == 5
      return .t.
   elseif OPC3 == 1
      inc_cio()
   elseif OPC3 == 2
      alt_cio()
   elseif OPC3 == 3
      con_cio()
   elseif OPC3 == 4
      exc_cio()
   endif

enddo

return .t.


***************
function SC_CIO
***************

@ 05,00 clear to 22,79
@ 24,00 clear to 24,79

@ 08,01 say "Controle.:"
@ 10,01 say "Animal...:        - " 
@ 12,01 say "Data.....:"

return .t.


****************
function INC_CIO
****************

while .t. 

    WANIMAL = space(06)
    WDATA   = date()   

    sc_cio()

    setcolor(COR_CONS)
    @ 24,00 say "INCLUS"+chr(142)+"O"
    @ 24,73 say "ESC-FIM"
    setcolor(COR_TEXT)

    select CIO
    ordsetfocus('SINC001')
    go bottom

    WSEQ = SEQ + 1

    @ 08,12 say Strzero(WSEQ,6)

    While .T.

        @ 10,12 get WANIMAL picture "@!" valid busca_animal("CIO") 
        @ 12,12 get WDATA                valid ! empty(WDATA)

        select ANIMAL
        ordsetfocus('SINC001')
        seek WANIMAL
 
        if found()
           @ 10,21 say NOME
        else
           @ 10,21 say space(30)
        endif
  
        select CIO

        read_f()

        if lastkey() == 27
           return .t.
        endif

        if confirma(24,"Dados Corretos...(S/N)?","S") == "S"

           select CIO
           ordsetfocus('SINC001')
           go bottom

           WSEQ = SEQ + 1

           set deleted off
           go top
           if deleted()
              if ! reglock()
                 loop
              endif

              recall
           else
              if ! inc_reg()
                 loop
              endif
           endif
           set deleted on

           save_CIO()

           setcolor(COR_TEXT+"*+")
           @ 08,12 say Strzero(WSEQ,6)
           setcolor(COR_TEXT)

           inkey(10)

           Exit

        endif

    Enddo

Enddo

Return .T.


****************
function ALT_CIO
****************

while .t. 

   sc_cio()

   WSEQ = 0

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "ALTERA"+chr(128)+chr(142)+"O"
   setcolor(COR_TEXT)

   @ 08,12 get WSEQ picture "@Z 999999" 
   read_f()

   @ 24,10 clear to 24,79

   if empty(WSEQ) .or. lastkey() == 27
      return .t.
   endif

   select CIO
   ordsetfocus('SINC001')
   seek WSEQ

   if ! found()
      @ 24,10 clear to 24,79
      centro(24,"N"+Chr(163)+"mero de controle n"+Chr(132)+"o cadastrado...")
      inkey(5)
      loop
   endif

   WANIMAL = ANIMAL      
   WDATA   = DATA        

   while .t.

      sc_cio()

      setcolor(COR_CONS)
      @ 24,00 say "ALTERA"+chr(138)+chr(142)+"O"
      @ 24,73 say "ESC-FIM"
      setcolor(COR_TEXT)

      @ 08,12 say Strzero(WSEQ,6)
      @ 10,12 get WANIMAL  picture "@!" valid busca_animal() 
      @ 12,12 get WDATA                 valid ! empty(WDATA)

      select ANIMAL
      ordsetfocus('SINC001')
      seek WANIMAL
      
      if found()
         @ 10,21 say NOME
      else
         @ 10,21 say space(30)
      endif
      
      select CIO

      READ_F()

      if lastkey() == 27
         return .t.
      endif

      if confirma(24,"Dados Corretos...(S/N)?","S") == "S"
         select CIO
         save_CIO()
         exit
      endif

   enddo

enddo

return .t.


****************
Function CON_CIO
****************

while .t.

   @ 05,00 clear to 22,79
   @ 24,00 clear to 24,79

   setcolor(COR_CONS)
   @ 24,00 say "CONSULTA"
   @ 24,73 say "ESC-FIM"
   setcolor(COR_TEXT)

   OPC4 = 5

   SOMBRA(10,14,18,67,8)

   setcolor(COR_CONS)
   @ 09,12 clear to 17,66
   @ 09,12 to 17,66 

   @ 10,14 say "Consulta a partir do in"+chr(161)+"cio do arquivo..........[1]"
   @ 11,14 say "Consulta a partir do final do arquivo...........[2]"
   @ 12,14 say "Consulta a partir do n"+Chr(163)+"mero de controle.........[3]"
   @ 13,14 say "Consulta a partir do animal.....................[4]"
   @ 14,14 say "Retorno ao menu principal.......................[5]"

   setcolor(COR_TEXT)
   @ 16,14 say "Escolha sua op"+chr(135)+chr(132)+"o de consulta...................[ ]"
   @ 16,63 get OPC4 picture "9" range 1,5
   read_f()

   if OPC4 == 5
      return .t.
   endif

   while .t.

      sc_cio()

      if OPC4 == 1
         select CIO
         ordsetfocus('SINC001')
         go top

      elseif OPC4 == 2
         select CIO
         ordsetfocus('SINC001')
         go bottom

      elseif OPC4 == 3

         WSEQ  = 0          

         @ 08,12 get WSEQ picture "@Z 999999" 
         read_f()

         if lastkey() == 27
            exit
         endif

         select CIO
         ordsetfocus('SINC001')
         seek WSEQ  

         if ! found()
            centro(24,"N"+Chr(163)+"mero de controle n"+Chr(132)+"o cadastrado...")
            inkey(5)
            loop
         endif

      elseif OPC4 == 4

         WANIMAL = space(06)          
         WDATA   = ctod(space(08))

         @ 10,12 get WANIMAL picture "@!" valid busca_animal("CIO") 
         read_f()

         if lastkey() == 27
            exit
         endif

         select CIO
         ordsetfocus('SINC002')
         seek WANIMAL

         if ! found()
            centro(24,"N"+Chr(132)+"o houve nenhum lan"+Chr(135)+"amento para esse animal...")
            inkey(5)
            loop
         endif

      endif

      centro(24,"PgDw-Pr"+chr(162)+"ximo   PgUp-Anterior")

      KEY = 0

      while KEY  <> 27

         @ 08,12 say Strzero(SEQ,6)
         @ 10,12 say ANIMAL         
         @ 12,12 say DATA                               

         WANIMAL = ANIMAL

         select ANIMAL
         ordsetfocus('SINC001')
         seek WANIMAL
         
         if found()
            @ 10,21 say NOME
         else
            @ 10,21 say space(30)
         endif
         
         select CIO

         KEY = inkey(0)

         view_USER()

         if KEY == -7
            scalc()
         elseif KEY == -6
            calendario()
         elseif KEY == 3
            skip 
            if eof()
               skip -1
            endif
         elseif KEY == 18
            skip -1
         elseif KEY == 27
            @ 24,10 clear to 24,70
            exit
         endif

      enddo

      if OPC4 == 1 .or. OPC4 == 2 
         exit
      endif

   enddo

enddo

return .t.


****************
function EXC_CIO
****************

while .t. 

   sc_cio()

   WSEQ = 0

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "EXCLUS"+chr(142)+"O"
   setcolor(COR_TEXT)

   @ 08,12 get WSEQ picture "999999"
   read_f()

   @ 24,10 clear to 24,79

   if empty(WSEQ) .or. lastkey() == 27
      return .t.
   endif

   select CIO
   ordsetfocus('SINC001')
   seek WSEQ

   if ! found()
      centro(24,"N"+Chr(163)+"mero de controle n"+chr(132)+"o cadastrado!")
      inkey(5)
      loop
   endif

   @ 08,12 say Strzero(SEQ,6)
   @ 10,12 say ANIMAL         
   @ 12,12 say DATA                               

   WANIMAL = ANIMAL

   select ANIMAL
   ordsetfocus('SINC001')
   seek WANIMAL
   
   if found()
      @ 10,21 say NOME
   else
      @ 10,21 say space(30)
   endif
   
   select CIO

   if confirma(24,"Confirma EXCLUS"+chr(142)+"O...(S/N)?","N") == "S"

      if ! reglock()
         loop
      endif                         

      replace SEQ     with 0              ,;        
              ANIMAL  with space(06)      ,;        
              DATA    with ctod(space(08)),;   
              USUARIO with space(20)      ,;
              DATE    with ctod(space(08)),;
              TIME    with space(08)

      delete

      grava()

   endif

enddo

return .t.


*****************
Function save_CIO
*****************

Select CIO

if ! reglock()
   return .f.
endif
   
replace SEQ     with WSEQ     ,;    
        ANIMAL  with WANIMAL  ,; 
        DATA    with WDATA    ,;
        USUARIO with WPUSUARIO,;
        TIME    with TIME()   ,;
        DATE    with DATE()

GRAVA()

Return .T.


***************
Function COB_IA
***************
OPC3 = 1

while .t.

   setcolor(COR_CAB) 
   @ 03,42 say "COBERTURA/INSEMINA"+Chr(128)+Chr(142)+"O/TE          " 
   setcolor(COR_TEXT)

   private OPCAO[5]
   OPCAO[1] = " Inclus"+chr(132)+"o   " 
   OPCAO[2] = " Altera"+chr(135)+chr(132)+"o  " 
   OPCAO[3] = " Consulta   " 
   OPCAO[4] = " Exclus"+chr(132)+"o   " 
   OPCAO[5] = " Esc - Fim  " 

   private ACESSO[5]
   ACESSO[1] = .t.
   ACESSO[2] = .t.
   ACESSO[3] = .t.
   ACESSO[4] = .t.
   ACESSO[5] = .t.

   private LETRA[5]
   LETRA[1] = "I"  
   LETRA[2] = "A"
   LETRA[3] = "C"
   LETRA[4] = "E"
   LETRA[5] = "F"

   private POS[5] 
   POS[1] = 1
   POS[2] = 1
   POS[3] = 1
   POS[4] = 1
   POS[5] = 7

   private OBS_MENU[5]  
   OBS_MENU[1] = "Inclus"+chr(132)+"o de cobertura/insemina"+Chr(132)+Chr(135)+"o/TE!"
   OBS_MENU[2] = "Altera"+chr(135)+chr(132)+"o de cobertura/insemina"+Chr(135)+Chr(132)+"o!"
   OBS_MENU[3] = "Consulta as coberturas/insemina"+Chr(135)+Chr(148)+"es!"
   OBS_MENU[4] = "Exclus"+chr(132)+"o de cobertura/insemina"+Chr(135)+Chr(132)+"o!"
   OBS_MENU[5] = "Retorno ao menu principal!"                                                                                            

   OPC3 = achoise(OPC3,10,32,16,45,2,OPCAO,ACESSO,LETRA,POS,OBS_MENU,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)
   
   if OPC3 == 0 .or. OPC3 == 5
      return .t.
   elseif OPC3 == 1
      inc_COB_IA()   
   elseif OPC3 == 2
      alt_COB_IA()    
   elseif OPC3 == 3
      con_COB_IA()
   elseif OPC3 == 4
      exc_COB_IA()   
   endif

enddo

return .t.


******************
Function sc_COB_IA 
******************

@ 05,00 Clear To 22,79
@ 24,00 Clear To 24,79

@ 06,01 Say "Controle...:"    
@ 06,55 Say "Data.......:"
@ 08,01 Say "Animal.....:        -"
@ 08,55 Say "Tipo (CIT).:"

@ 09,00 To 12,78
@ 10,01 Say "Touro......:        -"
@ 10,55 Say "Comunica"+Chr(135)+Chr(132)+"o:"
@ 11,55 Say "N"+Chr(163)+"mero.....:"

@ 13,00 To 16,78
@ 14,01 Say "Lote.......:"
@ 15,01 Say "Dose.......:"
@ 15,55 Say "M"+Chr(130)+"todo.....:"

@ 17,00 To 21,78
@ 18,01 Say "Touro......:"
@ 19,01 Say "Mae........:"
@ 20,01 Say "Receptora..:"

@ 22,01 Say "Observa"+Chr(135)+Chr(132)+"o.:"

Setcolor("BG/B")
@ 09,01 Say "C-Cobertura Natural "
@ 13,01 Say "I-Insemina"+Chr(135)+Chr(132)+"o artificial"
@ 17,01 Say "T-Transplante de embri"+Chr(132)+"o"
@ 08,60 Say "(CIT)"

Setcolor(COR_TEXT)

Return .T.


*******************
Function inc_COB_IA 
*******************

while .t. 

    sc_COB_IA()

    setcolor(COR_CONS)
    @ 24,00 say "INCLUS"+chr(142)+"O"
    @ 24,73 say "ESC-FIM"
    setcolor(COR_TEXT)

    WANIMAL   = Space(06) 
    WDATA     = DATE()   
    WTIPO     = Space(01)
    WTOURO    = Space(06)
    WCOMUNICA = Ctod(Space(08))
    WNUMERO   = Space(15)          
    WLOTE     = Space(06)
    WDOSE     = 0
    WMETODO   = Space(03)
    WTOURO_TE = Space(30)
    WMAE_TE   = Space(30)
    WRECEP_TE = Space(30)
    WOBS      = Space(40)

    Select SERVICO
    ordsetfocus('SINC001')
    Go Bottom

    WSEQ = SEQ + 1

    @ 06,14 say Strzero(WSEQ,6)

    while .t. 

        setcolor(COR_TEXT)

        @ 06,68 Get WDATA                                                       Valid ! empty(WDATA)
        @ 08,14 Get WANIMAL   Picture "@!"                                      Valid busca_ANIMAL("COB_IA") 
        @ 08,68 Get WTIPO     Picture "!"                                       Valid tipo_COB_IA()

        @ 10,14 Get WTOURO    Picture "@!"    When WTIPO = "C" .Or. WTIPO = "N" Valid busca_TOURO()
        @ 10,68 Get WCOMUNICA                 When WTIPO = "C"
        @ 11,68 Get WNUMERO   Picture "@S10"  when WTIPO = "C"

        @ 14,14 Get WLOTE     Picture "@!"    When WTIPO = "I"                  Valid busca_BANCO()
        @ 15,14 Get WDOSE     Picture "@Z 99" When WTIPO = "I"                      
        @ 15,68 Get WMETODO   Picture "@!"    When WTIPO = "I"                      

        @ 18,14 Get WTOURO_TE                 When WTIPO = "T" 
        @ 19,14 Get WMAE_TE                   When WTIPO = "T"                      
        @ 20,14 Get WRECEP_TE                 When WTIPO = "T"                      

        @ 22,14 Get WOBS

        select ANIMAL
        ordsetfocus('SINC001')
        seek WANIMAL
 
        if found()
           @ 08,23 say NOME
        else
           @ 08,23 say space(30)
        endif

        If WTIPO = "C" 

           Seek WTOURO
 
           If Found()
              @ 10,23 Say NOME
           Else
              @ 10,23 Say Space(30)
           Endif
  
           @ 14,14 Say Space(06)
           @ 15,14 Say Space(02)
           @ 15,68 Say Space(03)
  
           @ 18,14 Say Space(30)
           @ 19,14 Say Space(30)
           @ 20,14 Say Space(30)

           WLOTE     = Space(06)
           WDOSE     = 0
           WMETODO   = Space(03)
           WTOURO_TE = Space(30)
           WMAE_TE   = Space(30)
           WRECEP_TE = Space(30)

        Elseif WTIPO = "I" 

           @ 10,14 Say Space(06)
           @ 10,23 Say Space(30)
           @ 10,68 Say Space(08)
           @ 11,68 Say Space(10)

           WTOURO    = Space(06)
           WCOMUNICA = Ctod(Space(08))
           WNUMERO   = Space(15)
           WTOURO_TE = Space(30)
           WMAE_TE   = Space(30)
           WRECEP_TE = Space(30)
  
           @ 18,14 Say Space(30)
           @ 19,14 Say Space(30)
           @ 20,14 Say Space(30)

        Elseif WTIPO = "T" 

           @ 10,14 Say Space(06)
           @ 10,23 Say Space(30)
           @ 10,68 Say Space(08)
           @ 11,68 Say Space(10)

           @ 14,14 Say Space(06)
           @ 15,14 Say Space(02)
           @ 15,68 Say Space(03)

           WTOURO    = Space(06)
           WCOMUNICA = Ctod(Space(08))
           WNUMERO   = Space(15)
           WLOTE     = Space(06)
           WDOSE     = 0
           WMETODO   = Space(03)

        Endif

        If WTIPO = "C" 
           @ 08,68 Say "Cobertura  "
        Elseif WTIPO = "I"
           @ 08,68 Say "Insemina"+Chr(135)+Chr(132)+"o"
        Elseif WTIPO = "T"
           @ 08,68 Say "Transplante"
        Else
           @ 08,68 Say "           "
        Endif

        select SERVICO

        READ_F()

        if lastkey() == 27
           return .t.
        endif

        if confirma(24,"Dados corretos...(S/N)?","S") == "S"

           select SERVICO
           ordsetfocus('SINC001')
           go bottom

           WSEQ = SEQ + 1

           set deleted off
           go top
           if deleted()
              if ! reglock()
                 loop
              endif

              recall
           else
              if ! inc_reg()
                 loop
              endif
           endif
           set deleted on

           save_COB_IA()

           If ! Empty(WLOTE)
              Select BANCO
              ordsetfocus('SINC001')
              Seek WLOTE
              
              If Found()
                 If REGLOCK()
                    Replace BAIXA With BAIXA + WDOSE
                    GRAVA()
                 Endif
              Endif
           Endif

           setcolor(COR_TEXT+"*+")
           @ 06,14 say Strzero(WSEQ,6)
           setcolor(COR_TEXT)

           INKEY(10)

           Exit

        Endif

    Enddo

Enddo

Return .T.


********************
Function tipo_COB_IA
********************

If Lastkey() = 27
   Return .T.
Endif

Setcolor(COR_TEXT)

If WTIPO = "C"
   @ 08,68 Say "Cobertura  "
Elseif WTIPO = "I"
   @ 08,68 Say "Insemina"+Chr(128)+Chr(142)+"o"
Elseif WTIPO = "T"
   @ 08,68 Say "Transplante"
Else
   Keyboard ""
   ERROR("Tipos v"+Chr(160)+"lidos: C-Cobertura  I-Insemina"+Chr(135)+Chr(132)+"o  T-Transplante",0)
   Keyboard Chr(Lastkey())
   Return .F.
Endif

Return .T.


********************
Function busca_BANCO
********************

If Empty(WLOTE) .Or. Lastkey() = 27
   Return .T.
Endif

Select BANCO
ordsetfocus('SINC001')
Seek WLOTE
              
If ! Found()
   CENTRO(24,"N"+Chr(163)+"mero de lote n"+chr(132)+"o cadastrado...")
   Inkey(5)
   @ 24,15 Clear To 24,65
   Return .F.
Endif

Return .T.


*******************
Function alt_COB_IA 
*******************

while .t. 

    sc_COB_IA()

    WSEQ = 0

    setcolor(COR_CONS)
    @ 24,73 say "ESC-FIM"
    @ 24,00 say "ALTERA"+chr(128)+chr(142)+"O"
    setcolor(COR_TEXT)

    @ 06,14 Get WSEQ Picture "@Z 999999" 
    READ_F()
    @ 06,14 Say Strzero(WSEQ,6)

    if empty(WSEQ) .or. lastkey() == 27
       return .t.
    endif

    select SERVICO
    ordsetfocus('SINC001')
    seek WSEQ

    if ! found()
       centro(24,"N"+Chr(163)+"mero de controle n"+chr(132)+"o cadastrado...")
       inkey(5)
       loop
    endif

    WANIMAL   = ANIMAL      
    WDATA     = DATA        
    WTIPO     = TIPO   
    WTOURO    = TOURO
    WCOMUNICA = COMUNICA
    WNUMERO   = NUMERO
    WLOTE     = LOTE  
    WDOSE     = DOSE  
    WMETODO   = METODO
    WTOURO_TE = TOURO_TE
    WMAE_TE   = MAE_TE   
    WRECEP_TE = RECEP_TE 
    WOBS      = OBS   

    WAUX_LOTE = LOTE
    WAUX_DOSE = DOSE

    while .t.

        setcolor(COR_TEXT)

        @ 06,68 Get WDATA                                                       Valid ! empty(WDATA)
        @ 08,14 Get WANIMAL   Picture "@!"                                      Valid busca_ANIMAL("COB_IA") 
        @ 08,68 Get WTIPO     Picture "!"                                       Valid tipo_COB_IA()

        @ 10,14 Get WTOURO    Picture "@!"    When WTIPO = "C" .Or. WTIPO = "N" Valid busca_TOURO()
        @ 10,68 Get WCOMUNICA                 When WTIPO = "C"
        @ 11,68 Get WNUMERO   Picture "@S10"  when WTIPO = "C"

        @ 14,14 Get WLOTE     Picture "@!"    When WTIPO = "I"                  Valid busca_BANCO()
        @ 15,14 Get WDOSE     Picture "@Z 99" When WTIPO = "I"                      
        @ 15,68 Get WMETODO   Picture "@!"    When WTIPO = "I"                      

        @ 18,14 Get WTOURO_TE                 When WTIPO = "T" 
        @ 19,14 Get WMAE_TE                   When WTIPO = "T"                      
        @ 20,14 Get WRECEP_TE                 When WTIPO = "T"                      

        @ 22,14 Get WOBS

        select ANIMAL
        ordsetfocus('SINC001')
        seek WANIMAL
 
        if found()
           @ 08,23 say NOME
        else
           @ 08,23 say space(30)
        endif

        If WTIPO = "C" 

           Seek WTOURO
 
           If Found()
              @ 10,23 Say NOME
           Else
              @ 10,23 Say Space(30)
           Endif
  
           @ 14,14 Say Space(06)
           @ 15,14 Say Space(02)
           @ 15,68 Say Space(03)
  
           @ 18,14 Say Space(30)
           @ 19,14 Say Space(30)
           @ 20,14 Say Space(30)

           WLOTE     = Space(06)
           WDOSE     = 0
           WMETODO   = Space(03)
           WTOURO_TE = Space(30)
           WMAE_TE   = Space(30)
           WRECEP_TE = Space(30)

        Elseif WTIPO = "I" 

           @ 10,14 Say Space(06)
           @ 10,23 Say Space(30)
           @ 10,68 Say Space(08)
           @ 11,68 Say Space(10)

           WTOURO    = Space(06)
           WCOMUNICA = Ctod(Space(08))
           WNUMERO   = Space(15)
           WTOURO_TE = Space(30)
           WMAE_TE   = Space(30)
           WRECEP_TE = Space(30)
  
           @ 18,14 Say Space(30)
           @ 19,14 Say Space(30)
           @ 20,14 Say Space(30)

        Elseif WTIPO = "T" 

           @ 10,14 Say Space(06)
           @ 10,23 Say Space(30)
           @ 10,68 Say Space(08)
           @ 11,68 Say Space(10)

           @ 14,14 Say Space(06)
           @ 15,14 Say Space(02)
           @ 15,68 Say Space(03)

           WTOURO    = Space(06)
           WCOMUNICA = Ctod(Space(08))
           WNUMERO   = Space(15)
           WLOTE     = Space(06)
           WDOSE     = 0
           WMETODO   = Space(03)

        Endif

        If WTIPO = "C" 
           @ 08,68 Say "Cobertura  "
        Elseif WTIPO = "I"
           @ 08,68 Say "Insemina"+Chr(135)+Chr(132)+"o"
        Elseif WTIPO = "T"
           @ 08,68 Say "Transplante"
        Else
           @ 08,68 Say "           "
        Endif

        select SERVICO

        READ_F()

        if lastkey() == 27
           return .t.
        endif

        if confirma(24,"Dados Corretos...(S/N)?","S") == "S"
           save_COB_IA()

           If ! Empty(WAUX_LOTE)
              Select BANCO
              ordsetfocus('SINC001')
              Seek WAUX_LOTE
              
              If Found()
                 If REGLOCK()
                    Replace BAIXA With BAIXA - WAUX_DOSE
                    GRAVA()
                 Endif
              Endif
           Endif

           If ! Empty(WLOTE)
              Select BANCO
              ordsetfocus('SINC001')
              Seek WLOTE
              
              If Found()
                 If REGLOCK()
                    Replace BAIXA With BAIXA + WDOSE
                    GRAVA()
                 Endif
              Endif
           Endif

           exit

        endif

    enddo

enddo

Return .T.


*******************
Function con_COB_IA  
*******************

while .t.

   OPC4 = 8

   sc_COB_IA()

   setcolor(COR_CONS)
   @ 24,00 say "CONSULTA"
   @ 24,73 say "ESC-FIM"
   setcolor(COR_TEXT)

   SOMBRA(08,14,19,67,8)

   setcolor(COR_CONS)
   @ 07,12 clear to 18,66
   @ 07,12 to 18,66 

   @ 08,14 say "Consulta a partir do in"+chr(161)+"cio do arquivo..........[1]"
   @ 09,14 say "Consulta a partir do final do arquivo...........[2]"
   @ 10,14 say "Consulta a partir do n"+Chr(163)+"mero de controle.........[3]"
   @ 11,14 say "Consulta a partir do animal.....................[4]"
   @ 12,14 say "Consulta a partir do touro (cobertura)..........[5]"
   @ 13,14 say "Consulta a partir do lote (insemina"+Chr(135)+Chr(132)+"o).........[6]"
   @ 14,14 say "Consulta a partir do touro (transplante)........[7]"
   @ 15,14 say "Retorno ao menu principal.......................[8]"

   setcolor(COR_TEXT)
   @ 17,14 say "Escolha sua op"+chr(135)+chr(132)+"o de consulta...................[ ]"
   @ 17,63 get OPC4 picture "9" range 1,8
   read_f()

   if OPC4 == 8
      return .t.
   endif

   VOLTA = .F.

   while .t.

      sc_COB_IA()   

      if OPC4 == 1
         select SERVICO
         ordsetfocus('SINC001')
         go top

      elseif OPC4 == 2
         select SERVICO
         ordsetfocus('SINC001')
         go bottom

      elseif OPC4 == 3

         WSEQ  = 0          

         @ 06,14 get WSEQ picture "@Z 999999" 
         read_f()

         If Empty(WSEQ) .Or. Lastkey() == 27
            VOLTA = .T.
            exit
         endif

         select SERVICO
         ordsetfocus('SINC001')
         seek WSEQ  

         if ! found()
            centro(24,"N"+Chr(163)+"mero de controle n"+Chr(132)+"o cadastrado...")
            inkey(5)
            loop
         endif

      elseif OPC4 == 4

         While .T.

             WANIMAL = space(06)          

             @ 08,14 get WANIMAL picture "@!" Valid busca_animal("COB_IA")
             read_f()

             if lastkey() == 27
                VOLTA = .T.
                exit
             endif

             select SERVICO
             ordsetfocus('SINC002')
             seek WANIMAL

             If ! Found()
                centro(24,"N"+Chr(132)+"o foi encontrado nenhum lan"+Chr(135)+"amento para esse animal...")
                inkey(5)
                loop
             Endif

         Exit
         Enddo

      elseif OPC4 == 5

         While .T.

             WTOURO = space(06)          

             @ 10,14 get WTOURO picture "@!" Valid busca_TOURO()
             read_f()

             if lastkey() == 27
                VOLTA = .T.
                exit
             endif

             select SERVICO
             ordsetfocus('SINC003')
             seek WTOURO

             If ! Found()
                centro(24,"N"+Chr(132)+"o foi encontrado nenhum lan"+Chr(135)+"amento para esse animal...")
                inkey(5)
                loop
             Endif

         Exit
         Enddo

      elseif OPC4 == 6

         While .T.

             WLOTE = space(06)          

             @ 14,14 get WLOTE picture "@!" 
             read_f()

             if lastkey() == 27
                VOLTA = .T.
                exit
             endif

             select SERVICO
             ordsetfocus('SINC004')
             seek WLOTE

             If ! Found()
                centro(24,"N"+Chr(132)+"o foi encontrado nenhum lan"+Chr(135)+"amento para esse lote...")
                inkey(5)
                loop
             Endif

         Exit
         Enddo

      Elseif OPC4 == 7

         While .T.

             WTOURO_TE = Space(30)          

             @ 18,14 Get WTOURO_TE 
             READ_F()

             If Lastkey() == 27
                VOLTA = .T.
                Exit
             Endif

             Select SERVICO
             ordsetfocus('SINC005')
             Seek Upper(WTOURO_TE)

             If EOF()    
                Skip -1
             Endif

         Exit
         Enddo

      Endif

      If VOLTA
         Exit
      Endif

      KEY = 0

      while KEY <> 27

         sc_COB_IA()
 
         setcolor(COR_CONS)
         @ 24,00 say "CONSULTA"
         @ 24,73 say "ESC-FIM"

         setcolor(COR_TEXT)
         centro(24,"PgDw-Pr"+chr(162)+"ximo   PgUp-Anterior")

         Select SERVICO

         @ 06,14 Say Strzero(SEQ,6)

         @ 06,68 Say DATA                          
         @ 08,14 Say ANIMAL   Picture "@!"                     
         @ 08,68 Say TIPO     Picture "!"                      

         @ 10,14 Say TOURO    Picture "@!"    
         @ 10,68 Say COMUNICA                 
         @ 11,68 Say NUMERO   Picture "@S10"   

         @ 14,14 Say LOTE     Picture "@!"    
         @ 15,14 Say DOSE     Picture "@Z 99" 
         @ 15,68 Say METODO   Picture "@!"    

         @ 18,14 Say TOURO_TE     
         @ 19,14 Say MAE_TE   
         @ 20,14 Say RECEP_TE     

         @ 22,14 Say OBS

         WANIMAL = ANIMAL
         WTOURO  = TOURO
         WTIPO   = TIPO

         select ANIMAL
         ordsetfocus('SINC001')
         seek WANIMAL
 
         if found()
            @ 08,23 say NOME
         else
            @ 08,23 say space(30)
         endif

         Seek WTOURO
 
         If Found()
            @ 10,23 Say NOME
         Else
            @ 10,23 Say Space(30)
         Endif

         If WTIPO = "C"
            @ 08,68 Say "Cobertura  "
         Elseif WTIPO = "I"
            @ 08,68 Say "Insemina"+Chr(135)+Chr(132)+"o "
         Elseif WTIPO = "T"
            @ 08,68 Say "Transplante"
         Else
            @ 08,68 Say "           "
         Endif

         select SERVICO

         KEY = inkey(0)

         view_USER()

         if KEY == -7
            scalc()
         elseif KEY == -6
            calendario()
         elseif KEY == 3
            skip +1
            if eof()
               skip -1
            endif
         elseif KEY == 18
            skip -1
         elseif KEY == 27
            @ 24,10 clear to 24,70
            exit
         endif
      enddo

      if OPC4 == 1 .or. OPC4 == 2 
         exit
      endif

   Enddo

Enddo

Return .T.


*******************
Function exc_COB_IA
*******************

while .t. 

   sc_COB_IA()

   WSEQ = 0

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "EXCLUS"+chr(142)+"O"
   setcolor(COR_TEXT)

   @ 06,14 get WSEQ picture "@Z 999999"
   read_f()

   if empty(WSEQ) .or. lastkey() == 27
      return .t.
   endif

   select SERVICO
   ordsetfocus('SINC001')
   seek WSEQ

   if ! found()
      centro(24,"N"+Chr(163)+"mero de controle n"+Chr(132)+"o cadastrado...")
      inkey(5)
      loop
   endif

   @ 06,14 Say Strzero(SEQ,6)

   @ 06,68 Say DATA                          
   @ 08,14 Say ANIMAL   Picture "@!"                     
   @ 08,68 Say TIPO     Picture "!"                      

   @ 10,14 Say TOURO    Picture "@!"    
   @ 10,68 Say COMUNICA                 
   @ 11,68 Say NUMERO   Picture "@S10"   

   @ 14,14 Say LOTE     Picture "@!"    
   @ 15,14 Say DOSE     Picture "@Z 99" 
   @ 15,68 Say METODO   Picture "@!"    

   @ 18,14 Say TOURO_TE 
   @ 19,14 Say MAE_TE   
   @ 20,14 Say RECEP_TE 

   @ 22,14 Say OBS

   WANIMAL   = ANIMAL
   WTOURO    = TOURO
   WTIPO     = TIPO
   WAUX_LOTE = LOTE
   WAUX_DOSE = DOSE

   select ANIMAL
   ordsetfocus('SINC001')
   seek WANIMAL
 
   if found()
      @ 08,23 say NOME
   else
      @ 08,23 say space(30)
   endif

   Seek WTOURO
 
   If Found()
      @ 10,23 Say NOME
   Else
      @ 10,23 Say Space(30)
   Endif

   If WTIPO = "C"
      @ 08,68 Say "Cobertura  "
   Elseif WTIPO = "I"
      @ 08,68 Say "Insemina"+Chr(135)+Chr(132)+"o "
   Elseif WTIPO = "T"
      @ 08,68 Say "Transplante"
   Else
      @ 08,68 Say "           "
   Endif

   select SERVICO

   if confirma(24,"Confirma EXCLUS"+chr(142)+"O...(S/N)?","N") == "S"

      if ! reglock()
         loop
      endif                         

      replace SEQ      With 0              ,;        
              ANIMAL   With Space(06)      ,;        
              DATA     With Ctod(Space(08)),;   
              TIPO     With Space(01)      ,;
              TOURO    With Space(06)      ,;
              COMUNICA With Ctod(Space(08)),;
              NUMERO   With Space(15)      ,;
              LOTE     With Space(06)      ,;
              DOSE     With 0              ,;
              METODO   With Space(03)      ,;
              TOURO_TE With Space(30)      ,;
              MAE_TE   With Space(30)      ,;
              RECEP_TE With Space(30)      ,;
              OBS      With Space(40)      ,;
              USUARIO  With Space(20)      ,;
              DATE     With Ctod(Space(08)),;
              TIME     With Space(08)

      DELETE

      GRAVA()

      If ! Empty(WAUX_LOTE)
         Select BANCO
         ordsetfocus('SINC001')
         Seek WAUX_LOTE
         
         If Found()
            If REGLOCK()
               Replace BAIXA With BAIXA - WAUX_DOSE
               GRAVA()
            Endif
         Endif
      Endif

   Endif

Enddo

Return .T.


********************
Function save_COB_IA
********************

If ! REGLOCK()
   Return .F.
Endif
   
Replace SEQ      With WSEQ     ,;    
        DATA     With WDATA    ,;
        ANIMAL   With WANIMAL  ,; 
        TIPO     With WTIPO    ,;
        TOURO    With WTOURO   ,;
        COMUNICA With WCOMUNICA,;
        NUMERO   With WNUMERO  ,;
        LOTE     With WLOTE    ,;
        DOSE     With WDOSE    ,;
        METODO   With WMETODO  ,;
        TOURO_TE With WTOURO_TE,;
        MAE_TE   With WMAE_TE  ,;
        RECEP_TE With WRECEP_TE,;
        OBS      With WOBS     ,;
        USUARIO  With WPUSUARIO,;
        TIME     With TIME()   ,;
        DATE     With DATE()

GRAVA()

Return .T.


****************
Function PRENHEZ
****************
OPC3 = 1

while .t.

   setcolor(COR_CAB) 
   @ 03,42 say "PRENHEZ                            " 
   setcolor(COR_TEXT)

   private OPCAO[5]
   OPCAO[1] = " Inclus"+chr(132)+"o   " 
   OPCAO[2] = " Altera"+chr(135)+chr(132)+"o  " 
   OPCAO[3] = " Consulta   " 
   OPCAO[4] = " Exclus"+chr(132)+"o   " 
   OPCAO[5] = " Esc - Fim  " 

   private ACESSO[5]
   ACESSO[1] = .t.
   ACESSO[2] = .t.
   ACESSO[3] = .t.
   ACESSO[4] = .t.
   ACESSO[5] = .t.

   private LETRA[5]
   LETRA[1] = "I"  
   LETRA[2] = "A"
   LETRA[3] = "C"
   LETRA[4] = "E"
   LETRA[5] = "F"

   private POS[5] 
   POS[1] = 1
   POS[2] = 1
   POS[3] = 1
   POS[4] = 1
   POS[5] = 7

   private OBS_MENU[5]  
   OBS_MENU[1] = "Inclus"+chr(132)+"o de prenhez!" 
   OBS_MENU[2] = "Altera"+chr(135)+chr(132)+"o de prenhez!"       
   OBS_MENU[3] = "Consulta as prenhez!"                                       
   OBS_MENU[4] = "Exclus"+chr(132)+"o de prenhez!"  
   OBS_MENU[5] = "Retornar ao menu principal!"                                                                                            

   OPC3 = achoise(OPC3,09,32,15,45,2,OPCAO,ACESSO,LETRA,POS,OBS_MENU,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)
   
   if OPC3 == 0 .or. OPC3 == 5
      return .t.
   elseif OPC3 == 1
      inc_PRENHEZ()
   elseif OPC3 == 2
      alt_PRENHEZ()
   elseif OPC3 == 3
      con_PRENHEZ()
   elseif OPC3 == 4
      exc_PRENHEZ()
   endif

enddo

Return .T.


*******************
Function sc_PRENHEZ
*******************

@ 05,00 Clear To 22,79
@ 24,00 Clear To 24,79

@ 08,01 say "Controle..:"
@ 10,01 say "Data......:"
@ 12,01 say "Animal....:        - " 
@ 14,01 say "Touro.....:        - " 
@ 16,01 say "Observa"+Chr(135)+Chr(132)+"o:" 

Return .T.


********************
Function inc_PRENHEZ
********************

while .t. 

    WDATA   = DATE()   
    WANIMAL = Space(06)
    WTOURO  = Space(06)
    WOBS    = Space(40)

    Select EXAME
    ordsetfocus('SINC001')
    Go Bottom
  
    WSEQ = SEQ + 1

    sc_PRENHEZ()

    setcolor(COR_CONS)
    @ 24,00 say "INCLUS"+chr(142)+"O"
    @ 24,73 say "ESC-FIM"
    setcolor(COR_TEXT)

    @ 08,13 say Strzero(WSEQ,6)

    while .t. 

        setcolor(COR_TEXT)

        @ 10,13 get WDATA                           valid ! empty(WDATA)
        @ 12,13 get WANIMAL    picture "@!"         valid busca_ANIMAL("PRENHEZ")
        @ 14,13 get WTOURO     picture "@!"         valid busca_TOURO()
        @ 16,13 get WOBS 

        select ANIMAL
        ordsetfocus('SINC001')
        seek WANIMAL
 
        if found()
           @ 12,23 say NOME
        else
           @ 12,23 say Space(30)
        endif

        Seek WTOURO

        if found()
           @ 14,23 say NOME
        else
           @ 14,23 say Space(30)
        endif

        select EXAME

        read_f()

        if lastkey() == 27
           return .t.
        endif

        if CONFIRMA(24,"Dados corretos...(S/N)?","S") == "S"

           select EXAME
           ordsetfocus('SINC001')
           go bottom

           WSEQ = SEQ + 1

           set deleted off
           go top
           if deleted()
              if ! reglock()
                 loop
              endif

              recall
           else
              if ! inc_reg()
                 loop
              endif
           endif
           set deleted on

           save_PRENHEZ()

           setcolor(COR_TEXT+"*+")
           @ 08,13 say Strzero(WSEQ,6)
           setcolor(COR_TEXT)

           Inkey(10)

           Exit
   
        Endif

    Enddo

Enddo

Return .T.


********************
Function alt_PRENHEZ
********************

While .T. 

   sc_PRENHEZ()

   WSEQ = 0

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "ALTERA"+chr(128)+chr(142)+"O"
   setcolor(COR_TEXT)

   @ 08,13 get WSEQ picture "@Z 999999" 
   read_f()
   @ 08,13 say Strzero(WSEQ,6)

   @ 24,10 clear to 24,79

   if empty(WSEQ) .or. lastkey() == 27
      return .t.
   endif

   select EXAME
   ordsetfocus('SINC001')
   seek WSEQ

   if ! found()
      centro(24,"N"+Chr(163)+"mero de controle n"+chr(132)+"o cadastrado...")
      inkey(1)
      @ 24,10 clear to 24,79
      loop
   endif

   WDATA   = DATA        
   WANIMAL = ANIMAL      
   WTOURO  = TOURO
   WOBS    = OBS

   while .t.

      setcolor(COR_TEXT)

      @ 10,13 get WDATA                 Valid ! Empty(WDATA)
      @ 12,13 get WANIMAL picture "@!"  Valid busca_ANIMAL("PRENHEZ")
      @ 14,13 get WTOURO  picture "@!"  Valid busca_TOURO()
      @ 16,13 get WOBS 

      select ANIMAL
      ordsetfocus('SINC001')
      seek WANIMAL
 
      if found()
         @ 12,23 say NOME
      else
         @ 12,23 say Space(30)
      endif

      Seek WTOURO

      if found()
         @ 14,23 say NOME
      else
         @ 14,23 say Space(30)
      endif

      select EXAME

      read_f()

      if lastkey() == 27
         return .t.
      endif

      if confirma(24,"Dados corretos...(S/N)?","S") == "S"
         save_PRENHEZ()
         exit
      endif

   Enddo

Enddo

Return .T.


********************
Function con_PRENHEZ
********************

while .t.

   OPC4 = 6

   sc_PRENHEZ()

   setcolor(COR_CONS)
   @ 24,00 say "CONSULTA"
   @ 24,73 say "ESC-FIM"
   setcolor(COR_TEXT)

   SOMBRA(09,14,18,67,8)

   setcolor(COR_CONS)
   @ 08,12 clear to 17,66
   @ 08,12 to 17,66 

   @ 09,14 say "Consulta a partir do in"+chr(161)+"cio do arquivo..........[1]"
   @ 10,14 say "Consulta a partir do final do arquivo...........[2]"
   @ 11,14 say "Consulta a partir do n"+Chr(163)+"mero de controle.........[3]"
   @ 12,14 say "Consulta a partir do Animal.....................[4]"
   @ 13,14 say "Consulta a partir do Touro......................[5]"
   @ 14,14 say "Retorno ao menu principal.......................[6]"

   setcolor(COR_TEXT)
   @ 16,14 say "Escolha sua op"+chr(135)+chr(132)+"o de consulta...................[ ]"
   @ 16,63 get OPC4 Picture "9" Range 1,6
   read_f()

   if OPC4 == 6
      return .t.
   endif

   VOLTA = .F.

   while .t.

      sc_PRENHEZ()

      if OPC4 == 1
         select EXAME
         ordsetfocus('SINC001')
         go top

      elseif OPC4 == 2
         select EXAME
         ordsetfocus('SINC001')
         go bottom

      elseif OPC4 == 3

         WSEQ = 0          

         @ 08,13 get WSEQ picture "@Z 999999" 
         read_f()

         if lastkey() == 27
            VOLTA = .T.
            exit
         endif

         select EXAME
         ordsetfocus('SINC001')
         seek WSEQ  

         if ! found()
            centro(24,"N"+Chr(163)+"mero de controle n"+chr(132)+"o cadastrado...")
            inkey(5)
            @ 24,10 clear to 24,79
            loop
         endif

      elseif OPC4 == 4

         WANIMAL = space(06)          

         @ 12,13 get WANIMAL picture "@!" valid busca_ANIMAL("PRENHEZ") 
         read_f()

         if lastkey() == 27
            VOLTA = .T.
            exit
         endif

         select EXAME
         ordsetfocus('SINC002')
         seek WANIMAL

         if ! found()
            centro(24,"N"+Chr(132)+"o houve nenhum lan"+Chr(135)+"amento para esse animal...")
            inkey(5)
            @ 24,10 clear to 24,79
            loop
         endif

      elseif OPC4 == 5

         WTOURO = space(06)          

         @ 14,13 get WTOURO picture "@!" valid busca_TOURO()            
         read_f()

         if lastkey() == 27
            VOLTA = .T.
            exit
         endif

         select EXAME
         ordsetfocus('SINC003')
         seek WTOURO

         if ! found()
            centro(24,"N"+Chr(132)+"o houve nenhum lan"+Chr(135)+"amento para esse animal...")
            inkey(5)
            @ 24,10 clear to 24,79
            loop
         endif

      endif

      If VOLTA
         Exit
      Endif

      CENTRO(24,"PgDw-Pr"+chr(162)+"ximo   PgUp-Anterior")

      KEY = 0

      while KEY  <> 27

          setcolor(COR_TEXT)

          @ 08,13 say Strzero(SEQ,6)
          @ 10,13 say DATA                 
          @ 12,13 say ANIMAL picture "@!"  
          @ 14,13 say TOURO  picture "@!"  
          @ 16,13 say OBS 

          WANIMAL = ANIMAL
          WTOURO  = TOURO

          select ANIMAL
          ordsetfocus('SINC001')
          seek WANIMAL
 
          if found()
             @ 12,23 say NOME
          else
             @ 12,23 say Space(30)
          endif

          Seek WTOURO

          if found()
             @ 14,23 say NOME
          else
             @ 14,23 say Space(30)
          endif

          select EXAME

          KEY = inkey(0)

          view_user()

          if KEY == -7
             scalc()
          elseif KEY == -6
             calendario()
          elseif KEY == 3
             skip +1
             if eof()
                skip -1
             endif
          elseif KEY == 18
             skip -1
          elseif KEY == 27
             exit
          endif

       enddo

       If OPC4 == 1 .Or. OPC4 == 2 
          Exit
       Endif

    Enddo

Enddo

Return .T.


********************
Function exc_PRENHEZ
********************


while .t. 

   sc_PRENHEZ()

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "EXCLUS"+chr(142)+"O"
   setcolor(COR_TEXT)

   WSEQ = 0

   @ 08,13 get WSEQ Picture "@Z 999999"
   read_f()
   @ 08,13 say Strzero(WSEQ,6)

   if empty(WSEQ) .or. lastkey() == 27
      return .t.
   endif

   select EXAME
   ordsetfocus('SINC001')
   seek WSEQ

   if ! found()
      centro(24,"N"+Chr(163)+"mero de controle n"+chr(132)+"o cadastrado...")
      inkey(5)
      @ 24,10 clear to 24,79
      loop
   endif

   setcolor(COR_TEXT)

   @ 08,13 say Strzero(SEQ,6)
   @ 10,13 say DATA                 
   @ 12,13 say ANIMAL picture "@!"  
   @ 14,13 say TOURO  picture "@!"  
   @ 16,13 say OBS 

   WANIMAL = ANIMAL
   WTOURO  = TOURO

   select ANIMAL
   ordsetfocus('SINC001')
   seek WANIMAL
 
   if found()
      @ 12,23 say NOME
   else
      @ 12,23 say Space(30)
   endif

   Seek WTOURO

   if found()
      @ 14,23 say NOME
   else
      @ 14,23 say Space(30)
   endif

   select EXAME

   if confirma(24,"Confirma EXCLUS"+chr(142)+"O...(S/N)?","N") == "S"

      if ! reglock()
         loop
      endif                         

      replace SEQ     With 0              ,;        
              DATA    With Ctod(space(08)),;   
              ANIMAL  With Space(06)      ,;        
              TOURO   With Space(06)      ,;
              OBS     With 0              ,;
              USUARIO With Space(20)      ,;
              DATE    With Ctod(Space(08)),;
              TIME    With Space(08)

      delete

      grava()

   endif

enddo

return .t.


*********************
Function save_PRENHEZ
*********************

Select EXAME

if ! reglock()
   return .f.
endif
   
replace SEQ     With WSEQ     ,;    
        DATA    With WDATA    ,;
        ANIMAL  With WANIMAL  ,; 
        TOURO   With WTOURO   ,;
        OBS     With WOBS     ,;
        USUARIO With WPUSUARIO,;
        TIME    With time()   ,;
        DATE    With date()

grava()

return .t.


***************
Function ABORTO
***************
OPC3 = 1

while .t.

   setcolor(COR_CAB) 
   @ 03,42 say "ABORTOS                            " 
   setcolor(COR_TEXT)

   private OPCAO[5]
   OPCAO[1] = " Inclus"+chr(132)+"o   " 
   OPCAO[2] = " Altera"+chr(135)+chr(132)+"o  " 
   OPCAO[3] = " Consulta   " 
   OPCAO[4] = " Exclus"+chr(132)+"o   " 
   OPCAO[5] = " Esc - Fim  " 

   private ACESSO[5]
   ACESSO[1] = .t.
   ACESSO[2] = .t.
   ACESSO[3] = .t.
   ACESSO[4] = .t.
   ACESSO[5] = .t.

   private LETRA[5]
   LETRA[1] = "I"  
   LETRA[2] = "A"
   LETRA[3] = "C"
   LETRA[4] = "E"
   LETRA[5] = "F"

   private POS[5] 
   POS[1] = 1
   POS[2] = 1
   POS[3] = 1
   POS[4] = 1
   POS[5] = 7

   private OBS_MENU[5]  
   OBS_MENU[1] = "Inclus"+chr(132)+"o de abortos!" 
   OBS_MENU[2] = "Altera"+chr(135)+chr(132)+"o de abortos!"       
   OBS_MENU[3] = "Consulta aos abortos!"                                       
   OBS_MENU[4] = "Exclus"+chr(132)+"o de abortos!"  
   OBS_MENU[5] = "Retorno ao menu principal!"   

   OPC3 = achoise(OPC3,09,32,15,45,2,OPCAO,ACESSO,LETRA,POS,OBS_MENU,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)
   
   if OPC3 == 0 .or. OPC3 == 5
      return .t.
   elseif OPC3 == 1
      inc_ABORTO()
   elseif OPC3 == 2
      alt_ABORTO()
   elseif OPC3 == 3
      con_ABORTO()
   elseif OPC3 == 4
      exc_ABORTO()
   endif

enddo

Return .T.


******************
Function SC_ABORTO
******************

@ 05,00 Clear To 22,79
@ 24,00 Clear To 24,79

@ 08,01 Say "Controle..:"
@ 10,01 Say "Data......:"
@ 12,01 Say "Animal....:        - " 
@ 14,01 Say "Observa"+Chr(135)+Chr(132)+"o:"

Return .T.


*******************
Function inc_ABORTO
*******************

while .t. 

    WANIMAL = Space(06)
    WDATA   = date()   
    WOBS    = Space(40)

    sc_ABORTO()

    setcolor(COR_CONS)
    @ 24,00 say "INCLUS"+chr(142)+"O"
    @ 24,73 say "ESC-FIM"
    setcolor(COR_TEXT)

    Select ABORTO
    ordsetfocus('SINC001')
    Go Bottom

    WSEQ = SEQ + 1

    @ 08,13 say Strzero(WSEQ,6)

    While .T.

        setcolor(COR_TEXT)

        @ 10,13 get WDATA                 Valid ! empty(WDATA)
        @ 12,13 get WANIMAL picture "@!"  Valid busca_ANIMAL("ABORTO")
        @ 14,13 get WOBS

        select ANIMAL
        ordsetfocus('SINC001')
        seek WANIMAL
 
        if found()
           @ 12,22 say NOME
        else
           @ 12,22 say space(30)
        endif

        select ABORTO

        read_f()

        if lastkey() == 27
           return .t.
        endif

        if confirma(24,"Dados corretos...(S/N)?","S") == "S"

           select ABORTO
           ordsetfocus('SINC001')
           go bottom

           WSEQ = SEQ + 1

           set deleted off
           go top
           if deleted()
              if ! reglock()
                 loop
              endif

              recall
           else
              if ! inc_reg()
                 loop
              endif
           endif
           set deleted on

           save_ABORTO()

           setcolor(COR_TEXT+"*+")
           @ 08,13 say Strzero(WSEQ,6)
           setcolor(COR_TEXT)

           Inkey(10)

           Exit

        Endif

    Enddo

Enddo

Return .T.


*******************
Function alt_ABORTO
*******************

while .t. 

   sc_ABORTO()

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "ALTERA"+chr(128)+chr(142)+"O"
   setcolor(COR_TEXT)

   WSEQ = 0

   @ 08,13 get WSEQ picture "@Z 999999" 
   read_f()
   @ 08,13 say Strzero(WSEQ,6)

   if empty(WSEQ) .or. lastkey() == 27
      return .t.
   endif

   select ABORTO
   ordsetfocus('SINC001')
   seek WSEQ

   if ! found()
      centro(24,"N"+Chr(163)+"mero de controle n"+chr(132)+"o cadastrado...")
      inkey(5)
      loop
   endif

   WDATA   = DATA        
   WANIMAL = ANIMAL      
   WOBS    = OBS

   While .T.

      setcolor(COR_TEXT)

      @ 10,13 get WDATA                valid ! empty(WDATA)
      @ 12,13 get WANIMAL picture "@!" valid busca_ANIMAL("ABORTO")
      @ 14,13 get WOBS

      select ANIMAL
      ordsetfocus('SINC001')
      seek WANIMAL
 
      if found()
         @ 12,22 say NOME
      else
         @ 12,22 say space(30)
      endif

      select ABORTO

      read_f()

      if lastkey() == 27
         return .t.
      endif

      if confirma(24,"Dados corretos...(S/N)?","S") == "S"
         save_ABORTO()
         exit
      endif

   Enddo

Enddo

Return .T.


*******************
Function con_ABORTO
*******************

while .t.

   OPC4 = 5

   sc_ABORTO()

   setcolor(COR_CONS)
   @ 24,00 say "CONSULTA"
   @ 24,73 say "ESC-FIM"
   setcolor(COR_TEXT)

   SOMBRA(10,14,18,67,8)
  
   setcolor(COR_CONS)
   @ 09,12 clear to 17,66
   @ 09,12 to 17,66 

   @ 10,14 say "Consulta a partir do in"+chr(161)+"cio do arquivo..........[1]"
   @ 11,14 say "Consulta a partir do final do arquivo...........[2]"
   @ 12,14 say "Consulta a partir do n"+Chr(163)+"mero de controle.........[3]"
   @ 13,14 say "Consulta a partir do animal.....................[4]"
   @ 14,14 say "Retorno ao menu principal.......................[5]"

   setcolor(COR_TEXT)
   @ 16,14 say "Escolha sua op"+chr(135)+chr(132)+"o de consulta...................[ ]"
   @ 16,63 get OPC4 picture "9" range 1,5
   read_f()

   if OPC4 == 5
      return .t.
   endif

   VOLTA = .F.

   while .t.

      sc_ABORTO()

      if OPC4 == 1
         select ABORTO
         ordsetfocus('SINC001')
         go top

      elseif OPC4 == 2
         select ABORTO
         ordsetfocus('SINC001')
         go bottom

      elseif OPC4 == 3

         WSEQ = 0          

         @ 08,13 get WSEQ picture "@Z 999999" 
         read_f()

         if lastkey() == 27
            VOLTA = .T.
            exit
         endif

         select ABORTO
         ordsetfocus('SINC001')
         seek WSEQ  

         if ! found()
            centro(24,"N"+Chr(163)+"mero de controle n"+chr(132)+"o cadastrado...")
            inkey(1)
            @ 24,10 clear to 24,79
            loop
         endif

      elseif OPC4 == 4

         WANIMAL = space(06)          

         @ 12,13 get WANIMAL picture "@!" valid busca_ANIMAL("ABORTO") 
         read_f()

         if lastkey() == 27
            VOLTA = .T.
            exit
         endif

         select ABORTO
         ordsetfocus('SINC002')
         seek WANIMAL

         if ! found()
            centro(24,"N"+Chr(132)+"o houve nenhum lan"+Chr(135)+"amento para esse animal...")
            inkey(1)
            @ 24,10 clear to 24,79
            loop
         endif

      endif

      If VOLTA 
         Exit
      Endif

      centro(24,"PgDw-Pr"+chr(162)+"ximo   PgUp-Anterior")

      KEY = 0

      while KEY  <> 27
      
         @ 08,13 say Strzero(SEQ,6)
         @ 10,13 say DATA                               
         @ 12,13 say ANIMAL     
         @ 14,13 say OBS

         WANIMAL = ANIMAL

         select ANIMAL
         ordsetfocus('SINC001')
         seek WANIMAL
 
         if found()
            @ 12,22 say NOME
         else
            @ 12,22 say space(30)
         endif

         select ABORTO

         KEY = inkey(0)

         view_user()

         if KEY == -7
            scalc()
         elseif KEY == -6
            calendario()
         elseif KEY == 3
            skip 
            if eof()
               skip -1
            endif
         elseif KEY == 18
            skip -1
         elseif KEY == 27
            exit
         endif

      enddo

      if OPC4 == 1 .or. OPC4 == 2 
         exit
      endif

   enddo

Enddo

Return .T.


*******************
Function exc_ABORTO
*******************

while .t. 

   sc_ABORTO()

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "EXCLUS"+chr(142)+"O"
   setcolor(COR_TEXT)

   WSEQ = 0

   @ 08,13 get WSEQ picture "@Z 999999"
   read_f()
   @ 08,13 say Strzero(WSEQ,6)

   if empty(WSEQ) .or. lastkey() == 27
      return .t.
   endif

   select ABORTO
   ordsetfocus('SINC001')
   seek WSEQ

   if ! found()
      centro(24,"N"+Chr(163)+"mero de controle n"+chr(132)+"o cadastrado...")
      inkey(1)
      @ 24,10 clear to 24,79
      loop
   endif

   @ 10,13 say DATA                               
   @ 12,13 say ANIMAL     
   @ 14,13 say OBS

   WANIMAL = ANIMAL

   select ANIMAL
   ordsetfocus('SINC001')
   seek WANIMAL
 
   if found()
      @ 12,22 say NOME
   else
      @ 12,22 say space(30)
   endif

   select ABORTO

   if confirma(24,"Confirma EXCLUS"+chr(142)+"O...(S/N)?","N") == "S"

      if ! reglock()
         loop
      endif                         

      replace SEQ     With 0              ,;        
              DATA    With Ctod(Space(08)),;   
              ANIMAL  With Space(06)      ,;        
              OBS     With Space(06)      ,;        
              USUARIO With Space(20)      ,;
              DATE    With Ctod(Space(08)),;
              TIME    With Space(08)

      delete

      grava()

   endif

enddo

return .t.


********************
Function save_ABORTO
********************

select ABORTO

if ! reglock()
   return .f.
endif
   
replace SEQ     With WSEQ     ,;    
        DATA    With WDATA    ,;
        ANIMAL  With WANIMAL  ,; 
        OBS     With WOBS     ,;
        USUARIO With WPUSUARIO,;
        TIME    With TIME()   ,;
        DATE    With DATE()

GRAVA()

Return .T.


***************
Function PARTOS
***************
OPC3 = 1

while .t.

   setcolor(COR_CAB) 
   @ 03,42 say "PARTOS/DESMAME                     " 
   setcolor(COR_TEXT)

   private OPCAO[5]
   OPCAO[1] = " Inclus"+chr(132)+"o   " 
   OPCAO[2] = " Altera"+chr(135)+chr(132)+"o  " 
   OPCAO[3] = " Consulta   " 
   OPCAO[4] = " Exclus"+chr(132)+"o   " 
   OPCAO[5] = " Esc - Fim  " 

   private ACESSO[5]
   ACESSO[1] = .t.
   ACESSO[2] = .t.
   ACESSO[3] = .t.
   ACESSO[4] = .t.
   ACESSO[5] = .t.

   private LETRA[5]
   LETRA[1] = "I"  
   LETRA[2] = "A"
   LETRA[3] = "C"
   LETRA[4] = "E"
   LETRA[5] = "F"

   private POS[5] 
   POS[1] = 1
   POS[2] = 1
   POS[3] = 1
   POS[4] = 1
   POS[5] = 7

   private OBS_MENU[5]  
   OBS_MENU[1] = "Inclus"+chr(132)+"o de partos!" 
   OBS_MENU[2] = "Altera"+chr(135)+chr(132)+"o de partos!"       
   OBS_MENU[3] = "Consulta aos partos!"                                       
   OBS_MENU[4] = "Exclus"+chr(132)+"o de partos!"  
   OBS_MENU[5] = "Retornar ao menu principal!"

   OPC3 = achoise(OPC3,09,32,15,45,2,OPCAO,ACESSO,LETRA,POS,OBS_MENU,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)
   
   if OPC3 == 0 .or. OPC3 == 5
      return .t.
   elseif OPC3 == 1
      inc_PARTO()
   elseif OPC3 == 2
      alt_PARTO()
   elseif OPC3 == 3
      con_PARTO()
   elseif OPC3 == 4
      exc_PARTO()
   endif

enddo

Return .T.


*****************
Function sc_PARTO
*****************

@ 05,00 Clear To 22,79
@ 24,00 Clear To 24,79

@ 06,01 say "Controle..:"
@ 08,01 say "Data......:"
@ 10,01 say "Mae.......:        - " 
@ 12,01 say "Bezerro...:"
@ 13,01 say "Sexo......:" 
@ 15,01 say "Pai.......:        - "
@ 17,01 say "Resenha...:"

Setcolor("BG/B")
@ 19,01 say "Desmame...:"

Setcolor(COR_TEXT)
@ 21,01 say "Observa"+Chr(135)+Chr(132)+"o:"

Return .T.


******************
Function inc_PARTO
******************

While .T.

    sc_PARTO()

    setcolor(COR_CONS)
    @ 24,00 say "INCLUS"+chr(142)+"O"
    @ 24,73 say "ESC-FIM"
    setcolor(COR_TEXT)

    WDATA    = DATE()   
    WMAE     = Space(06)
    WBEZERRO = Space(30)
    WSEXO    = Space(01)
    WPAI     = Space(06)
    WRESENHA = Space(15)
    WDESMAME = Ctod(Space(08))
    WOBS     = Space(40)

    select PARTO
    ordsetfocus('SINC001')
    Go Bottom

    WSEQ = SEQ + 1

    @ 06,13 Say Strzero(WSEQ,6)

    while .t. 

        setcolor(COR_TEXT)

        @ 08,13 get WDATA                           valid ! empty(WDATA)
        @ 10,13 get WMAE       picture "@!"         valid busca_ANIMAL("PARTO") 

        @ 12,13 get WBEZERRO   
        @ 13,13 get WSEXO      picture "@!"         valid busca_SEXO(13,14) 

        @ 15,13 get WPAI       picture "@!"         Valid busca_TOURO()

        @ 17,13 get WRESENHA 
        @ 19,13 get WDESMAME

        @ 21,13 get WOBS
       
        select ANIMAL
        ordsetfocus('SINC001')
        seek WMAE
 
        if found()
           @ 10,22 say NOME
        else
           @ 10,22 say space(30)
        endif

        seek WPAI
 
        if found()
           @ 15,22 say NOME
        else
           @ 15,22 say space(30)
        endif

        If WSEXO = "M"
           @ 13,14 Say "-Macho"
        Elseif WSEXO = "F"
           @ 13,14 Say "-Femea"
        Endif

        select PARTO

        read_f()

        if lastkey() == 27
           return .t.
        endif

        if confirma(24,"Dados Corretos...(S/N)?","S") == "S"

           select PARTO
           ordsetfocus('SINC001')
           go bottom

           WSEQ = SEQ + 1

           set deleted off
           go top
           if deleted()
              if ! reglock()
                 loop
              endif

              recall
           else
              if ! inc_reg()
                 loop
              endif
           endif
           set deleted on

           save_PARTO()

           setcolor(COR_TEXT+"*+")
           @ 06,13 say Strzero(WSEQ,6)
           setcolor(COR_TEXT)

           Inkey(5)
  
           Exit

        endif

   Enddo

Enddo

Return .T.


******************
Function alt_PARTO
******************

while .t. 

   sc_PARTO()

   WSEQ = 0

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "ALTERA"+chr(128)+chr(142)+"O"
   setcolor(COR_TEXT)

   @ 06,13 get WSEQ picture "@Z 999999" 
   read_f()

   if empty(WSEQ) .or. lastkey() == 27
      return .t.
   endif

   select PARTO
   ordsetfocus('SINC001')
   seek WSEQ

   if ! found()
      centro(24,"N"+Chr(163)+"mero de controle n"+chr(132)+"o cadastrado...")
      inkey(1)
      @ 24,10 clear to 24,79
      loop
   endif

   WDATA    = DATA        
   WMAE     = MAE         
   WBEZERRO = BEZERRO
   WSEXO    = SEXO
   WPAI     = PAI
   WRESENHA = RESENHA
   WDESMAME = DESMAME
   WOBS     = OBS

   while .t.

       setcolor(COR_TEXT)

       @ 08,13 get WDATA                           valid ! empty(WDATA)
       @ 10,13 get WMAE       picture "@!"         valid busca_ANIMAL("PARTO") 

       @ 12,13 get WBEZERRO   
       @ 13,13 get WSEXO      picture "@!"         valid busca_SEXO(13,14) 

       @ 15,13 get WPAI       picture "@!"         Valid busca_TOURO()

       @ 17,13 get WRESENHA 
       @ 19,13 get WDESMAME

       @ 21,13 get WOBS
       
       select ANIMAL
       ordsetfocus('SINC001')
       seek WMAE
 
       if found()
          @ 10,22 say NOME
       else
          @ 10,22 say space(30)
       endif

       seek WPAI
 
       if found()
          @ 15,22 say NOME
       else
          @ 15,22 say space(30)
       endif

       If WSEXO = "M"
          @ 13,14 Say "-Macho"
       Elseif WSEXO = "F"
          @ 13,14 Say "-Femea"
       Endif

       select PARTO

       read_f()

       if lastkey() == 27
          return .t.
       endif

       if confirma(24,"Dados corretos...(S/N)?","S") == "S"
          save_PARTO()
          exit
       endif

    enddo

enddo

Return .T.


******************
Function con_PARTO
******************

while .t.

   OPC4 = 6

   sc_PARTO()

   setcolor(COR_CONS)
   @ 24,00 say "CONSULTA"
   @ 24,73 say "ESC-FIM"
   setcolor(COR_TEXT)

   SOMBRA(09,14,18,67,8)

   setcolor(COR_CONS)
   @ 08,12 clear to 17,66
   @ 08,12 to 17,66 

   @ 09,14 say "Consulta a partir do in"+chr(161)+"cio do arquivo..........[1]"
   @ 10,14 say "Consulta a partir do final do arquivo...........[2]"
   @ 11,14 say "Consulta a partir do n"+Chr(163)+"mero de controle.........[3]"
   @ 12,14 say "Consulta a partir da mae........................[4]"
   @ 13,14 say "Consulta a partir do pai........................[5]"
   @ 14,14 say "Retorno ao menu principal.......................[6]"

   setcolor(COR_TEXT)
   @ 16,14 say "Escolha sua op"+chr(135)+chr(132)+"o de consulta...................[ ]"
   @ 16,63 get OPC4 picture "9" range 1,6
   read_f()

   if OPC4 == 6
      return .t.
   endif

   while .t.

      sc_PARTO()

       VOLTA = .F.

      if OPC4 == 1
         select PARTO
         ordsetfocus('SINC001')
         go top

      elseif OPC4 == 2
         select PARTO
         ordsetfocus('SINC001')
         go bottom

      elseif OPC4 == 3

         WSEQ = 0          

         @ 06,13 get WSEQ picture "@Z 999999" 
         read_f()

         if lastkey() == 27
            VOLTA = .T.
            exit
         endif

         select PARTO
         ordsetfocus('SINC001')
         seek WSEQ  

         if ! found()
            centro(24,"N"+Chr(163)+"mero de controle n"+chr(132)+"o cadastrado...")
            inkey(5)
            @ 24,10 clear to 24,79
            loop
         endif

      elseif OPC4 == 4

         WMAE = space(06)          

         @ 10,13 get WMAE picture "@!" valid busca_ANIMAL("PARTO")
         read_f()

         if lastkey() == 27
            VOLTA = .T.
            exit
         endif

         select PARTO
         ordsetfocus('SINC002')
         seek WMAE

         if ! found()
            centro(24,"N"+Chr(132)+"o houve nenhum lan"+Chr(135)+"amento para esse animal...")
            inkey(5)
            @ 24,10 clear to 24,79
            loop
         endif

      elseif OPC4 == 5

         WPAI = space(06)          

         @ 15,13 get WPAI picture "@!" valid busca_TOURO()
         read_f()

         if lastkey() == 27
            VOLTA = .T.
            exit
         endif

         select PARTO
         ordsetfocus('SINC003')
         seek WPAI

         if ! found()
            centro(24,"N"+Chr(132)+"o houve nenhum lan"+Chr(135)+"amento para esse animal...")
            inkey(5)
            @ 24,10 clear to 24,79
            loop
         endif

      endif

      If VOLTA
         Exit
      Endif

      centro(24,"PgDw-Pr"+chr(162)+"ximo   PgUp-Anterior")

      KEY = 0

      while KEY <> 27

         setcolor(COR_TEXT)

         @ 06,13 say Strzero(SEQ,6)

         @ 08,13 say DATA                           
         @ 10,13 say MAE       picture "@!"         

         @ 12,13 say BEZERRO   
         @ 13,13 say SEXO      picture "@!"          

         @ 15,13 say PAI       picture "@!"     

         @ 17,13 say RESENHA                    
         @ 19,13 say DESMAME                    

         @ 21,13 say OBS
 
         WMAE = MAE
         WPAI = PAI
     
         select ANIMAL
         ordsetfocus('SINC001')
         seek WMAE
 
         if found()
            @ 10,22 say NOME
         else
            @ 10,22 say space(30)
         endif

         seek WPAI
 
         if found()
            @ 15,22 say NOME
         else
            @ 15,22 say space(30)
         endif

         select PARTO

         If SEXO = "M"
            @ 13,14 Say "-Macho"
         Elseif SEXO = "F"
            @ 13,14 Say "-Femea"
         Endif

         KEY = inkey(0)

         view_user()

         if KEY == -7
            scalc()
         elseif KEY == -6
            calendario()
         elseif KEY == 3
            skip 
            if eof()
               skip -1
            endif
         elseif KEY == 18
            skip -1
         elseif KEY == 27
            exit
         endif

      enddo

      if OPC4 == 1 .or. OPC4 == 2 
         exit
      endif

   Enddo

Enddo

Return .T.


******************
Function exc_PARTO
******************

while .t. 

   sc_PARTO()

   WSEQ = 0

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "EXCLUS"+chr(142)+"O"
   setcolor(COR_TEXT)

   @ 06,13 get WSEQ picture "@Z 999999"
   read_f()
   @ 08,13 say Strzero(WSEQ,6)

   if empty(WSEQ) .or. lastkey() == 27
      return .t.
   endif
 
   select PARTO
   ordsetfocus('SINC001')
   seek WSEQ

   if ! found()
      centro(24,"N"+Chr(132)+"o houve nenhum lan"+Chr(135)+"amento para esse animal...")
      inkey(5)
      @ 24,10 clear to 24,79
      loop
   endif

   @ 08,13 say DATA                           
   @ 10,13 say MAE       picture "@!"         

   @ 12,13 say BEZERRO   
   @ 13,13 say SEXO      picture "@!"          

   @ 15,13 say PAI       picture "@!"     

   @ 17,13 say RESENHA                    
   @ 19,13 say DESMAME                    

   @ 21,13 say OBS
 
   WMAE = MAE
   WPAI = PAI
   
   select ANIMAL
   ordsetfocus('SINC001')
   seek WMAE
 
   if found()
      @ 10,22 say NOME
   else
      @ 10,22 say space(30)
   endif

   seek WPAI
 
   if found()
      @ 15,22 say NOME
   else
      @ 15,22 say space(30)
   endif

   select PARTO

   If SEXO = "M"
      @ 13,14 Say "-Macho"
   Elseif SEXO = "F"
      @ 13,14 Say "-Femea"
   Endif

   if confirma(24,"Confirma EXCLUS"+chr(142)+"O...(S/N)?","N") == "S"
 
      if ! reglock()
         loop
      endif                         

      replace SEQ     With 0              ,;        
              DATA    With Ctod(Space(08)),;   
              MAE     With Space(06)      ,;        
              BEZERRO With Space(30)      ,;
              SEXO    With Space(01)      ,;
              PAI     With Space(06)      ,;
              RESENHA With Space(15)      ,;
              DESMAME With Ctod(Space(08)),;
              OBS     With Space(40)      ,;
              USUARIO With Space(20)      ,;
              DATE    With Ctod(space(08)),;
              TIME    With Space(08)

      delete

      grava()

   endif

Enddo

Return .T.


*******************
Function save_PARTO
*******************

Select PARTO

If ! REGLOCK()
   Return .F.
Endif
   
Replace SEQ     With WSEQ     ,;    
        DATA    With WDATA    ,;
        MAE     With WMAE     ,; 
        BEZERRO With WBEZERRO ,;
        SEXO    With WSEXO    ,;
        PAI     With WPAI     ,; 
        RESENHA With WRESENHA ,; 
        DESMAME With WDESMAME ,; 
        OBS     With WOBS     ,; 
        USUARIO With WPUSUARIO,;
        TIME    With TIME()   ,;
        DATE    With DATE()

GRAVA()

Return .T.


****************
Function SINTOMA
****************
OPC2 = 1

while .t.

   setcolor(COR_CAB) 
   @ 03,42 say "SINTOMAS CLINICOS/TRATAMENTOS      " 
   setcolor(COR_TEXT)

   private OPCAO[5]
   OPCAO[1] = " Inclus"+chr(132)+"o   " 
   OPCAO[2] = " Altera"+chr(135)+chr(132)+"o  " 
   OPCAO[3] = " Consulta   " 
   OPCAO[4] = " Exclus"+chr(132)+"o   " 
   OPCAO[5] = " Esc - Fim  " 

   private ACESSO[5]
   ACESSO[1] = .t.
   ACESSO[2] = .t.
   ACESSO[3] = .t.
   ACESSO[4] = .t.
   ACESSO[5] = .t.

   private LETRA[5]
   LETRA[1] = "I"  
   LETRA[2] = "A"
   LETRA[3] = "C"
   LETRA[4] = "E"
   LETRA[5] = "F"

   private POS[5] 
   POS[1] = 1
   POS[2] = 1
   POS[3] = 1
   POS[4] = 1
   POS[5] = 7

   private OBS_MENU[5] 
   OBS_MENU[1] = "Inclus"+chr(132)+"o de sintomas cl"+chr(161)+"nicos/tratamento..." 
   OBS_MENU[2] = "Altera"+chr(135)+chr(132)+"o de sintomas cl"+chr(161)+"nicos/tratamento..."      
   OBS_MENU[3] = "Consulta aos sintomas cl"+chr(161)+"nicos/tratamento..."                                                
   OBS_MENU[4] = "Exclus"+chr(132)+"o de sintomas cl"+chr(161)+"nicos/tratamento..." 
   OBS_MENU[5] = "Retorno ao menu principal..."                                                                                            

   OPC2 = achoise(OPC2,09,32,15,45,2,OPCAO,ACESSO,LETRA,POS,OBS_MENU,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)
   
   if OPC2 == 0 .or. OPC2 == 5
      return .t.
   elseif OPC2 == 1
      inc_sintoma()
   elseif OPC2 == 2
      alt_sintoma()
   elseif OPC2 == 3
      con_sintoma()
   elseif OPC2 == 4
      exc_sintoma()
   endif

enddo

Return .T.


*******************
Function SC_SINTOMA
*******************

@ 05,00 Clear To 22,79
@ 24,00 Clear To 24,79

@ 08,01 Say "Controle.:"
@ 08,40 Say "Data:"

@ 10,01 say "Animal...:        - " 

@ 12,01 say "Sintoma..:     - " 
@ 13,01 say "         :" 

@ 15,01 say "Tratament:"           
@ 16,01 say "         :" 
@ 17,01 say "         :" 

Return .T.


********************
Function inc_SINTOMA
********************

while .t. 

    select SINTOMA
    ordsetfocus('SINC001')

    WSEQ     = 0
    WDATA    = DATE()   
    WANIMAL  = space(06)     
    WSINTOMA = space(03)
    WCOMPLEM = space(60)   
    WTRATA1  = space(60)
    WTRATA2  = space(60)
    WTRATA3  = space(60)

    sc_SINTOMA()

    setcolor(COR_CONS)
    @ 24,00 say "INCLUS"+chr(142)+"O"
    @ 24,73 say "ESC-FIM"
    setcolor(COR_TEXT)

    select SINTOMA
    ordsetfocus('SINC001')
    go bottom

    WSEQ = SEQ + 1

    @ 08,12 Say Strzero(WSEQ,6)

    While .T.

        @ 08,46 get WDATA                                valid ! empty(WDATA)

        @ 10,12 get WANIMAL         picture "@!"         valid busca_ANIMAL()

        @ 12,12 get WSINTOMA        picture "@!"         valid busca_SINTOMA()
        @ 13,12 get WCOMPLEM

        @ 15,12 get WTRATA1
        @ 16,12 get WTRATA2
        @ 17,12 get WTRATA3

        select ANIMAL
        ordsetfocus('SINC001')
        seek WANIMAL
 
        if found()
           @ 10,21 say NOME
        else
           @ 10,21 say space(20)
        endif

        select CSINTOMA
        ordsetfocus('SINC001')
        seek WSINTOMA
 
        if found()
           @ 12,18 say SINTOMA
        else
           @ 12,18 say space(25)
        endif
  
        select SINTOMA

        READ_F()

        if lastkey() == 27
           return .t.
        endif

        if confirma(24,"Dados Corretos...(S/N)?","S") == "S"

           select SINTOMA
           ordsetfocus('SINC001')
           go bottom

           WSEQ = SEQ + 1

           set deleted off
           go top
           if deleted()
              if ! reglock()
                 loop
              endif
              recall
           else
              if ! inc_reg()
                 loop
              endif
           endif
           set deleted on

           save_SINTOMA()

           setcolor(COR_TEXT+"*+")
           @ 08,12 say WSEQ Picture "999999"
           setcolor(COR_TEXT)

           Inkey(5)

           Exit

        Endif

    Enddo

Enddo

Return .T.


********************
Function alt_SINTOMA
********************

while .t. 

   sc_SINTOMA()

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "ALTERA"+chr(128)+chr(142)+"O"
   setcolor(COR_TEXT)

   WSEQ = 0

   @ 08,12 get WSEQ picture "@Z 999999" 
   read_f()
   @ 08,12 Say Strzero(WSEQ,6)

   if empty(WSEQ) .or. lastkey() == 27
      return .t.
   endif

   select SINTOMA
   ordsetfocus('SINC001')
   seek WSEQ

   if ! found()
      @ 24,10 clear to 24,79
      centro(24,"N"+Chr(163)+"mero de controle n"+chr(132)+"o cadastrado...")
      inkey(5)
      @ 24,10 clear to 24,79
      loop
   endif

   WANIMAL  = ANIMAL      
   WDATA    = DATA        
   WSINTOMA = SINTOMA     
   WCOMPLEM = COMPLEM     
   WTRATA1  = TRATA1      
   WTRATA2  = TRATA2      
   WTRATA3  = TRATA3      

   while .t.

      @ 08,46 get WDATA                                valid ! empty(WDATA)

      @ 10,12 get WANIMAL         picture "@!"         valid busca_animal()
      
      @ 12,12 get WSINTOMA        picture "@!"         valid busca_sintoma()
      @ 13,12 get WCOMPLEM

      @ 15,12 get WTRATA1
      @ 16,12 get WTRATA2
      @ 17,12 get WTRATA3
      
      select ANIMAL
      ordsetfocus('SINC001')
      seek WANIMAL
      
      if found()
         @ 10,21 say NOME
      else
         @ 10,21 say space(20)
      endif

      select CSINTOMA
      ordsetfocus('SINC001')
      seek WSINTOMA
 
      if found()
         @ 12,18 say SINTOMA
      else
         @ 12,18 say space(25)
      endif
      
      select SINTOMA
      read_f()

      if lastkey() == 27
         return .t.
      endif

      if confirma(24,"Dados corretos...(S/N)?","S") == "S"
         save_SINTOMA()
         exit
      endif

   enddo

enddo

Return .T.


********************
Function con_SINTOMA
********************

Select SINTOMA

while .t.

   sc_SINTOMA()

   @ 05,00 clear to 22,79
   @ 24,00 clear to 24,79

   setcolor(COR_CONS)
   @ 24,00 say "CONSULTA"
   @ 24,73 say "ESC-FIM"
   setcolor(COR_TEXT)

   OPC3 = 5

   SOMBRA(10,14,18,67,8)

   setcolor(COR_CONS)
   @ 09,12 clear to 17,66
   @ 09,12 to 17,66 

   @ 10,14 say "Consulta a partir do in"+chr(161)+"cio do arquivo..........[1]"
   @ 11,14 say "Consulta a partir do final do arquivo...........[2]"
   @ 12,14 say "Consulta a partir da sequ"+Chr(136)+"ncia..................[3]"
   @ 13,14 say "Consulta a partir do animal.....................[4]"
   @ 14,14 say "Retorno ao menu principal.......................[5]"

   setcolor(COR_TEXT)
   @ 16,14 say "Escolha sua op"+chr(135)+chr(132)+"o de consulta...................[ ]"
   @ 16,63 get OPC3   picture "9"   range 1,5
   read_f()

   if OPC3 == 5
      return .t.
   endif

   while .t.

      sc_SINTOMA()

      if OPC3 == 1
         select SINTOMA
         ordsetfocus('SINC001')
         go top

      elseif OPC3 == 2
         select SINTOMA
         ordsetfocus('SINC001')
         go bottom

      elseif OPC3 == 3
         WSEQ  = 0          

         @ 08,12 get WSEQ picture "@Z 999999" 
         read_f()

         if lastkey() == 27
            exit
         endif

         select SINTOMA
         ordsetfocus('SINC001')
         seek WSEQ  

         if ! found()
            @ 24,10 clear to 24,79
            centro(24,"N"+Chr(163)+"mero de controle n"+chr(132)+"o cadastrado...")
            inkey(1)
            @ 24,10 clear to 24,79
            loop
         endif

      elseif OPC3 == 4

         WDATA   = ctod(space(08))
         WANIMAL = space(06)  

         @ 08,46 get WDATA                                
         @ 10,12 get WANIMAL picture "@!" valid busca_animal()
         read_f()

         if lastkey() == 27
            exit
         endif

         select SINTOMA
         ordsetfocus('SINC002')
         seek WANIMAL+dtos(WDATA)

      endif

      centro(24,"PgDw-Pr"+chr(162)+"ximo     PgUp-Anterior")

      KEY = 0

      while KEY  <> 27

         WANIMAL   = ANIMAL
         WSINTOMA  = SINTOMA

         @ 08,12 say Strzero(SEQ,6)
         
         @ 08,46 say DATA                               
         @ 10,12 say ANIMAL         picture "@!"     
         
         @ 12,12 say SINTOMA        picture "@!"
         @ 13,12 say COMPLEM

         @ 15,12 say TRATA1
         @ 16,12 say TRATA2
         @ 17,12 say TRATA3
         
         select ANIMAL
         ordsetfocus('SINC001')
         seek WANIMAL
         
         if found()
            @ 10,21 say NOME
         else
            @ 10,21 say space(20)
         endif

         select CSINTOMA
         ordsetfocus('SINC001')
         seek WSINTOMA
 
         if found()
            @ 12,18 say SINTOMA
         else
            @ 12,18 say space(25)
         endif
         
         select SINTOMA

         KEY = inkey(0)

         view_USER()

         if KEY == -7
            scalc()
         elseif KEY == -6
            calendario()
         elseif KEY == 3
            skip +1
            if eof()
               skip -1
            endif
         elseif KEY == 18
            skip -1
         elseif KEY == 27
            @ 24,10 clear to 24,70
            exit
         endif

      enddo

      if OPC3 == 1 .or. OPC3 == 2 
         exit
      endif

   enddo

enddo

Return .T.


********************
Function EXC_SINTOMA
********************

while .t. 

   WSEQ = 0

   sc_SINTOMA()

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "EXCLUS"+chr(142)+"O"
   setcolor(COR_TEXT)

   @ 08,12 get WSEQ picture "@Z 999999"
   read_f()

   if empty(WSEQ) .or. lastkey() == 27
      return .t.
   endif

   select SINTOMA
   ordsetfocus('SINC001')
   seek WSEQ

   if ! found()
      centro(24,"N"+Chr(163)+"mero de controle n"+chr(132)+"o cadastrado...")
      inkey(1)
      @ 24,10 clear to 24,79
      loop
   endif

   WANIMAL   = ANIMAL
   WSINTOMA  = SINTOMA

   @ 08,12 say Strzero(SEQ,6)
   
   @ 08,46 say DATA                               
   @ 10,12 say ANIMAL         picture "@!"
   
   @ 12,12 say SINTOMA        picture "@!"
   @ 13,12 say COMPLEM

   @ 15,12 say TRATA1
   @ 16,12 say TRATA2
   @ 17,12 say TRATA3
   
   select ANIMAL
   ordsetfocus('SINC001')
   seek WANIMAL
   
   if found()
      @ 10,21 say NOME
   else
      @ 10,21 say space(20)
   endif

   select CSINTOMA
   ordsetfocus('SINC001')
   seek WSINTOMA
 
   if found()
      @ 12,18 say SINTOMA
   else
      @ 12,18 say space(25)
   endif
   
   select SINTOMA

   if confirma(24,"Confirma EXCLUS"+chr(142)+"O...(S/N)?","N") == "S"

      if ! reglock()
         loop
      endif                         

      replace SEQ      with 0              ,;        
              ANIMAL   with space(06)      ,;        
              DATA     with ctod(space(08)),;   
              SINTOMA  with space(03)      ,;
              COMPLEM  with space(60)      ,;
              TRATA1   with space(60)      ,;
              TRATA2   with space(60)      ,;
              TRATA3   with space(60)      ,;
              USUARIO  with space(20)      ,;
              DATE     with ctod(space(08)),;
              TIME     with space(08)

      delete

      grava()
   endif

enddo

Return .T.


*********************
Function SAVE_SINTOMA
*********************

if ! reglock()
   return .f.
endif
   
replace SEQ     with WSEQ     ,;    
        ANIMAL  with WANIMAL  ,; 
        DATA    with WDATA    ,;   
        SINTOMA with WSINTOMA ,;
        COMPLEM with WCOMPLEM ,;
        TRATA1  with WTRATA1  ,; 
        TRATA2  with WTRATA2  ,;
        TRATA3  with WTRATA3  ,;
        USUARIO with WPUSUARIO,;
        TIME    with TIME()   ,;
        DATE    with DATE()

GRAVA()

Return .T.


*************
Function PESO
*************
OPC2 = 1

while .t.
   setcolor(COR_CAB) 
   @ 03,42 say "PESO E CONDI"+chr(128)+chr(142)+"O CORPOREA           " 
   setcolor(COR_TEXT)

   private OPCAO[5]
   OPCAO[1] = " Inclus"+chr(132)+"o   " 
   OPCAO[2] = " Altera"+chr(135)+chr(132)+"o  " 
   OPCAO[3] = " Consulta   " 
   OPCAO[4] = " Exclus"+chr(132)+"o   " 
   OPCAO[5] = " Esc - Fim  " 

   private ACESSO[5]
   ACESSO[1] = .t.
   ACESSO[2] = .t.
   ACESSO[3] = .t.
   ACESSO[4] = .t.
   ACESSO[5] = .t.

   private LETRA[5]
   LETRA[1] = "I"  
   LETRA[2] = "A"
   LETRA[3] = "C"
   LETRA[4] = "E"
   LETRA[5] = "F"

   private POS[5] 
   POS[1] = 1
   POS[2] = 1
   POS[3] = 1
   POS[4] = 1
   POS[5] = 7

   private OBS_MENU[5] 
   OBS_MENU[1] = "Inclus"+chr(132)+"o de peso dos animais..." 
   OBS_MENU[2] = "Altera"+chr(135)+chr(132)+"o de peso dos animais..."      
   OBS_MENU[3] = "Consulta de peso dos animais..."                                                
   OBS_MENU[4] = "Exclus"+chr(132)+"o de peso dos animais..." 
   OBS_MENU[5] = "Retorno ao menu principal..."                                                                                            

   OPC2 = achoise(OPC2,09,32,15,45,2,OPCAO,ACESSO,LETRA,POS,OBS_MENU,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)
   
   if OPC2 == 0 .or. OPC2 == 5
      return .t.
   elseif OPC2 == 1
      inc_PESO()
   elseif OPC2 == 2
      alt_PESO()
   elseif OPC2 == 3
      con_PESO()
   elseif OPC2 == 4
      exc_PESO()
   endif

Enddo

Return .T.


****************
Function SC_PESO
****************

@ 05,00 Clear To 22,79
@ 24,00 Clear To 24,79

@ 08,01 Say "Controle.:"
@ 10,01 Say "Data.....:"
@ 12,01 Say "Animal...:        - " 
@ 14,01 Say "Peso.....:" 
@ 16,01 Say "Condi"+Chr(135)+Chr(132)+"o.:" 

Return .T.


*****************
Function inc_PESO
*****************

while .t. 

    WSEQ      = 0
    WDATA     = date()   
    WANIMAL   = space(06)
    WPESO     = 0         
    WCONDICAO = Space(30)

    Select PESO
    ordsetfocus('SINC001')
    Go Bottom

    WSEQ = SEQ + 1

    sc_PESO()

    setcolor(COR_CONS)
    @ 24,00 say "INCLUS"+chr(142)+"O"
    @ 24,73 say "ESC-FIM"

    setcolor(COR_TEXT)
    @ 08,12 say Strzero(WSEQ,6)

    while .t. 

        setcolor(COR_TEXT)

        @ 10,12 get WDATA                          Valid ! Empty(WDATA)
        @ 12,12 get WANIMAL   Picture "999999"     Valid busca_ANIMAL()
        @ 14,12 get WPESO     Picture "@R@E 999.99 Kg" 
        @ 16,12 get WCONDICAO                                      

        select ANIMAL
        ordsetfocus('SINC001')
        seek WANIMAL
 
        if found()
           @ 12,21 say NOME
        else
           @ 12,21 say space(30)
        endif
  
        select PESO
        read_f()

        if lastkey() == 27
           return .t.
        endif

        if confirma(24,"Dados corretos...(S/N)?","S") == "S"

           select PESO
           ordsetfocus('SINC001')
           go bottom

           WSEQ = SEQ + 1

           set deleted off
           go top
           if deleted()
              if ! reglock()
                 loop
              endif
              recall
           else
              if ! inc_reg()
                 loop
              endif
           endif
           set deleted on

           save_PESO()

           setcolor(COR_TEXT+"*+")
           @ 08,12 say Strzero(WSEQ,6)
           setcolor(COR_TEXT)

           inkey(10)

           Exit

        Endif

   Enddo

Enddo

Return .T.


*****************
Function ALT_PESO
*****************

while .t. 

   sc_PESO()

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "ALTERA"+chr(128)+chr(142)+"O"
   setcolor(COR_TEXT)

   WSEQ = 0

   @ 08,12 get WSEQ picture "@Z 999999" 
   read_f()
   @ 08,12 say Strzero(WSEQ,6)

   if empty(WSEQ) .or. lastkey() == 27
      return .t.
   endif

   select PESO
   ordsetfocus('SINC001')
   seek WSEQ

   if ! found()
      centro(24,"N"+Chr(163)+"mero de controle n"+chr(132)+"o cadastrado...")
      inkey(5)
      @ 24,10 clear to 24,79
      loop
   endif

   WSEQ      = SEQ         
   WDATA     = DATA        
   WANIMAL   = ANIMAL      
   WPESO     = PESO 
   WCONDICAO = CONDICAO

   while .t.
      
      @ 10,12 get WDATA                         valid ! empty(WDATA)
      @ 12,12 get WANIMAL   picture "999999"    valid busca_animal()
      @ 14,12 get WPESO     picture "@R@E 999.99 Kg" 
      @ 16,12 get WCONDICAO 

      select ANIMAL
      ordsetfocus('SINC001')
      seek WANIMAL
      
      if found()
         @ 12,21 say NOME
      else
         @ 12,21 say space(20)
      endif
      
      select PESO
      read_f()

      if lastkey() == 27
         return .t.
      endif

      if confirma(24,"Dados corretos...(S/N)?","S") == "S"
         save_PESO()
         exit
      endif

   Enddo

Enddo

Return .T.


*****************
Function CON_PESO
*****************

while .t.

   OPC3 = 5

   sc_peso()

   setcolor(COR_CONS)
   @ 24,00 say "CONSULTA"
   @ 24,73 say "ESC-FIM"
   setcolor(COR_TEXT)

   SOMBRA(10,14,18,67,8)

   setcolor(COR_CONS)
   @ 09,12 clear to 17,66
   @ 09,12 to 17,66 

   @ 10,14 say "Consulta a partir do in"+chr(161)+"cio do arquivo..........[1]"
   @ 11,14 say "Consulta a partir do final do arquivo...........[2]"
   @ 12,14 say "Consulta a partir da n"+Chr(163)+"mero de controle.........[3]"
   @ 13,14 say "Consulta a partir do animal.....................[4]"
   @ 14,14 say "Retorno ao menu principal.......................[5]"

   setcolor(COR_TEXT)
   @ 16,14 say "Escolha sua op"+chr(135)+chr(132)+"o de consulta...................[ ]"
   @ 16,63 get OPC3 picture "9" range 1,5
   read_f()

   if OPC3 == 5
      return .t.
   endif

   while .t.

      sc_peso()

      if OPC3 == 1
         select PESO
         ordsetfocus('SINC001')
         go top

      elseif OPC3 == 2
         select PESO
         ordsetfocus('SINC001')
         go bottom

      elseif OPC3 == 3
         WSEQ  = 0          

         @ 08,12 get WSEQ picture "@Z 999999" 
         read_f()

         if lastkey() == 27
            exit
         endif

         select PESO
         ordsetfocus('SINC001')
         seek WSEQ  

         if ! found()
            @ 24,10 clear to 24,79
            centro(24,"N"+Chr(163)+"mero de controle n"+chr(132)+"o cadastrado...")
            inkey(5)
            @ 24,10 clear to 24,79
            loop
         endif

      elseif OPC3 == 4

         WDATA   = DATE()
         WANIMAL = space(06)          

         @ 10,12 get WDATA                                
         @ 12,12 get WANIMAL picture "@Z 999999" valid busca_animal()
         read_f()

         if lastkey() == 27
            exit
         endif

         select PESO
         ordsetfocus('SINC002')
         seek WANIMAL+dtos(WDATA)

         If EOF()
            Skip -1
            Loop
         Endif

      endif

      centro(24,"PgDw-Pr"+chr(162)+"ximo     PgUp-Anterior")

      KEY = 0

      while KEY  <> 27

         @ 08,12 say Strzero(SEQ,6)
         @ 10,12 say DATA                               
         @ 12,12 say ANIMAL         picture "999999"
         @ 14,12 say PESO           picture "@R@E 999.99 Kg" 
         @ 16,12 say CONDICAO                    

         WANIMAL = ANIMAL

         select ANIMAL
         ordsetfocus('SINC001')
         seek WANIMAL
         
         if found()
            @ 12,21 say NOME
         else
            @ 12,21 say space(30)
         endif
         
         select PESO

         KEY = inkey(0)

         view_user()

         if KEY == -7
            scalc()
         elseif KEY == -6
            calendario()
         elseif KEY == 3
            skip +1
            if eof()
               skip -1
            endif
         elseif KEY == 18
            skip -1
         elseif KEY == 27
            @ 24,10 clear to 24,70
            exit
         endif
      enddo

      if OPC3 == 1 .or. OPC3 == 2 
         exit
      endif

   Enddo

Enddo

Return .T.


*****************
Function EXC_PESO
*****************

while .t. 

   WSEQ = 0

   sc_PESO()

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "EXCLUS"+chr(142)+"O"
   setcolor(COR_TEXT)

   @ 08,12 get WSEQ picture "@Z 999999"
   read_f()
   @ 08,12 Say Strzero(WSEQ,6)

   if empty(WSEQ) .or. lastkey() == 27
      return .t.
   endif

   select PESO
   ordsetfocus('SINC001')
   seek WSEQ

   if ! found()
      @ 24,10 clear to 24,79
      centro(24,"N"+Chr(163)+"mero de controle n"+chr(132)+"o cadastrado...")
      inkey(5)
      @ 24,10 clear to 24,79
      loop
   endif

   @ 10,12 say DATA                               
   @ 12,12 say ANIMAL   picture "999999"
   @ 14,12 say PESO     picture "@R@E 999.99 Kg" 
   @ 16,12 say CONDICAO  

   WANIMAL = ANIMAL
   
   select ANIMAL
   ordsetfocus('SINC001')
   seek WANIMAL
   
   if found()
      @ 12,21 say NOME
   else
      @ 12,21 say space(30)
   endif
   
   select PESO

   if confirma(24,"Confirma EXCLUS"+chr(142)+"O...(S/N)?","N") == "S"

      if ! reglock()
         loop
      endif                         

      replace SEQ      with 0              ,;        
              DATA     with ctod(space(08)),;   
              ANIMAL   with space(06)      ,;        
              PESO     with 0              ,;         
              CONDICAO with Space(30)      ,;
              USUARIO  with space(20)      ,;
              DATE     with ctod(space(08)),;
              TIME     with space(08)

      delete

      grava()

   endif

Enddo

Return .T.


******************
Function save_PESO
******************

if ! reglock()
   return .f.
endif
   
replace SEQ      With WSEQ     ,;    
        ANIMAL   With WANIMAL  ,; 
        DATA     With WDATA    ,;   
        PESO     With WPESO    ,;             
        CONDICAO With WCONDICAO,;
        USUARIO  With WPUSUARIO,;
        TIME     With TIME()   ,;
        DATE     With DATE()

GRAVA()

Return .T.


***************
Function VACINA
***************
OPC2 = 1

while .t.

   setcolor(COR_CAB) 
   @ 03,42 say "VACINA"+chr(128)+chr(148)+"ES                         " 
   setcolor(COR_TEXT)

   private OPCAO[5]
   OPCAO[1] = " Inclus"+chr(132)+"o   " 
   OPCAO[2] = " Altera"+chr(135)+chr(132)+"o  " 
   OPCAO[3] = " Consulta   " 
   OPCAO[4] = " Exclus"+chr(132)+"o   " 
   OPCAO[5] = " Esc - Fim  " 

   private ACESSO[5]
   ACESSO[1] = .t.
   ACESSO[2] = .t.
   ACESSO[3] = .t.
   ACESSO[4] = .t.
   ACESSO[5] = .t.

   private LETRA[5]
   LETRA[1] = "I"  
   LETRA[2] = "A"
   LETRA[3] = "C"
   LETRA[4] = "E"
   LETRA[5] = "F"

   private POS[5] 
   POS[1] = 1
   POS[2] = 1
   POS[3] = 1
   POS[4] = 1
   POS[5] = 7

   private OBS_MENU[5] 
   OBS_MENU[1] = "Inclus"+chr(132)+"o de vacina"+chr(135)+chr(148)+"es..." 
   OBS_MENU[2] = "Altera"+chr(135)+chr(132)+"o de vacina"+chr(135)+chr(148)+"es..."      
   OBS_MENU[3] = "Consulta as vacina"+chr(135)+chr(148)+"es..."                                      
   OBS_MENU[4] = "Exclus"+chr(132)+"o de vacina"+chr(135)+chr(148)+"es..." 
   OBS_MENU[5] = "Retorno ao menu principal..."                                                                                            

   OPC2 = achoise(OPC2,09,32,15,45,2,OPCAO,ACESSO,LETRA,POS,OBS_MENU,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)
   
   if OPC2 == 0 .or. OPC2 == 5
      return .t.
   elseif OPC2 == 1
      inc_VACINA()
   elseif OPC2 == 2
      alt_VACINA()
   elseif OPC2 == 3
      con_VACINA()
   elseif OPC2 == 4
      exc_VACINA()
   endif

Enddo

Return .T.


******************
Function SC_VACINA
******************

@ 05,00 clear to 22,79
@ 24,00 clear to 24,79

@ 08,01 say "Controle.:"
@ 10,01 say "Data.....:"
@ 12,01 say "Animal...:        - " 
@ 14,01 say "Vacina...:     - "
@ 16,01 say "Obs......:"
@ 17,01 say "         :"

Return .T.


*******************
Function INC_VACINA
*******************

while .t. 

    sc_vacina()

    setcolor(COR_CONS)
    @ 24,00 say "INCLUS"+chr(142)+"O"
    @ 24,73 say "ESC-FIM"

    WSEQ    = 0
    WDATA   = date()   
    WANIMAL = space(06)
    WVACINA = space(03)   
    WOBS1   = space(60)
    WOBS2   = space(60)

    select VACINA
    ordsetfocus('SINC001')
    Go Bottom

    WSEQ = SEQ + 1

    setcolor(COR_TEXT)
    @ 08,12 say Strzero(WSEQ,6)

    while .t. 

        @ 10,12 get WDATA                         valid ! empty(WDATA)
        @ 12,12 get WANIMAL  picture "999999"     valid busca_animal()
        @ 14,12 get WVACINA  picture "@!"         valid busca_vacina()
        @ 16,12 get WOBS1
        @ 17,12 get WOBS2

        select ANIMAL
        ordsetfocus('SINC001')
        seek WANIMAL
 
        if found()
           @ 12,21 say NOME
        else
           @ 12,21 say space(30)
        endif

        select CVACINA
        ordsetfocus('SINC001')
        seek WVACINA
 
        if found()
           @ 14,18 say VACINA
        else
           @ 14,18 say space(25)
        endif
  
        select VACINA
        read_f()

        if lastkey() == 27
           return .t.
        endif

        if confirma(24,"Dados Corretos...(S/N)?","S") == "S"

           select VACINA
           ordsetfocus('SINC001')
           go bottom

           WSEQ = SEQ + 1

           set deleted off
           go top
           if deleted()
              if ! reglock()
                 loop
              endif
              recall
           else
              if ! inc_reg()
                 loop
              endif
           endif
           set deleted on

           save_VACINA()

           setcolor(COR_TEXT+"*+")
           @ 08,12 say Strzero(WSEQ,6)
           setcolor(COR_TEXT)

           inkey(5)

           Exit

        Endif

    Enddo

Enddo

Return .T.


*******************
Function ALT_VACINA
*******************

while .t. 

   sc_vacina()

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "ALTERA"+chr(128)+chr(142)+"O"
   setcolor(COR_TEXT)

   WSEQ = 0

   @ 08,12 get WSEQ picture "@Z 999999" 
   read_f()
   @ 08,12 Say Strzero(WSEQ,6)

   if empty(WSEQ) .or. lastkey() == 27
      return .t.
   endif

   select VACINA
   ordsetfocus('SINC001')
   seek WSEQ

   if ! found()
      @ 24,10 clear to 24,79
      centro(24,"N"+Chr(163)+"mero de controle n"+chr(132)+"o cadastrado...")
      inkey(5)
      @ 24,10 clear to 24,79
      loop
   endif

   WDATA   = DATA        
   WANIMAL = ANIMAL      
   WVACINA = VACINA
   WOBS1   = OBS1
   WOBS2   = OBS2

   while .t.

      setcolor(COR_TEXT)

      @ 10,12 get WDATA                                valid ! empty(WDATA)
      @ 12,12 get WANIMAL         picture "999999"     valid busca_animal()
      @ 14,12 get WVACINA         picture "@!"         valid busca_vacina()
      @ 16,12 get WOBS1
      @ 17,12 get WOBS2

      select ANIMAL
      ordsetfocus('SINC001')
      seek WANIMAL
      
      if found()
         @ 12,21 say NOME
      else
         @ 12,21 say space(30)
      endif

      select CVACINA
      ordsetfocus('SINC001')
      seek WVACINA
 
      if found()
         @ 14,18 say VACINA
      else
         @ 14,18 say space(25)
      endif
      
      select VACINA
      read_f()

      if lastkey() == 27
         return .t.
      endif

      if confirma(24,"Dados corretos...(S/N)?","S") == "S"
         save_vacina()
         exit
      endif

   enddo

Enddo

Return .T.


*******************
Function CON_VACINA
*******************

while .t.

   OPC3 = 5

   sc_vacina()

   setcolor(COR_CONS)
   @ 24,00 say "CONSULTA"
   @ 24,73 say "ESC-FIM"
   setcolor(COR_TEXT)

   setcolor(COR_CONS)
   @ 09,12 clear to 17,66
   @ 09,12 to 17,66 

   @ 10,14 say "Consulta a partir do in"+chr(161)+"cio do arquivo..........[1]"
   @ 11,14 say "Consulta a partir do final do arquivo...........[2]"
   @ 12,14 say "Consulta a partir do n"+Chr(163)+"mero de controle.........[3]"
   @ 13,14 say "Consulta a partir do animal.....................[4]"
   @ 14,14 say "Retorno ao menu principal.......................[5]"

   setcolor(COR_TEXT)
   @ 16,14 say "Escolha sua op"+chr(135)+chr(132)+"o de consulta...................[ ]"
   @ 16,63 get OPC3 picture "9" range 1,5
   read_f()

   if OPC3 == 5
      return .t.
   endif

   while .t.

      sc_vacina()

      if OPC3 == 1
         select VACINA
         ordsetfocus('SINC001')
         go top

      elseif OPC3 == 2
         select VACINA
         ordsetfocus('SINC001')
         go bottom

      elseif OPC3 == 3
         WSEQ = 0          

         @ 08,12 get WSEQ picture "@Z 999999" 
         read_f()

         if lastkey() == 27
            exit
         endif

         select VACINA
         ordsetfocus('SINC001')
         seek WSEQ  

         if ! found()
            @ 24,10 clear to 24,79
            centro(24,"N"+Chr(163)+"mero de controle n"+chr(132)+"o cadastrado...")
            inkey(5)
            @ 24,10 clear to 24,79
            loop
         endif

      elseif OPC3 == 4

         WDATA   = DATE()                
         WANIMAL = space(06)          

         @ 10,12 get WDATA                                
         @ 12,12 get WANIMAL picture "999999"  valid busca_animal()
         read_f()

         if lastkey() == 27
            exit
         endif

         select VACINA
         ordsetfocus('SINC002')
         seek WANIMAL+dtos(WDATA)

         If EOF()
            Skip -1
            Loop
         Endif

      endif

      centro(24,"PgDw-Pr"+chr(162)+"ximo     PgUp-Anterior")

      KEY = 0

      while KEY  <> 27

         @ 08,12 say Strzero(SEQ,6)
         @ 10,12 say DATA                               
         @ 12,12 say ANIMAL         picture "@!"     
         @ 14,12 say VACINA         picture "@!"
         @ 16,12 say OBS1
         @ 17,12 say OBS2

         WANIMAL   = ANIMAL
         WVACINA   = VACINA

         select ANIMAL
         ordsetfocus('SINC001')
         seek WANIMAL
         
         if found()
            @ 12,21 say NOME
         else
            @ 12,21 say space(30)
         endif

         select CVACINA
         ordsetfocus('SINC001')
         seek WVACINA
 
         if found()
            @ 14,18 say VACINA
         else
            @ 14,18 say space(25)
         endif
  
         select VACINA

         KEY = inkey(0)

         view_user()

         if KEY == -7
            scalc()
         elseif KEY == -6
            calendario()
         elseif KEY == 3
            skip +1
            if eof()
               skip -1
            endif
         elseif KEY == 18
            skip -1
         elseif KEY == 27
            @ 24,10 clear to 24,70
            exit
         endif
      enddo

      if OPC3 == 1 .or. OPC3 == 2 
         exit
      endif

   Enddo

Enddo

Return .T.


*******************
Function EXC_VACINA
*******************

while .t. 

   sc_vacina()

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "EXCLUS"+chr(142)+"O"
   setcolor(COR_TEXT)

   WSEQ = 0

   @ 08,12 get WSEQ picture "@Z 999999"
   read_f()
   @ 08,12 Say Strzero(WSEQ,6)

   if empty(WSEQ) .or. lastkey() == 27
      return .t.
   endif

   select VACINA
   ordsetfocus('SINC001')
   seek WSEQ

   if ! found()
      @ 24,10 clear to 24,79
      centro(24,"N"+Chr(163)+"mero de controle n"+chr(132)+"o cadastrado...")
      inkey(5)
      @ 24,10 clear to 24,79
      loop
   endif

   @ 10,12 say DATA                               
   @ 12,12 say ANIMAL         picture "@!"     
   @ 14,12 say VACINA         picture "@!"
   @ 16,12 say OBS1
   @ 17,12 say OBS2

   WANIMAL = ANIMAL
   WVACINA = VACINA

   select ANIMAL
   ordsetfocus('SINC001')
   seek WANIMAL
   
   if found()
      @ 12,21 say NOME
   else
      @ 12,21 say space(35)
   endif

   select CVACINA
   ordsetfocus('SINC001')
   seek WVACINA
 
   if found()
      @ 14,18 say VACINA
   else
      @ 14,18 say space(20)
   endif
  
   select VACINA

   if confirma(24,"Confirma EXCLUS"+chr(142)+"O...(S/N)?","N") == "S"

      if ! reglock()
         loop
      endif                         

      replace SEQ     with 0              ,;        
              DATA    with ctod(space(08)),;   
              ANIMAL  with space(06)      ,;        
              VACINA  with space(03)      ,;
              OBS1    with space(60)      ,;
              OBS2    with space(60)      ,;
              USUARIO with space(20)      ,;
              DATE    with ctod(space(08)),;
              TIME    with space(08)

      delete

      grava()

   endif

Enddo

Return .T.

********************
Function SAVE_VACINA
********************

if ! reglock()
   return .f.
endif
   
replace SEQ     with WSEQ     ,;    
        ANIMAL  with WANIMAL  ,; 
        DATA    with WDATA    ,;   
        VACINA  with WVACINA  ,;
        OBS1    with WOBS1    ,;
        OBS2    with WOBS2    ,;  
        USUARIO with WPUSUARIO,;
        TIME    with TIME()   ,;
        DATE    with DATE()

GRAVA()

Return .T.


****************
Function cad_DIV
****************
OPC2 = 1

while .t.

   setcolor(COR_CAB)   
   @ 03,42 say "CADASTRO DIVERSOS                  " 
   setcolor(COR_TEXT)

   private OPCAO[9]
   OPCAO[1] = " Sintomas   " 
   OPCAO[2] = " Vacinas    " 
   OPCAO[3] = " Ra"+chr(135)+"a       " 
   OPCAO[4] = " Categorias " 
   OPCAO[5] = " Pasto      " 
   OPCAO[6] = " Pend"+Chr(136)+"ncias " 
   OPCAO[7] = " Grupos     " 
   OPCAO[8] = " Sub-Grupo  " 
   OPCAO[9] = " Esc - Fim  " 

   private ACESSO[9]
   ACESSO[1] = .T.
   ACESSO[2] = .T.
   ACESSO[3] = .T.
   ACESSO[4] = .T.
   ACESSO[5] = .T.
   ACESSO[6] = .T.
   ACESSO[7] = .T.
   ACESSO[8] = .T.
   ACESSO[9] = .T.

   private LETRA[9]
   LETRA[1] = "S"  
   LETRA[2] = "V"  
   LETRA[3] = "R"  
   LETRA[4] = "C"  
   LETRA[5] = "P"  
   LETRA[6] = "E"  
   LETRA[7] = "G"  
   LETRA[8] = "U"  
   LETRA[9] = "F"  

   private POS[9]
   POS[1] = 1
   POS[2] = 1 
   POS[3] = 1 
   POS[4] = 1 
   POS[5] = 1 
   POS[6] = 2
   POS[7] = 1
   POS[8] = 2
   POS[9] = 7
 
   private OBS_MENU[9]
   OBS_MENU[1] = "Cadastro de sintomas!"        
   OBS_MENU[2] = "Cadastro de vacinas!"        
   OBS_MENU[3] = "Cadastro de ra"+chr(135)+"as!"        
   OBS_MENU[4] = "Cadastro de categorias!"        
   OBS_MENU[5] = "Cadastro de pastos!"        
   OBS_MENU[6] = "Cadastro de pend"+Chr(136)+"ncias!"                  
   OBS_MENU[7] = "Cadastro de grupos!"                  
   OBS_MENU[8] = "Cadastro de sub-grupo!"            
   OBS_MENU[9] = "Retornar ao menu principal!"                  
 
   OPC2 = ACHOISE(OPC2,08,32,18,45,1,OPCAO,ACESSO,LETRA,POS,OBS_MENU,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)

   If OPC2 == 0 .or. OPC2 == 9
      return .t.
   Elseif OPC2 == 1
      csintoma()
   Elseif OPC2 == 2
      cvacina()
   Elseif OPC2 == 3
      raca()
   Elseif OPC2 == 4
      categ()
   Elseif OPC2 == 5
      pasto()
   Elseif OPC2 == 6
      PENDENCIAS()
   Elseif OPC2 == 7
      GRUPOS()
   Elseif OPC2 == 8
      LOTES()
   Endif

Enddo

Return .T.


*****************
Function CSINTOMA
*****************
OPC3 = 1

while .t.

   setcolor(COR_CAB) 
   @ 03,42 say "CADASTRO DE SINTOMAS               " 
   setcolor(COR_TEXT)

   private OPCAO[5]
   OPCAO[1] = " Inclus"+chr(132)+"o   " 
   OPCAO[2] = " Altera"+chr(135)+chr(132)+"o  " 
   OPCAO[3] = " Consulta   " 
   OPCAO[4] = " Exclus"+chr(132)+"o   " 
   OPCAO[5] = " Esc - Fim  " 

   private ACESSO[5]
   ACESSO[1] = .t.
   ACESSO[2] = .t.
   ACESSO[3] = .t.
   ACESSO[4] = .t.
   ACESSO[5] = .t.

   private LETRA[5]
   LETRA[1] = "I"  
   LETRA[2] = "A"
   LETRA[3] = "C"
   LETRA[4] = "E"
   LETRA[5] = "F"

   private POS[5] 
   POS[1] = 1
   POS[2] = 1
   POS[3] = 1
   POS[4] = 1
   POS[5] = 7

   private OBS_MENU[5] 
   OBS_MENU[1] = "Inclus"+chr(132)+"o de sintomas..." 
   OBS_MENU[2] = "Altera"+chr(135)+chr(132)+"o de sintomas..."      
   OBS_MENU[3] = "Consulta aos sintomas..."                                                
   OBS_MENU[4] = "Exclus"+chr(132)+"o de sintomas..." 
   OBS_MENU[5] = "Retorno ao menu principal..."                                                                                            

   OPC3 = achoise(OPC3,10,32,16,45,2,OPCAO,ACESSO,LETRA,POS,OBS_MENU,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)
   
   if OPC3 == 0 .or. OPC3 == 5
      return .t.
   elseif OPC3 == 1
      inc_csintoma()
   elseif OPC3 == 2
      alt_csintoma()
   elseif OPC3 == 3
      con_csintoma()
   elseif OPC3 == 4
      exc_csintoma()
   endif

Enddo

Return .T.

********************
Function SC_CSINTOMA
********************

@ 05,00 clear to 22,79
@ 24,00 clear to 24,79

@ 08,01 say "C"+chr(162)+"digo...:"
@ 10,01 say "Sintoma..:" 

Return .T.


*********************
function INC_CSINTOMA
*********************

select CSINTOMA
ordsetfocus('SINC001')

WCODIGO  = space(03)
WSINTOMA = space(25)

while .t. 

   sc_csintoma()

   setcolor(COR_CONS)
   @ 24,00 say "INCLUS"+chr(142)+"O"
   @ 24,73 say "ESC-FIM"
   setcolor(COR_TEXT)

   @ 08,12 get WCODIGO picture "@!" valid chck_csintoma()
   @ 10,12 get WSINTOMA
   read_f()

   if lastkey() == 27
      return .t.
   endif

   if confirma(24,"Dados Corretos...(S/N)?","S") == "S"
      select CSINTOMA
      ordsetfocus('SINC001')

      set deleted off
      go top
      if deleted()
         if ! reglock()
            loop
         endif

         recall
      else
         if ! inc_reg()
            loop
         endif
      endif
      set deleted on

      save_csintoma()

      WCODIGO  = space(03)
      WSINTOMA = space(25)

   endif

enddo

return .t.


*********************
function ALT_CSINTOMA
*********************

while .t. 

   sc_csintoma()

   WCODIGO = space(03)

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "ALTERA"+chr(128)+chr(142)+"O"
   setcolor(COR_TEXT)

   @ 08,12 get WCODIGO picture "@!" 
   read_f()

   if empty(WCODIGO) .or. lastkey() == 27
      return .t.
   endif

   select CSINTOMA
   ordsetfocus('SINC001')
   seek upper(WCODIGO)

   if ! found()
      @ 24,10 clear to 24,79
      centro(24,"C"+chr(162)+"digo n"+chr(132)+"o cadastrado...")
      inkey(1)
      @ 24,10 clear to 24,79
      loop
   endif

   WCODIGO  = CODIGO      
   WSINTOMA = SINTOMA     

   while .t.

      sc_csintoma()

      setcolor(COR_CONS)
      @ 24,00 say "ALTERA"+chr(138)+chr(142)+"O"
      @ 24,73 say "ESC-FIM"
      setcolor(COR_TEXT)

      @ 08,12 say WCODIGO picture "@!"
      @ 10,12 get WSINTOMA
      read_f()

      if lastkey() == 27
         return .t.
      endif

      if confirma(24,"Dados corretos...(S/N)?","S") == "S"
         save_csintoma()
         exit
      endif

   enddo

enddo

return .t.


*********************
function CON_CSINTOMA
*********************

select CSINTOMA

while .t.

   @ 05,00 clear to 22,79
   @ 24,00 clear to 24,79

   setcolor(COR_CONS)
   @ 24,00 say "CONSULTA"
   @ 24,73 say "ESC-FIM"
   setcolor(COR_TEXT)

   OPC4 = 5

   SOMBRA(10,14,18,67,8)

   setcolor(COR_CONS)
   @ 09,12 clear to 17,66
   @ 09,12 to 17,66 

   @ 10,14 say "Consulta a partir do in"+chr(161)+"cio do arquivo..........[1]"
   @ 11,14 say "Consulta a partir do final do arquivo...........[2]"
   @ 12,14 say "Consulta a partir do c"+chr(162)+"digo.....................[3]"
   @ 13,14 say "Consulta a partir do sintoma....................[4]"
   @ 14,14 say "Retorno ao menu principal.......................[5]"

   setcolor(COR_TEXT)
   @ 16,14 say "Escolha sua op"+chr(135)+chr(132)+"o de consulta...................[ ]"
   @ 16,63 get OPC4 picture "9" range 1,5
   read_f()

   if OPC4 == 5
      return .t.
   endif

   do while .t.
      sc_csintoma()

      if OPC4 == 1
         select CSINTOMA
         ordsetfocus('SINC001')
         go top

      elseif OPC4 == 2
         select CSINTOMA
         ordsetfocus('SINC001')
         go bottom

      elseif OPC4 == 3
         WCODIGO  = space(03)          

         @ 08,12 get WCODIGO picture "@!"
         read_f()

         if lastkey() == 27
            exit
         endif

         select CSINTOMA
         ordsetfocus('SINC001')
         seek upper(WCODIGO)

         if ! found()
            @ 24,10 clear to 24,79
            centro(24,"C"+chr(162)+"digo n"+chr(132)+"o cadastrado...")
            inkey(1)
            @ 24,10 clear to 24,79
            loop
         endif

      elseif OPC4 == 4
         WSINTOMA = space(25)

         @ 10,12 get WSINTOMA
         read_f()

         if lastkey() == 27
            exit
         endif

         select CSINTOMA
         ordsetfocus('SINC002')
         seek upper(WSINTOMA)
      endif

      centro(24,"PgDw-Pr"+chr(162)+"ximo   PgUp-Anterior")

      KEY = 0

      while KEY  <> 27

         @ 08,12 say CODIGO picture "@!"
         @ 10,12 say SINTOMA

         KEY = inkey(0)

         view_user()

         if KEY == -7
            scalc()
         elseif KEY == -6
            calendario()
         elseif KEY == 3
            skip +1
            if eof()
               skip -1
            endif
         elseif KEY == 18
            skip -1
         elseif KEY == 27
            @ 24,10 clear to 24,70
            exit
         endif
      enddo

      if OPC4 == 1 .or. OPC4 == 2 
         exit
      endif
   enddo

enddo

return .t.

*********************
function EXC_CSINTOMA
*********************

while .t. 

   sc_csintoma()

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "EXCLUS"+chr(142)+"O"
   setcolor(COR_TEXT)

   WCODIGO = space(03)

   @ 08,12 get WCODIGO picture "@!"
   read_f()

   if empty(WCODIGO) .or. lastkey() == 27
      return .t.
   endif

   select CSINTOMA
   ordsetfocus('SINC001')
   seek upper(WCODIGO)

   if ! found()
      @ 24,10 clear to 24,79
      centro(24,"C"+chr(162)+"digo n"+chr(132)+"o cadastrado...")
      inkey(1)
      @ 24,10 clear to 24,79
      loop
   endif

   @ 08,12 say CODIGO picture "@!"
   @ 10,12 say SINTOMA

   if confirma(24,"Confirma EXCLUS"+chr(142)+"O...(S/N)?","N") == "S"
      if ! reglock()
         loop
      endif                         

      replace CODIGO  with space(03)      ,;
              SINTOMA with space(25)      ,;
              USUARIO with space(20)      ,;
              DATE    with ctod(space(08)),;
              TIME    with space(08)

      delete

      grava()
   endif
enddo

return .t.


**********************
function CHCK_CSINTOMA
**********************

select CSINTOMA
ordsetfocus('SINC001')
seek upper(WCODIGO)

if found()
   setcursor(0)
   @ 24,10 clear to 24,70
   centro(24,"C"+chr(162)+"digo de sintoma j"+chr(160)+" cadastrado...")
   inkey(5)
   @ 24,10 clear to 24,70
   setcursor(1)
   return .f.
endif

return .t.


**********************
function SAVE_CSINTOMA
**********************

if ! reglock()
   return .f.
endif
   
replace CODIGO   with WCODIGO  ,;    
        SINTOMA  with WSINTOMA ,;
        USUARIO  with WPUSUARIO,;
        TIME     with TIME()   ,;
        DATE     with DATE()

grava()

return .t.


****************
Function CVACINA
****************
OPC3 = 1

while .t.

   setcolor(COR_CAB) 
   @ 03,42 say "CADASTRO DE VACINAS                " 
   setcolor(COR_TEXT)

   private OPCAO[5]
   OPCAO[1] = " Inclus"+chr(132)+"o   " 
   OPCAO[2] = " Altera"+chr(135)+chr(132)+"o  " 
   OPCAO[3] = " Consulta   " 
   OPCAO[4] = " Exclus"+chr(132)+"o   " 
   OPCAO[5] = " Esc - Fim  " 

   private ACESSO[5]
   ACESSO[1] = .t.
   ACESSO[2] = .t.
   ACESSO[3] = .t.
   ACESSO[4] = .t.
   ACESSO[5] = .t.

   private LETRA[5]
   LETRA[1] = "I"  
   LETRA[2] = "A"
   LETRA[3] = "C"
   LETRA[4] = "E"
   LETRA[5] = "F"

   private POS[5] 
   POS[1] = 1
   POS[2] = 1
   POS[3] = 1
   POS[4] = 1
   POS[5] = 7

   private OBS_MENU[5] 
   OBS_MENU[1] = "Inclus"+chr(132)+"o de vacinas..." 
   OBS_MENU[2] = "Altera"+chr(135)+chr(132)+"o de vacinas..."      
   OBS_MENU[3] = "Consulta as vacinas..."                                                
   OBS_MENU[4] = "Exclus"+chr(132)+"o de vacinas..." 
   OBS_MENU[5] = "Retorno ao menu principal..."                                                                                            

   OPC3 = achoise(OPC3,10,32,16,45,2,OPCAO,ACESSO,LETRA,POS,OBS_MENU,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)
   
   if OPC3 == 0 .or. OPC3 == 5
      return .t.
   elseif OPC3 == 1
      inc_cvacina()
   elseif OPC3 == 2
      alt_cvacina()
   elseif OPC3 == 3
      con_cvacina()
   elseif OPC3 == 4
      exc_cvacina()
   endif

Enddo

Return .T.

*******************
Function SC_CVACINA
*******************

@ 05,00 clear to 22,79
@ 24,00 clear to 24,79

@ 08,01 say "C"+chr(162)+"digo...:"
@ 10,01 say "Vacina...:" 
@ 12,01 say "Frequenc.:"

return .t.

********************
function INC_CVACINA
********************

select CVACINA
ordsetfocus('SINC001')

WCODIGO     = space(03)
WVACINA     = space(25)
WFREQUENCIA = 0

while .t. 

   sc_cvacina()

   setcolor(COR_CONS)
   @ 24,00 say "INCLUS"+chr(142)+"O"
   @ 24,73 say "ESC-FIM"
   setcolor(COR_TEXT)

   @ 08,12 get WCODIGO         picture "@!"         valid chck_cvacina()
   @ 10,12 get WVACINA         
   @ 12,12 get WFREQUENCIA     picture "@R@E 9999dd" 
   read_f()

   if lastkey() == 27
      return .t.
   endif

   if confirma(24,"Dados Corretos...(S/N)?","S") == "S"

      select CVACINA
      ordsetfocus('SINC001')

      set deleted off
      go top
      if deleted()
         if ! reglock()
            loop
         endif

         recall
      else
         if ! inc_reg()
            loop
         endif
      endif
      set deleted on

      save_cvacina()

      WCODIGO      = space(03)
      WVACINA      = space(25)
      WFREQUENCIA  = 0

   endif

enddo

return .t.


********************
function ALT_CVACINA
********************

while .t. 

   sc_cvacina()

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "ALTERA"+chr(128)+chr(142)+"O"
   setcolor(COR_TEXT)

   WCODIGO = space(03)

   @ 08,12 get WCODIGO picture "@!" 
   read_f()

   if empty(WCODIGO) .or. lastkey() == 27
      return .t.
   endif

   select CVACINA
   ordsetfocus('SINC001')
   seek upper(WCODIGO)

   if ! found()
      @ 24,10 clear to 24,79
      centro(24,"C"+chr(162)+"digo n"+chr(132)+"o cadastrado...")
      inkey(1)
      @ 24,10 clear to 24,79
      loop
   endif

   WCODIGO      = CODIGO      
   WVACINA      = VACINA     
   WFREQUENCIA  = FREQUENCIA

   while .t.

      sc_cvacina()

      setcolor(COR_CONS)
      @ 24,00 say "ALTERA"+chr(138)+chr(142)+"O"
      @ 24,73 say "ESC-FIM"
      setcolor(COR_TEXT)

      @ 08,12 say WCODIGO     picture "@!"
      @ 10,12 get WVACINA
      @ 12,12 get WFREQUENCIA picture "@R@E 9999dd" 
      read_f()

      if lastkey() == 27
         return .t.
      endif

      if confirma(24,"Dados Corretos...(S/N)?","S") == "S"
         save_cvacina()
         exit
      endif

   enddo

enddo

return .t.

********************
function CON_CVACINA
********************

while .t.

   OPC4 = 5

   @ 05,00 clear to 22,79
   @ 24,00 clear to 24,79

   setcolor(COR_CONS)
   @ 24,00 say "CONSULTA"
   @ 24,73 say "ESC-FIM"
   setcolor(COR_TEXT)

   SOMBRA(10,14,18,67,8)

   setcolor(COR_CONS)
   @ 09,12 clear to 17,66
   @ 09,12 to 17,66 

   @ 10,14 say "Consulta a partir do in"+chr(161)+"cio do arquivo..........[1]"
   @ 11,14 say "Consulta a partir do final do arquivo...........[2]"
   @ 12,14 say "Consulta a partir do c"+chr(162)+"digo.....................[3]"
   @ 13,14 say "Consulta a partir do vacina.....................[4]"
   @ 14,14 say "Retorno ao menu principal.......................[5]"

   setcolor(COR_TEXT)
   @ 16,14 say "Escolha sua op"+chr(135)+chr(132)+"o de consulta...................[ ]"
   @ 16,63 get OPC4 picture "9" range 1,5
   read_f()

   if OPC4 == 5
      return .t.
   endif

   while .t.

      sc_cvacina()

      if OPC4 == 1
         select CVACINA
         ordsetfocus('SINC001')
         go top

      elseif OPC4 == 2
         select CVACINA
         ordsetfocus('SINC001')
         go bottom

      elseif OPC4 == 3
         WCODIGO  = space(03)          

         @ 08,12 get WCODIGO picture "@!"
         read_f()

         if lastkey() == 27
            exit
         endif

         select CVACINA
         ordsetfocus('SINC001')
         seek upper(WCODIGO)

         if ! found()
            @ 24,10 clear to 24,79
            centro(24,"C"+chr(162)+"digo n"+chr(132)+"o cadastrado...")
            inkey(1)
            @ 24,10 clear to 24,79
            loop
         endif

      elseif OPC4 == 4
         WVACINA = space(25)

         @ 10,12 get WVACINA
         read_f()

         if lastkey() == 27
            exit
         endif

         select CVACINA
         ordsetfocus('SINC002')
         seek upper(WVACINA)
      endif

      centro(24,"PgDw-Pr"+chr(162)+"ximo   PgUp-Anterior")

      KEY = 0

      while KEY  <> 27

         @ 08,12 say CODIGO      picture "@!"
         @ 10,12 say VACINA
         @ 12,12 say FREQUENCIA  picture "@R@E 9999dd" 

         KEY = inkey(0)

         view_user()

         if KEY == -7
            scalc()
         elseif KEY == -6
            calendario()
         elseif KEY == 3
            skip +1
            if eof()
               skip -1
            endif
         elseif KEY == 18
            skip -1
         elseif KEY == 27
            @ 24,10 clear to 24,70
            exit
         endif
      enddo

      if OPC4 == 1 .or. OPC4 == 2 
         exit
      endif
   enddo
enddo

return .t.


********************
function EXC_CVACINA
********************

while .t. 

   sc_cvacina()

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "EXCLUS"+chr(142)+"O"
   setcolor(COR_TEXT)

   WCODIGO = space(03)

   @ 08,12 get WCODIGO picture "@!"
   read_f()

   if empty(WCODIGO) .or. lastkey() == 27
      return .t.
   endif

   select CVACINA
   ordsetfocus('SINC001')
   seek upper(WCODIGO)

   if ! found()
      @ 24,10 clear to 24,79
      centro(24,"C"+chr(162)+"digo n"+chr(132)+"o cadastrado...")
      inkey(1)
      @ 24,10 clear to 24,79
      loop
   endif

   @ 08,12 say CODIGO     picture "@!"
   @ 10,12 say VACINA
   @ 12,12 say FREQUENCIA picture "@R@E 9999dd" 

   if confirma(24,"Confirma EXCLUS"+chr(142)+"O...(S/N)?","N") == "S"

      if ! reglock()
         loop
      endif                         

      replace CODIGO     with space(03)      ,;
              VACINA     with space(25)      ,;
              FREQUENCIA with 0              ,;
              USUARIO    with space(20)      ,;
              DATE       with ctod(space(08)),;
              TIME       with space(08)

      delete

      grava()
   endif

enddo

return .t.


*********************
function CHCK_CVACINA
*********************

select CVACINA
ordsetfocus('SINC001')
seek upper(WCODIGO)

if found()
   setcursor(0)
   @ 24,10 clear to 24,70
   centro(24,"C"+chr(162)+"digo de vacina j"+chr(160)+" cadastrado...")
   inkey(5)
   @ 24,10 clear to 24,70
   setcursor(1)
   return .f.
endif

return .t.


*********************
function SAVE_CVACINA
*********************

if ! reglock()
   return .f.
endif
   
replace CODIGO     with WCODIGO    ,;    
        FREQUENCIA with WFREQUENCIA,; 
        VACINA     with WVACINA    ,; 
        USUARIO    with WPUSUARIO  ,;
        TIME       with TIME()     ,;
        DATE       with DATE()

grava()

return .t.


*************
Function RACA
*************
OPC3 = 1

while .t.

   setcolor(COR_CAB) 
   @ 03,42 say "CADASTRO DE RA"+chr(128)+"AS                  " 
   setcolor(COR_TEXT)

   private OPCAO[5]
   OPCAO[1] = " Inclus"+chr(132)+"o   " 
   OPCAO[2] = " Altera"+chr(135)+chr(132)+"o  " 
   OPCAO[3] = " Consulta   " 
   OPCAO[4] = " Exclus"+chr(132)+"o   " 
   OPCAO[5] = " Esc - Fim  " 

   private ACESSO[5]
   ACESSO[1] = .t.
   ACESSO[2] = .t.
   ACESSO[3] = .t.
   ACESSO[4] = .t.
   ACESSO[5] = .t.

   private LETRA[5]
   LETRA[1] = "I"  
   LETRA[2] = "A"
   LETRA[3] = "C"
   LETRA[4] = "E"
   LETRA[5] = "F"

   private POS[5] 
   POS[1] = 1
   POS[2] = 1
   POS[3] = 1
   POS[4] = 1
   POS[5] = 7

   private OBS_MENU[5] 
   OBS_MENU[1] = "Inclus"+chr(132)+"o de ra"+chr(135)+"as..." 
   OBS_MENU[2] = "Altera"+chr(135)+chr(132)+"o de ra"+chr(135)+"as..."      
   OBS_MENU[3] = "Consulta as ra"+chr(135)+"as..."                                                
   OBS_MENU[4] = "Exclus"+chr(132)+"o de ra"+chr(135)+"as..." 
   OBS_MENU[5] = "Retorno ao menu principal..."                                                                                            

   OPC3 = achoise(OPC3,10,32,16,45,2,OPCAO,ACESSO,LETRA,POS,OBS_MENU,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)
   
   if OPC3 == 0 .or. OPC3 == 5
      return .t.
   elseif OPC3 == 1
      inc_raca()
   elseif OPC3 == 2
      alt_raca()
   elseif OPC3 == 3
      con_raca()
   elseif OPC3 == 4
      exc_raca()
   endif
enddo

return .t.

****************
function SC_RACA
****************

@ 05,00 clear to 22,79
@ 24,00 clear to 24,79

@ 08,01 say "C"+chr(162)+"digo...:"
@ 10,01 say "Ra"+chr(135)+"a.....:" 

return .t.

*****************
function INC_RACA
*****************

select RACA
ordsetfocus('SINC001')

WCODIGO = space(03)
WRACA   = space(25)

while .t. 

   sc_raca()

   setcolor(COR_CONS)
   @ 24,00 say "INCLUS"+chr(142)+"O"
   @ 24,73 say "ESC-FIM"
   setcolor(COR_TEXT)

   @ 08,12 get WCODIGO  picture "@!" valid chck_raca()
   @ 10,12 get WRACA
   read_f()

   if lastkey() == 27
      return .t.
   endif

   if confirma(24,"Dados Corretos...(S/N)?","S") == "S"
      select RACA
      ordsetfocus('SINC001')

      set deleted off
      go top
      if deleted()
         if ! reglock()
            loop
         endif

         recall
      else
         if ! inc_reg()
            loop
         endif
      endif
      set deleted on

      save_raca()

      WCODIGO = space(03)
      WRACA   = space(25)
   endif
enddo

return .t.

*****************
function ALT_RACA
*****************

while .t. 

   sc_raca()

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "ALTERA"+chr(128)+chr(142)+"O"
   setcolor(COR_TEXT)

   WCODIGO = space(03)

   @ 08,12 get WCODIGO picture "@!" 
   read_f()

   if empty(WCODIGO) .or. lastkey() == 27
      return .t.
   endif

   select RACA
   ordsetfocus('SINC001')
   seek upper(WCODIGO)

   if ! found()
      @ 24,10 clear to 24,79
      centro(24,"C"+chr(162)+"digo n"+chr(132)+"o cadastrado...")
      inkey(1)
      @ 24,10 clear to 24,79
      loop
   endif

   WCODIGO = CODIGO      
   WRACA   = RACA        

   while .t.

      sc_raca()

      @ 24,00 clear to 24,79

      setcolor(COR_CONS)
      @ 24,00 say "ALTERA"+chr(138)+chr(142)+"O"
      @ 24,73 say "ESC-FIM"
      setcolor(COR_TEXT)

      @ 08,12 say WCODIGO picture "@!"
      @ 10,12 get WRACA
      read_f()

      if lastkey() == 27
         return .t.
      endif

      if confirma(24,"Dados Corretos...(S/N)?","S") == "S"
         save_raca()
         exit
      endif

   enddo

enddo

return .t.

*****************
function CON_RACA
*****************

while .t.

   @ 05,00 clear to 22,79
   @ 24,00 clear to 24,79

   setcolor(COR_CONS)
   @ 24,00 say "CONSULTA"
   @ 24,73 say "ESC-FIM"
   setcolor(COR_TEXT)

   OPC4 = 5

   SOMBRA(10,14,18,67,8)

   setcolor(COR_CONS)
   @ 09,12 clear to 17,66
   @ 09,12 to 17,66 

   @ 10,14 say "Consulta a partir do in"+chr(161)+"cio do arquivo..........[1]"
   @ 11,14 say "Consulta a partir do final do arquivo...........[2]"
   @ 12,14 say "Consulta a partir do c"+chr(162)+"digo.....................[3]"
   @ 13,14 say "Consulta a partir do ra"+chr(135)+"a.......................[4]"
   @ 14,14 say "Retorno ao menu principal.......................[5]"

   setcolor(COR_TEXT)
   @ 16,14 say "Escolha sua op"+chr(135)+chr(132)+"o de consulta...................[ ]"
   @ 16,63 get OPC4 picture "9" range 1,5
   read_f()

   if OPC4 == 5
      return .t.
   endif

   do while .t.
      sc_raca()

      if OPC4 == 1
         select RACA
         ordsetfocus('SINC001')
         go top

      elseif OPC4 == 2
         select RACA
         ordsetfocus('SINC001')
         go bottom

      elseif OPC4 == 3
         WCODIGO  = space(03)          

         @ 08,12 get WCODIGO        picture "@!"
         read_f()

         if lastkey() == 27
            exit
         endif

         select RACA
         ordsetfocus('SINC001')
         seek upper(WCODIGO)

         if ! found()
            @ 24,10 clear to 24,79
            centro(24,"C"+chr(162)+"digo n"+chr(132)+"o cadastrado...")
            inkey(1)
            @ 24,10 clear to 24,79
            loop
         endif

      elseif OPC4 == 4
         WRACA = space(25)

         @ 10,12 get WRACA
         read_f()

         if lastkey() == 27
            exit
         endif

         select RACA
         ordsetfocus('SINC002')
         seek upper(WRACA)
      endif

      centro(24,"PgDw-Pr"+chr(162)+"ximo   PgUp-Anterior")

      KEY = 0

      while KEY  <> 27

         @ 08,12 say CODIGO picture "@!"
         @ 10,12 say RACA

         KEY = inkey(0)

         view_user()

         if KEY == -7
            scalc()
         elseif KEY == -6
            calendario()
         elseif KEY == 3
            skip +1
            if eof()
               skip -1
            endif
         elseif KEY == 18
            skip -1
         elseif KEY == 27
            @ 24,10 clear to 24,70
            exit
         endif
      enddo

      if OPC4 == 1 .or. OPC4 == 2 
         exit
      endif
   enddo
enddo

return .t.

*****************
function EXC_RACA
*****************

while .t. 

   sc_raca()

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "EXCLUS"+chr(142)+"O"
   setcolor(COR_TEXT)

   WCODIGO = space(03)

   @ 08,12 get WCODIGO picture "@!"
   read_f()

   @ 24,10 clear to 24,79

   if empty(WCODIGO) .or. lastkey() == 27
      return .t.
   endif

   select RACA
   ordsetfocus('SINC001')
   seek upper(WCODIGO)

   if ! found()
      @ 24,10 clear to 24,79
      centro(24,"C"+chr(162)+"digo n"+chr(132)+"o cadastrado...")
      inkey(1)
      @ 24,10 clear to 24,79
      loop
   endif

   @ 08,12 say CODIGO picture "@!"
   @ 10,12 say RACA

   if confirma(24,"Confirma EXCLUS"+chr(142)+"O...(S/N)?","N") == "S"

      if ! reglock()
         loop
      endif                         

      replace CODIGO   with space(03)      ,;
              RACA     with space(25)      ,;
              USUARIO  with space(20)      ,;
              DATE     with ctod(space(08)),;
              TIME     with space(08)

      delete

      grava()
   endif
enddo

return .t.


******************
function CHCK_RACA
******************

select RACA
ordsetfocus('SINC001')
seek upper(WCODIGO)

if found()
   setcursor(0)
   @ 24,10 clear to 24,70
   centro(24,"C"+chr(162)+"digo de ra"+Chr(135)+"a j"+chr(160)+" cadastrado...")
   inkey(5)
   @ 24,10 clear to 24,70
   setcursor(1)
   return .f.
endif

return .t.


******************
function SAVE_RACA
******************

if ! reglock()
   return .f.
endif
   
replace CODIGO  with WCODIGO  ,;    
        RACA    with WRACA    ,;   
        USUARIO with WPUSUARIO,;
        TIME    with TIME()   ,;
        DATE    with DATE()

grava()

return .t.


**************
function CATEG
**************
OPC3 = 1

while .t.

   setcolor(COR_CAB) 
   @ 03,42 say "CADASTRO DE CATEGORIAS             " 
   setcolor(COR_TEXT)

   private OPCAO[5]
   OPCAO[1] = " Inclus"+chr(132)+"o   " 
   OPCAO[2] = " Altera"+chr(135)+chr(132)+"o  " 
   OPCAO[3] = " Consulta   " 
   OPCAO[4] = " Exclus"+chr(132)+"o   " 
   OPCAO[5] = " Esc - Fim  " 

   private ACESSO[5]
   ACESSO[1] = .t.
   ACESSO[2] = .t.
   ACESSO[3] = .t.
   ACESSO[4] = .t.
   ACESSO[5] = .t.

   private LETRA[5]
   LETRA[1] = "I"  
   LETRA[2] = "A"
   LETRA[3] = "C"
   LETRA[4] = "E"
   LETRA[5] = "F"

   private POS[5] 
   POS[1] = 1
   POS[2] = 1
   POS[3] = 1
   POS[4] = 1
   POS[5] = 7

   private OBS_MENU[5] 
   OBS_MENU[1] = "Inclus"+chr(132)+"o de categorias..." 
   OBS_MENU[2] = "Altera"+chr(135)+chr(132)+"o de categorias..."      
   OBS_MENU[3] = "Consulta as categorias..."                                                
   OBS_MENU[4] = "Exclus"+chr(132)+"o de categorias..." 
   OBS_MENU[5] = "Retorno ao menu principal..."                                                                                            

   OPC3 = achoise(OPC3,10,32,16,45,2,OPCAO,ACESSO,LETRA,POS,OBS_MENU,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)
   
   if OPC3 == 0 .or. OPC3 == 5
      return .t.
   elseif OPC3 == 1
      inc_categ()
   elseif OPC3 == 2
      alt_categ()
   elseif OPC3 == 3
      con_categ()
   elseif OPC3 == 4
      exc_categ()
   endif
enddo

return .t.

*****************
function SC_CATEG
*****************

@ 05,00 clear to 22,79
@ 24,00 clear to 24,79

@ 08,01 say "C"+chr(162)+"digo...:"
@ 10,01 say "Categoria:" 

return .t.

******************
function INC_CATEG
******************

select CATEG
ordsetfocus('SINC001')

WCODIGO    = space(03)
WCATEGORIA = space(20)

while .t. 

   sc_categ()

   setcolor(COR_CONS)
   @ 24,00 say "INCLUS"+chr(142)+"O"
   @ 24,73 say "ESC-FIM"
   setcolor(COR_TEXT)

   @ 08,12 get WCODIGO    picture "@!"  valid chck_categ()
   @ 10,12 get WCATEGORIA
   read_f()

   if lastkey() == 27
      return .t.
   endif

   if confirma(24,"Dados Corretos...(S/N)?","S") == "S"

      select CATEG
      ordsetfocus('SINC001')

      set deleted off
      go top
      if deleted()
         if ! reglock()
            loop
         endif

         recall
      else
         if ! inc_reg()
            loop
         endif
      endif
      set deleted on

      save_categ()

      WCODIGO    = space(03)
      WCATEGORIA = space(20)

   endif

enddo

return .t.

******************
function ALT_CATEG
******************

while .t. 

   sc_categ()

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "ALTERA"+chr(128)+chr(142)+"O"
   setcolor(COR_TEXT)

   WCODIGO = space(03)

   @ 08,12 get WCODIGO picture "@!" 
   read_f()

   @ 24,10 clear to 24,79

   if empty(WCODIGO) .or. lastkey() == 27
      return .t.
   endif

   select CATEG
   ordsetfocus('SINC001')
   seek upper(WCODIGO)

   if ! found()
      @ 24,10 clear to 24,79
      centro(24,"C"+chr(162)+"digo n"+chr(132)+"o cadastrado...")
      inkey(1)
      @ 24,10 clear to 24,79
      loop
   endif

   WCODIGO    = CODIGO      
   WCATEGORIA = CATEGORIA   

   while .t.

      sc_categ()

      setcolor(COR_CONS)
      @ 24,00 say "ALTERA"+chr(138)+chr(142)+"O"
      @ 24,73 say "ESC-FIM"
      setcolor(COR_TEXT)

      @ 08,12 say WCODIGO     picture "@!"
      @ 10,12 get WCATEGORIA
      read_f()

      if lastkey() == 27
         return .t.
      endif

      if confirma(24,"Dados Corretos...(S/N)?","S") == "S"
         save_categ()
         exit
      endif
   enddo
enddo

return .t.

******************
function CON_CATEG
******************

while .t.

   OPC4 = 5

   @ 05,00 clear to 22,79
   @ 24,00 clear to 24,79

   setcolor(COR_CONS)
   @ 24,00 say "CONSULTA"
   @ 24,73 say "ESC-FIM"
   setcolor(COR_TEXT)

   SOMBRA(10,14,18,67,8)

   setcolor(COR_CONS)
   @ 09,12 clear to 17,66
   @ 09,12 to 17,66 

   @ 10,14 say "Consulta a partir do in"+chr(161)+"cio do arquivo..........[1]"
   @ 11,14 say "Consulta a partir do final do arquivo...........[2]"
   @ 12,14 say "Consulta a partir do c"+chr(162)+"digo.....................[3]"
   @ 13,14 say "Consulta a partir do categoria..................[4]"
   @ 14,14 say "Retorno ao menu principal.......................[5]"

   setcolor(COR_TEXT)
   @ 16,14 say "Escolha sua op"+chr(135)+chr(132)+"o de consulta...................[ ]"
   @ 16,63 get OPC4 picture "9" range 1,5
   read_f()

   if OPC4 == 5
      return .t.
   endif

   while .t.

      sc_categ()

      if OPC4 == 1
         select CATEG
         ordsetfocus('SINC001')
         go top

      elseif OPC4 == 2
         select CATEG
         ordsetfocus('SINC001')
         go bottom

      elseif OPC4 == 3
         WCODIGO  = space(03)          

         @ 08,12 get WCODIGO picture "@!"
         read_f()

         if lastkey() == 27
            exit
         endif

         select CATEG
         ordsetfocus('SINC001')
         seek upper(WCODIGO)

         if ! found()
            @ 24,10 clear to 24,79
            centro(24,"C"+chr(162)+"digo n"+chr(132)+"o cadastrado...")
            inkey(1)
            @ 24,10 clear to 24,79
            loop
         endif

      elseif OPC4 == 4
         WCATEGORIA = space(20)

         @ 10,12 get WCATEGORIA
         read_f()

         if lastkey() == 27
            exit
         endif

         select CATEG
         ordsetfocus('SINC002')
         seek upper(WCATEGORIA)
      endif

      centro(24,"PgDw-Pr"+chr(162)+"ximo   PgUp-Anterior")

      KEY = 0

      while KEY  <> 27

         @ 08,12 say CODIGO    picture "@!"
         @ 10,12 say CATEGORIA

         KEY = inkey(0)

         view_user()

         if KEY == -7
            scalc()
         elseif KEY == -6
            calendario()
         elseif KEY == 3
            skip +1
            if eof()
               skip -1
            endif
         elseif KEY == 18
            skip -1
         elseif KEY == 27
            @ 24,10 clear to 24,70
            exit
         endif
      enddo

      if OPC4 == 1 .or. OPC4 == 2 
         exit
      endif
   enddo
enddo

return .t.

******************
function EXC_CATEG
******************

while .t. 

   sc_categ()

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "EXCLUS"+chr(142)+"O"
   setcolor(COR_TEXT)

   WCODIGO = space(03)

   @ 08,12 get WCODIGO picture "@!"
   read_f()

   if empty(WCODIGO) .or. lastkey() == 27
      return .t.
   endif

   select CATEG
   ordsetfocus('SINC001')
   seek upper(WCODIGO)

   if ! found()
      @ 24,10 clear to 24,79
      centro(24,"C"+chr(162)+"digo n"+chr(132)+"o cadastrado...")
      inkey(1)
      @ 24,10 clear to 24,79
      loop
   endif

   @ 08,12 say CODIGO   picture "@!"
   @ 10,12 say CATEGORIA

   if confirma(24,"Confirma EXCLUS"+chr(142)+"O...(S/N)?","N") == "S"
      if ! reglock()
         loop
      endif                         

      replace CODIGO    with space(03)      ,;
              CATEGORIA with space(20)      ,;
              USUARIO   with space(20)      ,;
              DATE      with ctod(space(08)),;
              TIME      with space(08)

      delete

      grava()
   endif
enddo

return .t.

*******************
function CHCK_CATEG
*******************

select CATEG
ordsetfocus('SINC001')
seek upper(WCODIGO)

if found()
   setcursor(0)
   @ 24,10 clear to 24,70
   centro(24,"C"+chr(162)+"digo de categoria j"+chr(160)+" cadastrado...")
   inkey(5)
   @ 24,10 clear to 24,70
   setcursor(1)

   return .f.
endif

return .t.

*******************
function SAVE_CATEG
*******************

if ! reglock()
   return .f.
endif
   
replace CODIGO    with WCODIGO   ,;    
        CATEGORIA with WCATEGORIA,;
        USUARIO   with WPUSUARIO ,;
        TIME      with TIME()    ,;
        DATE      with DATE()

grava()

return .t.

**************
Function PASTO
**************
OPC3 = 1

while .t.

   setcolor(COR_CAB) 
   @ 03,42 say "CADASTRO DE PASTOS                 " 
   setcolor(COR_TEXT)

   private OPCAO[5]
   OPCAO[1] = " Inclus"+chr(132)+"o   " 
   OPCAO[2] = " Altera"+chr(135)+chr(132)+"o  " 
   OPCAO[3] = " Consulta   " 
   OPCAO[4] = " Exclus"+chr(132)+"o   " 
   OPCAO[5] = " Esc - Fim  " 

   private ACESSO[5]
   ACESSO[1] = .t.
   ACESSO[2] = .t.
   ACESSO[3] = .t.
   ACESSO[4] = .t.
   ACESSO[5] = .t.

   private LETRA[5]
   LETRA[1] = "I"  
   LETRA[2] = "A"
   LETRA[3] = "C"
   LETRA[4] = "E"
   LETRA[5] = "F"

   private POS[5] 
   POS[1] = 1
   POS[2] = 1
   POS[3] = 1
   POS[4] = 1
   POS[5] = 7

   private OBS_MENU[5] 
   OBS_MENU[1] = "Inclus"+chr(132)+"o de pastos..." 
   OBS_MENU[2] = "Altera"+Chr(135)+Chr(132)+"o de pastos..."                                                
   OBS_MENU[3] = "Consulta aos pastos..."                                                
   OBS_MENU[4] = "Exclus"+chr(132)+"o de pastos..." 
   OBS_MENU[5] = "Retorno ao menu principal..."                                                                                            

   OPC3 = achoise(OPC3,10,32,16,45,2,OPCAO,ACESSO,LETRA,POS,OBS_MENU,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)
   
   if OPC3 == 0 .or. OPC3 == 4
      return .t.
   elseif OPC3 == 1
      inc_pasto()
   elseif OPC3 == 2
      alt_PASTO()
   elseif OPC3 == 3
      con_pasto()
   elseif OPC3 == 4 
      exc_pasto()
   endif

enddo

return .t.

*****************
function SC_PASTO
*****************

@ 05,00 clear to 22,79
@ 24,00 clear to 24,79

@ 08,01 say "Pasto....:"
@ 10,01 say "Descri"+Chr(135)+Chr(132)+"o:"

return .t.

******************
function INC_PASTO
******************

select PASTO
ordsetfocus('SINC001')

WPASTO = Space(10)
WDESC  = Space(35)

while .t. 

   sc_pasto()   

   setcolor(COR_CONS)
   @ 24,00 say "INCLUS"+chr(142)+"O"
   @ 24,73 say "ESC-FIM"
   setcolor(COR_TEXT)

   @ 08,12 get WPASTO picture "@!" valid chck_pasto()
   @ 10,12 get WDESC                                    
   read_f()

   if lastkey() == 27
      return .t.
   endif

   if confirma(24,"Dados Corretos...(S/N)?","S") == "S"
      select PASTO
      ordsetfocus('SINC001')

      set deleted off
      go top
      if deleted()
         if ! reglock()
            loop
         endif

         recall
      else
         if ! inc_reg()
            loop
         endif
      endif
      set deleted on

      save_pasto()   

      WPASTO = space(10)
      WDESC  = space(35)
   endif
enddo

return .t.


******************
Function alt_PASTO
******************

while .t. 

   sc_pasto()

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "ALTERA"+Chr(128)+Chr(142)+"O"
   setcolor(COR_TEXT)

   WPASTO = space(10)

   @ 08,12 get WPASTO picture "@!"
   read_f()

   @ 24,10 clear to 24,79

   if empty(WPASTO) .or. lastkey() == 27
      return .t.
   endif

   select PASTO
   ordsetfocus('SINC001')
   seek upper(WPASTO)

   if ! found()
      @ 24,10 clear to 24,79
      centro(24,"Pasto n"+chr(132)+"o cadastrado...")
      inkey(1)
      @ 24,10 clear to 24,79
      loop
   endif

   WDESC = DESCRICAO

   While .T.

       @ 10,12 Get WDESC
       READ_F()

       If Empty(WDESC) .Or. Lastkey()
          Exit
       Endif

       if confirma(24,"Dados corretos...(S/N)?","N") == "S"

          if ! reglock()
             loop
          endif                         

          replace DESCRICAO with space(20)      ,;
                  USUARIO   with space(20)      ,;
                  DATE      with ctod(space(08)),;
                  TIME      with space(08)

          grava()

          Exit
       endif

    Enddo

Enddo

return .t.


******************
function CON_PASTO
******************

while .t.

   OPC4 = 4

   @ 05,00 clear to 22,79
   @ 24,00 clear to 24,79

   setcolor(COR_CONS)
   @ 24,00 say "CONSULTA"
   @ 24,73 say "ESC-FIM"
   setcolor(COR_TEXT)

   SOMBRA(10,14,17,67,8)

   setcolor(COR_CONS)
   @ 09,12 clear to 16,66
   @ 09,12 to 16,66 

   @ 10,14 say "Consulta a partir do in"+chr(161)+"cio do arquivo..........[1]"
   @ 11,14 say "Consulta a partir do final do arquivo...........[2]"
   @ 12,14 say "Consulta a partir do pasto......................[3]"
   @ 13,14 say "Retorno ao menu principal.......................[4]"

   setcolor(COR_TEXT)
   @ 15,14 say "Escolha sua op"+chr(135)+chr(132)+"o de consulta...................[ ]"
   @ 15,63 get OPC4 picture "9" range 1,4
   read_f()

   if OPC4 == 4
      return .t.
   endif

   do while .t.
      sc_pasto()    

      if OPC4 == 1
         select PASTO
         ordsetfocus('SINC001')
         go top

      elseif OPC4 == 2
         select PASTO
         ordsetfocus('SINC001')
         go bottom

      elseif OPC4 == 3
         WPASTO  = space(10)          

         @ 08,12 get WPASTO picture "@!"
         read_f()

         if lastkey() == 27
            exit
         endif

         select PASTO
         ordsetfocus('SINC001')
         seek upper(WPASTO)

         if ! found()
            @ 24,10 clear to 24,79
            centro(24,"Pasto n"+chr(132)+"o cadastrado...")
            inkey(1)
            @ 24,10 clear to 24,79
            loop
         endif
      endif

      centro(24,"PgDw-Pr"+chr(162)+"ximo   PgUp-Anterior")

      KEY = 0

      while KEY  <> 27

         @ 08,12 say PASTO picture "@!"
         @ 10,12 say DESCRICAO                   

         KEY = inkey(0)

         view_user()

         if KEY == -7
            scalc()
         elseif KEY == -6
            calendario()
         elseif KEY == 3
            skip +1
            if eof()
               skip -1
            endif
         elseif KEY == 18
            skip -1
         elseif KEY == 27
            @ 24,10 clear to 24,70
            exit
         endif
      enddo

      if OPC4 == 1 .or. OPC4 == 2 
         exit
      endif
   enddo
enddo

return .t.

******************
function EXC_PASTO
******************

while .t. 

   sc_pasto()

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "EXCLUS"+chr(142)+"O"
   setcolor(COR_TEXT)

   WPASTO = space(10)

   @ 08,12 get WPASTO picture "@!"
   read_f()

   @ 24,10 clear to 24,79

   if empty(WPASTO) .or. lastkey() == 27
      return .t.
   endif

   select PASTO
   ordsetfocus('SINC001')
   seek upper(WPASTO)

   if ! found()
      @ 24,10 clear to 24,79
      centro(24,"Pasto n"+chr(132)+"o cadastrado...")
      inkey(1)
      @ 24,10 clear to 24,79
      loop
   endif

   @ 08,12 say PASTO    picture "@!"
   @ 10,12 say DESCRICAO 

   if confirma(24,"Confirma EXCLUS"+chr(142)+"O...(S/N)?","N") == "S"

      if ! reglock()
         loop
      endif                         

      replace PASTO     with space(10)      ,;
              DESCRICAO with space(20)      ,;
              USUARIO   with space(20)      ,;
              DATE      with ctod(space(08)),;
              TIME      with space(08)

      delete

      grava()
   endif
enddo

return .t.


*******************
function CHCK_PASTO
*******************

select PASTO
ordsetfocus('SINC001')
seek upper(WPASTO)

if found()
   setcursor(0)
   @ 24,10 clear to 24,70
   centro(24,"Pasto j"+chr(160)+" cadastrado...")
   inkey(5)
   @ 24,10 clear to 24,70
   setcursor(1)

   return .f.
endif

return .t.

*******************
function SAVE_PASTO
*******************

if ! reglock()
   return .f.
endif
   
replace PASTO     with WPASTO   ,;
        DESCRICAO with WDESC    ,; 
        USUARIO   with WPUSUARIO,;
        TIME      with TIME()   ,;
        DATE      with DATE()

grava()

return .t.


*****************
Function SC_TROCA
*****************

@ 05,00 clear to 22,79
@ 24,00 clear to 24,79

@ 08,01 say "Animal...:        - " 
@ 10,01 say "Lote.....:     - "
@ 12,01 say "NOVO LOTE:     - "

return .t.


**************
function TROCA
**************

setcolor(COR_CAB)   
@ 03,42 say "TROCA DE LOTES                     " 
setcolor(COR_TEXT)

WANIMAL = space(06)
WLOTE   = space(03)

while .t. 

   sc_troca()

   setcolor(COR_CONS)
   @ 24,00 say "INCLUS"+chr(142)+"O"
   @ 24,73 say "ESC-FIM"
   setcolor(COR_TEXT)

   @ 08,12 get WANIMAL picture "@!" valid busca_animal()
   read_f()

   if lastkey() == 27
      return .t.
   endif

   select ANIMAL
   ordsetfocus('SINC001')
   seek WANIMAL
 
   if found()
      @ 08,21 say NOME
      @ 08,12 say LOTE
   else
      @ 08,21 say space(30)
      @ 08,12 say space(03)
   endif

   WLOT = LOTE  

   Select LOTE
   ordsetfocus('SINC001')
   Seek WLOT

   If Found()
      @ 10,12 Say WLOT
      @ 10,18 Say DESCRICAO        
   Else
      @ 10,12 Say Space(03)
      @ 10,18 Say Space(30)
   Endif

   @ 12,12 get WLOTE Picture "@!" Valid win_LOTE(12,18,30)
   read_f()

   if lastkey() == 27
      return .t.
   endif

   if confirma(24,"Dados Corretos...(S/N)?","S") == "S"

      select ANIMAL
      ordsetfocus('SINC001')
      seek WANIMAL

      if ! reglock()
         loop
      endif

      replace LOTE with WLOTE

      grava()

      WANIMAL = space(06)        
      WLOTE   = space(03)        

   endif

enddo

return .t.


***************************
Function win_LOTE(LL,CC,TT)
***************************

If Lastkey() = 27
   Return .T.
Endif

If Empty(WLOTE)
   help_LOTE()
   Return .F.
Endif

Select LOTE
ordsetfocus('SINC001')
Seek WLOTE

If ! Found()
   ERROR("N"+Chr(163)+"mero de lote n"+Chr(132)+"o cadastrado!",0)
   Return .F.
Endif

@ LL,CC Say Left(DESCRICAO,TT)

Return .T.


*****************
function SC_BAIXA
*****************

@ 05,00 clear to 22,79
@ 24,00 clear to 24,79

@ 08,01 say "Animal...:        - " 
@ 10,01 say "Sa"+chr(161)+"da....:"
@ 11,01 say "Motivo...:"

Return .T.


**************
function BAIXA
**************

setcolor(COR_CAB)   
@ 03,42 say "SAIDA DE ANIMAIS                   " 
setcolor(COR_TEXT)

WANIMAL   = space(06)
WSAIDA    = DATE()            
WTP_SAIDA = space(30)

while .t. 

   sc_baixa()

   setcolor(COR_CONS)
   @ 24,00 say "INCLUS"+chr(142)+"O"
   @ 24,73 say "ESC-FIM"
   setcolor(COR_TEXT)

   @ 08,12 get WANIMAL         picture "@!"         valid busca_animal()
   @ 10,12 get WSAIDA                               valid ! empty(WSAIDA)
   @ 11,12 get WTP_SAIDA                            

   select ANIMAL
   ordsetfocus('SINC001')
   seek WANIMAL
 
   if found()
      @ 08,21 say NOME
   else
      @ 08,21 say space(30)
   endif
   read_f()

   if lastkey() == 27
      return .t.
   endif

   if confirma(24,"Dados Corretos...(S/N)?","S") == "S"

      select ANIMAL
      ordsetfocus('SINC001')
      seek WANIMAL

      if ! reglock()
         loop
      endif

      replace SAIDA    with WSAIDA   ,;
              TP_SAIDA with WTP_SAIDA,;
              USUARIO  with WPUSUARIO,;
              TIME     with TIME()   ,;
              DATE     with DATE()

      grava()

      WANIMAL   = space(06)        
      WSAIDA    = DATE()            
      WTP_SAIDA = space(30)

   endif

enddo

return .t.


*******************
Function PROD_LEITE
*******************
OPC2 = 1

while .t.

   setcolor(COR_CAB)   
   @ 03,42 say "PRODU"+chr(128)+chr(142)+"O DE LEITE                  " 
   setcolor(COR_TEXT)

   private OPCAO[3]
   OPCAO[1] = " Pesagem de Leite " 
   OPCAO[2] = " Secagem          "
   OPCAO[3] = " Esc - Fim        " 

   private ACESSO[3]
   ACESSO[1] = .t.
   ACESSO[2] = .t.
   ACESSO[3] = .t.

   private LETRA[3]
   LETRA[1] = "P"  
   LETRA[2] = "S"  
   LETRA[3] = "F"  

   private POS[3]
   POS[1] = 1
   POS[2] = 1 
   POS[3] = 7
 
   private OBS_MENU[3]
   OBS_MENU[1] = "Cadastro de pesagem de leite..."
   OBS_MENU[2] = "Cadastro de secagem..."
   OBS_MENU[3] = "Retornar ao menu principal..."
 
   OPC2 = achoise(OPC2,10,29,14,48,1,OPCAO,ACESSO,LETRA,POS,OBS_MENU,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)

   if OPC2 == 0 .or. OPC2 == 3
      return .t.
   elseif OPC2 == 1
      leite()    
   elseif OPC2 == 2
      seca()      
   endif

enddo

return .t.


**************
function LEITE
**************
OPC3 = 1

while .t.

   setcolor(COR_CAB) 
   @ 03,42 say "PRODU"+chr(128)+chr(142)+"O DE LEITE                  " 
   setcolor(COR_TEXT)

   private OPCAO[5]
   OPCAO[1] = " Inclus"+chr(132)+"o  " 
   OPCAO[2] = " Altera"+chr(135)+chr(132)+"o " 
   OPCAO[3] = " Consulta  " 
   OPCAO[4] = " Exclus"+chr(132)+"o  " 
   OPCAO[5] = " Esc - Fim " 

   private ACESSO[5]
   ACESSO[1] = .t.
   ACESSO[2] = .t.
   ACESSO[3] = .t.
   ACESSO[4] = .t.
   ACESSO[5] = .t.

   private LETRA[5]
   LETRA[1] = "I"  
   LETRA[2] = "A"
   LETRA[3] = "C"
   LETRA[4] = "E"
   LETRA[5] = "F"

   private POS[5] 
   POS[1] = 1
   POS[2] = 1
   POS[3] = 1
   POS[4] = 1
   POS[5] = 7

   private OBS_MENU[5] 
   OBS_MENU[1] = "Inclus"+chr(132)+"o de produ"+chr(135)+chr(132)+"o de Leite..." 
   OBS_MENU[2] = "Altera"+chr(135)+chr(132)+"o de produ"+chr(135)+chr(132)+"o de leite..."       
   OBS_MENU[3] = "Consulta a produ"+chr(135)+chr(148)+"oes de leite..."                                       
   OBS_MENU[4] = "Exclus"+chr(132)+"o de produ"+chr(135)+chr(132)+"o de leite..."  
   OBS_MENU[5] = "Retorno ao menu principal..."                                                                                            

   OPC3 = achoise(OPC3,10,32,16,44,3,OPCAO,ACESSO,LETRA,POS,OBS_MENU,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)
   
   if OPC3 == 0 .or. OPC3 == 5
      return .t.
   elseif OPC3 == 1
      inc_leite()
   elseif OPC3 == 2
      alt_leite()
   elseif OPC3 == 3
      con_leite()
   elseif OPC3 == 4
      exc_leite()
   endif

enddo

return .t.

*****************
Function SC_LEITE
*****************

Setcolor(COR_TEXT)
@ 05,00 Clear To 22,79
@ 24,00 clear to 24,79

@ 08,01 say "Controle.:"
@ 10,01 say "Animal...:        - " 
@ 12,01 say "Data.....:"
@ 14,01 say "Litros...:"

return .t.


******************
function INC_LEITE
******************

WANIMAL = space(06)
WDATA   = date()   
WLITRO  = 0          

select LEITE
ordsetfocus('SINC001')
Go Bottom

WSEQ = SEQ + 1

while .t. 

   sc_leite()

   setcolor(COR_CONS)
   @ 24,00 say "INCLUS"+chr(142)+"O"
   @ 24,73 say "ESC-FIM"
   setcolor(COR_TEXT)

   @ 08,12 say Strzero(WSEQ,6)
   @ 10,12 get WANIMAL         picture "@!"         valid busca_animal("LEITE") 
   @ 12,12 get WDATA                                valid ! empty(WDATA) 
   @ 14,12 get WLITRO          picture "@R@E 999,999.99"

   select ANIMAL
   ordsetfocus('SINC001')
   seek WANIMAL
 
   if found()
      @ 10,21 say NOME
   else
      @ 10,21 say space(20)
   endif
  
   select LEITE
   read_f()

   if lastkey() == 27
      return .t.
   endif

   if confirma(24,"Dados corretos...(S/N)?","S") == "S"

      select LEITE
      ordsetfocus('SINC001')
      go bottom

      WSEQ = SEQ + 1

      set deleted off
      go top
      if deleted()
         if ! reglock()
            loop
         endif

         recall
      else
         if ! inc_reg()
            loop
         endif
      endif
      set deleted on

      save_LEITE()

      setcolor(COR_TEXT+"*+")
      @ 08,12 say Strzero(WSEQ,6)
      setcolor(COR_TEXT)

      inkey(0)

      WSEQ    = 0        
      WANIMAL = Space(06)        
      WDATA   = DATE()   
      WLITRO  = 0        

   endif

enddo

return .t.


******************
function ALT_LEITE
******************

while .t. 

   sc_leite()

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "ALTERA"+chr(128)+chr(142)+"O"
   setcolor(COR_TEXT)

   WSEQ = 0

   @ 08,12 get WSEQ picture "@Z 999999" 
   read_f()

   if empty(WSEQ) .or. lastkey() == 27
      return .t.
   endif

   select LEITE
   ordsetfocus('SINC001')
   seek WSEQ

   if ! found()
      @ 24,10 clear to 24,79
      centro(24,"Sequ"+Chr(136)+"ncia de controle n"+chr(132)+"o cadastrada...")
      inkey(1)
      @ 24,10 clear to 24,79
      loop
   endif

   WANIMAL = ANIMAL      
   WDATA   = DATA        
   WLITRO  = LITRO

   while .t.

      sc_leite()

      setcolor(COR_CONS)
      @ 24,00 say "ALTERA"+chr(138)+chr(142)+"O"
      @ 24,73 say "ESC-FIM"
      setcolor(COR_TEXT)

      @ 08,12 say Strzero(WSEQ,6)
      @ 10,12 get WANIMAL         picture "@!"         valid busca_animal("LEITE")
      @ 12,12 get WDATA                                valid ! empty(WDATA)
      @ 14,12 get WLITRO          picture "@R@E 999,999.99" 

      select ANIMAL
      ordsetfocus('SINC001')
      seek WANIMAL
      
      if found()
         @ 10,21 say NOME
      else
         @ 10,21 say space(20)
      endif
      
      select LEITE
      read_f()

      if lastkey() == 27
         return .t.
      endif

      if confirma(24,"Dados corretos...(S/N)?","S") == "S"
         save_LEITE()
         exit
      endif

   enddo

enddo

return .t.


******************
function CON_LEITE
******************

select LEITE

while .t.

   OPC4 = 5

   @ 05,00 clear to 22,79
   @ 24,00 clear to 24,79

   setcolor(COR_CONS)
   @ 24,00 say "CONSULTA"
   @ 24,73 say "ESC-FIM"
   setcolor(COR_TEXT)

   SOMBRA(09,14,17,67,8)

   setcolor(COR_CONS)
   @ 08,12 clear to 16,66
   @ 08,12 to 16,66 

   @ 09,14 say "Consulta a partir do in"+chr(161)+"cio do arquivo..........[1]"
   @ 10,14 say "Consulta a partir do final do arquivo...........[2]"
   @ 11,14 say "Consulta a partir da sequ"+Chr(136)+"ncia de controle......[3]"
   @ 12,14 say "Consulta a partir do animal.....................[4]"
   @ 13,14 say "Retorno ao menu principal.......................[5]"

   setcolor(COR_TEXT)
   @ 15,14 say "Escolha sua op"+chr(135)+chr(132)+"o de consulta...................[ ]"
   @ 15,63 get OPC4 picture "9" range 1,5
   read_f()

   if OPC4 == 5
      return .t.
   endif

   while .t.

      sc_leite()

      if OPC4 == 1
         select LEITE
         ordsetfocus('SINC001')
         go top

      elseif OPC4 == 2
         select LEITE
         ordsetfocus('SINC001')
         go bottom

      elseif OPC4 == 3
         WSEQ  = 0          

         @ 08,12 get WSEQ picture "@Z 999999" 
         read_f()

         if lastkey() == 27
            exit
         endif

         select LEITE
         ordsetfocus('SINC001')
         seek WSEQ  

         if ! found()
            @ 24,10 clear to 24,79
            centro(24,"Sequ"+Chr(136)+"ncia de controle n"+chr(132)+"o cadastrada...")
            inkey(1)
            @ 24,10 clear to 24,79
            loop
         endif

      elseif OPC4 == 4

         WANIMAL = space(06)          
         WDATA   = ctod(space(08))

         @ 10,12 get WANIMAL  picture "@!" valid busca_animal("LEITE")
         @ 12,12 get WDATA                                
         read_f()

         if lastkey() == 27
            exit
         endif

         select LEITE
         ordsetfocus('SINC002')
         seek WANIMAL+dtos(WDATA)

         If EOF()
            Skip -1
         Endif

      endif

      centro(24,"PgDw-Pr"+chr(162)+"ximo    PgUp-Anterior")

      KEY = 0

      while KEY <> 27

         @ 08,12 say Strzero(SEQ,6)
         @ 10,12 say ANIMAL  picture "@!"     
         @ 12,12 say DATA                               
         @ 14,12 say LITRO   picture "@R@E 999,999.99" 

         WANIMAL = ANIMAL

         select ANIMAL
         ordsetfocus('SINC001')
         seek WANIMAL
         
         if found()
            @ 10,21 say NOME
         else
            @ 10,21 say space(20)
         endif
         
         select LEITE

         KEY = inkey(0)

         view_user()

         if KEY == -7
            scalc()
         elseif KEY == -6
            calendario()
         elseif KEY == 3
            skip +1
            if eof()
               skip -1
            endif
         elseif KEY == 18
            skip -1
         elseif KEY == 27
            @ 24,10 clear to 24,70
            exit
         endif
      enddo

      if OPC4 == 1 .or. OPC4 == 2 
         exit
      endif

   enddo

enddo

return .t.


******************
function EXC_LEITE
******************

while .t. 

   sc_leite()

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "EXCLUS"+chr(142)+"O"
   setcolor(COR_TEXT)

   WSEQ = 0

   @ 08,12 get WSEQ picture "@Z 999999"
   read_f()

   if empty(WSEQ) .or. lastkey() == 27
      return .t.
   endif

   select LEITE
   ordsetfocus('SINC001')
   seek WSEQ

   if ! found()
      @ 24,10 clear to 24,79
      centro(24,"Sequ"+Chr(136)+"ncia de controle n"+chr(132)+"o cadastrada...")
      inkey(1)
      @ 24,10 clear to 24,79
      loop
   endif

   @ 08,12 say Strzero(SEQ,6)
   @ 10,12 say ANIMAL  picture "@!"     
   @ 12,12 say DATA                               
   @ 14,12 say LITRO   picture "@R@E 999,999.99" 

   WANIMAL = ANIMAL

   select ANIMAL
   ordsetfocus('SINC001')
   seek WANIMAL
   
   if found()
      @ 10,21 say NOME
   else
      @ 10,21 say space(20)
   endif
   
   select LEITE

   if confirma(24,"Confirma EXCLUS"+chr(142)+"O...(S/N)?","N") == "S"

      if ! reglock()
         loop
      endif                         

      replace SEQ     with 0              ,;        
              ANIMAL  with space(06)      ,;        
              DATA    with ctod(space(08)),;   
              LITRO   with 0              ,;
              USUARIO with space(20)      ,;
              DATE    with ctod(space(08)),;
              TIME    with space(08)

      delete

      grava()
   endif
enddo

return .t.


*******************
function SAVE_LEITE
*******************

if ! reglock()
   return .f.
endif
   
replace SEQ     with WSEQ   ,;    
        ANIMAL  with WANIMAL,; 
        DATA    with WDATA  ,;   
        LITRO   with WLITRO

replace USUARIO with WPUSUARIO,;
        TIME    with time()   ,;
        DATE    with date()

grava()

return .t.


*************
function SECA
*************

OPC3 = 1

while .t.

   setcolor(COR_CAB) 
   @ 03,42 say "SECAGEM                            "
   setcolor(COR_TEXT)

   private OPCAO[5]
   OPCAO[1] = " Inclus"+chr(132)+"o  " 
   OPCAO[2] = " Altera"+chr(135)+chr(132)+"o " 
   OPCAO[3] = " Consulta  " 
   OPCAO[4] = " Exclus"+chr(132)+"o  " 
   OPCAO[5] = " Esc - Fim " 

   private ACESSO[5]
   ACESSO[1] = .t.
   ACESSO[2] = .t.
   ACESSO[3] = .t.
   ACESSO[4] = .t.
   ACESSO[5] = .t.

   private LETRA[5]
   LETRA[1] = "I"  
   LETRA[2] = "A"
   LETRA[3] = "C"
   LETRA[4] = "E"
   LETRA[5] = "F"

   private POS[5] 
   POS[1] = 1
   POS[2] = 1
   POS[3] = 1
   POS[4] = 1
   POS[5] = 7

   private OBS_MENU[5] 
   OBS_MENU[1] = "Inclus"+chr(132)+"o de secagem..." 
   OBS_MENU[2] = "Altera"+chr(135)+chr(132)+"o de secagem..."
   OBS_MENU[3] = "Consulta as secagem..."                   
   OBS_MENU[4] = "Exclus"+chr(132)+"o de secagem..."  
   OBS_MENU[5] = "Retorno ao menu principal..."                                                                                            

   OPC3 = achoise(OPC3,10,32,16,44,3,OPCAO,ACESSO,LETRA,POS,OBS_MENU,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)
   
   if OPC3 == 0 .or. OPC3 == 5
      return .t.
   elseif OPC3 == 1
      inc_seca()
   elseif OPC3 == 2
      alt_seca()
   elseif OPC3 == 3
      con_seca()
   elseif OPC3 == 4
      exc_seca()
   endif

enddo

return .t.


****************
function SC_SECA
****************

Setcolor(COR_TEXT)
@ 05,00 Clear To 22,79
@ 24,00 Clear To 24,79

@ 08,01 say "Controle.:"
@ 10,01 say "Animal...:        - " 
@ 12,01 say "Data.....:"

Return .T.


*****************
function INC_SECA
*****************

while .t. 

    WANIMAL = space(06)
    WDATA   = date()   

    select SECA
    ordsetfocus('SINC001')
    Go Bottom

    WSEQ = SEQ + 1

    while .t. 

        sc_seca()

        setcolor(COR_CONS)
        @ 24,00 say "INCLUS"+chr(142)+"O"
        @ 24,73 say "ESC-FIM"
        setcolor(COR_TEXT)

        @ 08,12 say strzero(WSEQ,6)
        @ 10,12 get WANIMAL  picture "@!"         valid busca_animal("LEITE") 
        @ 12,12 get WDATA                         valid ! empty(WDATA)

        select ANIMAL
        ordsetfocus('SINC001')
        seek WANIMAL
 
        if found()
           @ 10,21 say NOME
        else
           @ 10,21 say space(20)
        endif
  
        select SECA
        read_f()

        if lastkey() == 27
           return .t.
        endif

        if confirma(24,"Dados corretos...(S/N)?","S") == "S"

           select SECA
           ordsetfocus('SINC001')
           go bottom

           WSEQ = SEQ + 1

           set deleted off
           go top
           if deleted()
              if ! reglock()
                 loop
              endif
              recall
           else
              if ! inc_reg()
                 loop
              endif
           endif
           set deleted on   

           save_seca()

           setcolor(COR_TEXT+"*+")
           @ 08,12 say Strzero(WSEQ,6)
           setcolor(COR_TEXT)

           inkey(0)

           Exit

        endif

    enddo

enddo

return .t.


*****************
function ALT_SECA
*****************

while .t. 

   WSEQ  = 0

   sc_seca()

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "ALTERA"+chr(128)+chr(142)+"O"
   setcolor(COR_TEXT)

   @ 08,12 get WSEQ picture "@Z 999999" 
   read_f()

   if empty(WSEQ) .or. lastkey() == 27
      return .t.
   endif

   select SECA
   ordsetfocus('SINC001')
   seek WSEQ

   if ! found()
      @ 24,10 clear to 24,79
      centro(24,"Sequ"+Chr(136)+"ncia de controle n"+chr(132)+"o cadastrada...")
      inkey(1)
      @ 24,10 clear to 24,79
      loop
   endif

   WANIMAL = ANIMAL      
   WDATA   = DATA        

   while .t.

      sc_seca()

      setcolor(COR_CONS)
      @ 24,00 say "ALTERA"+chr(138)+chr(142)+"O"
      @ 24,73 say "ESC-FIM"
      setcolor(COR_TEXT)

      @ 08,12 say Strzero(WSEQ,6)
      @ 10,12 get WANIMAL picture "@!"  valid busca_animal("LEITE")
      @ 12,12 get WDATA                 valid ! empty(WDATA)

      select ANIMAL
      ordsetfocus('SINC001')
      seek WANIMAL
      
      if found()
         @ 10,21 say NOME
      else
         @ 10,21 say space(20)
      endif
      
      select SECA
      read_f()

      if lastkey() == 27
         return .t.
      endif

      if confirma(24,"Dados corretos...(S/N)?","S") == "S"
         save_seca()
         exit
      endif

   enddo

enddo

return .t.


*****************
function CON_SECA
*****************

while .t.

   OPC4 = 5

   Setcolor(COR_TEXT)
   @ 05,00 clear to 22,79
   @ 24,00 clear to 24,79

   setcolor(COR_CONS)
   @ 24,00 say "CONSULTA"
   @ 24,73 say "ESC-FIM"
   setcolor(COR_TEXT)

   SOMBRA(09,14,17,67,8)

   setcolor(COR_CONS)
   @ 08,12 clear to 16,66
   @ 08,12 to 16,66 

   @ 09,14 say "Consulta a partir do in"+chr(161)+"cio do arquivo..........[1]"
   @ 10,14 say "Consulta a partir do final do arquivo...........[2]"
   @ 11,14 say "Consulta a partir da sequ"+Chr(136)+"ncia de controle......[3]"
   @ 12,14 say "Consulta a partir do animal.....................[4]"
   @ 13,14 say "Retorno ao menu principal.......................[5]"

   setcolor(COR_TEXT)
   @ 15,14 say "Escolha sua op"+chr(135)+chr(132)+"o de consulta...................[ ]"
   @ 15,63 get OPC4 picture "9" range 1,5
   read_f()

   if OPC4 == 5
      return .t.
   endif

   while .t.

      sc_seca()

      if OPC4 == 1
         select SECA
         ordsetfocus('SINC001')
         go top

      elseif OPC4 == 2
         select SECA
         ordsetfocus('SINC001')
         go bottom

      elseif OPC4 == 3
         WSEQ  = 0          

         @ 08,12 get WSEQ picture "@Z 999999" 
         read_f()

         if lastkey() == 27
            exit
         endif

         select SECA
         ordsetfocus('SINC001')
         seek WSEQ  

         if ! found()
            @ 24,10 clear to 24,79
            centro(24,"Sequ"+Chr(136)+"ncia de controle n"+chr(132)+"o cadastrada...")
            inkey(1)
            @ 24,10 clear to 24,79
            loop
         endif

      elseif OPC4 == 4

         WANIMAL = space(06)          
         WDATA   = ctod(space(08))

         @ 10,12 get WANIMAL picture "@!" valid busca_animal("LEITE")
         @ 12,12 get WDATA                                
         read_f()

         if lastkey() == 27
            exit
         endif

         select SECA
         ordsetfocus('SINC002')
         seek WANIMAL+dtos(WDATA)

         If EOF()
            Skip -1
         Endif

      endif

      centro(24,"PgDw-Pr"+chr(162)+"ximo   PgUp-Anterior")

      KEY = 0

      while KEY  <> 27

         @ 08,12 say Strzero(SEQ,6)
         @ 10,12 say ANIMAL picture "@!"     
         @ 12,12 say DATA                               

         WANIMAL = ANIMAL

         select ANIMAL
         ordsetfocus('SINC001')
         seek WANIMAL
         
         if found()
            @ 10,21 say NOME
         else
            @ 10,21 say space(20)
         endif
         
         select SECA

         KEY = inkey(0)

         view_user()

         if KEY == -7
            scalc()
         elseif KEY == -6
            calendario()
         elseif KEY == 3
            skip +1
            if eof()
               skip -1
            endif
         elseif KEY == 18
            skip -1
         elseif KEY == 27
            @ 24,10 clear to 24,70
            exit
         endif
      enddo

      if OPC4 == 1 .or. OPC4 == 2 
         exit
      endif
   enddo
enddo

return .t.


*****************
function EXC_SECA
*****************

while .t. 

   WSEQ = 0

   sc_seca()

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "EXCLUS"+chr(142)+"O"
   setcolor(COR_TEXT)

   @ 08,12 get WSEQ picture "@Z 999999"
   read_f()

   @ 24,10 clear to 24,79

   if empty(WSEQ) .or. lastkey() == 27
      return .t.
   endif

   select SECA
   ordsetfocus('SINC001')
   seek WSEQ

   if ! found()
      @ 24,10 clear to 24,79
      centro(24,"Sequ"+Chr(136)+"ncia de controle n"+chr(132)+"o cadastrada...")
      inkey(1)
      @ 24,10 clear to 24,79
      loop
   endif

   @ 08,12 say Strzero(SEQ,6)
   @ 10,12 say ANIMAL picture "@!"     
   @ 12,12 say DATA                               

   WANIMAL = ANIMAL

   select ANIMAL
   ordsetfocus('SINC001')
   seek WANIMAL
   
   if found()
      @ 10,21 say NOME
   else
      @ 10,21 say space(20)
   endif
   
   select SECA

   if confirma(24,"Confirma EXCLUS"+chr(142)+"O...(S/N)?","N") == "S"

      if ! reglock()
         loop
      endif                         

      replace SEQ     with 0              ,;        
              ANIMAL  with space(06)      ,;        
              DATA    with ctod(space(08)),;   
              USUARIO with space(20)      ,;
              DATE    with ctod(space(08)),;
              TIME    with space(08)

      delete

      grava()
   endif
enddo

return .t.


******************
function SAVE_SECA
******************

if ! reglock()
   return .f.
endif
   
replace SEQ     with WSEQ     ,;    
        ANIMAL  with WANIMAL  ,; 
        DATA    with WDATA

replace USUARIO with WPUSUARIO,;
        TIME    with time()   ,;
        DATE    with date()

grava()

return .t.


*******************
Function PENDENCIAS
*******************
OPC3 = 1

while .t.

   setcolor(COR_CAB) 
   @ 03,42 say "PENDENCIAS                         " 
   setcolor(COR_TEXT)

   private OPCAO[5]
   OPCAO[1] = " Inclus"+chr(132)+"o   " 
   OPCAO[2] = " Altera"+chr(135)+chr(132)+"o  " 
   OPCAO[3] = " Consulta   " 
   OPCAO[4] = " Exclus"+chr(132)+"o   " 
   OPCAO[5] = " Esc - Fim  " 

   private ACESSO[5]
   ACESSO[1] = .t.
   ACESSO[2] = .t.
   ACESSO[3] = .t.
   ACESSO[4] = .t.
   ACESSO[5] = .t.

   private LETRA[5]
   LETRA[1] = "I"  
   LETRA[2] = "A"
   LETRA[3] = "C"
   LETRA[4] = "E"
   LETRA[5] = "F"

   private POS[5] 
   POS[1] = 1
   POS[2] = 1
   POS[3] = 1
   POS[4] = 1
   POS[5] = 7

   private OBS_MENU[5] 
   OBS_MENU[1] = "Inclus"+chr(132)+"o de pend"+Chr(136)+"ncias!" 
   OBS_MENU[2] = "Altera"+chr(135)+chr(132)+"o de pend"+Chr(136)+"ncias!" 
   OBS_MENU[3] = "Consulta as pend"+Chr(136)+"ncias!"                         
   OBS_MENU[4] = "Exclus"+chr(132)+"o de pend"+Chr(136)+"ncias!"  
   OBS_MENU[5] = "Retorno ao menu principal!"    

   OPC3 = achoise(OPC3,09,32,15,45,2,OPCAO,ACESSO,LETRA,POS,OBS_MENU,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)
   
   if OPC3 == 0 .or. OPC3 == 5
      return .t.
   elseif OPC3 == 1
      inc_PEND()
   elseif OPC3 == 2
      alt_PEND()
   elseif OPC3 == 3
      con_PEND()  
   elseif OPC3 == 4
      exc_PEND()
   endif

Enddo

Return .T.


****************
Function SC_PEND
****************

@ 05,00 Clear To 22,79
@ 24,00 Clear To 24,79

@ 08,01 say "C"+Chr(162)+"digo...:"
@ 10,01 say "Descri"+Chr(128)+Chr(142)+"o:" 

Return .T.


*****************
Function inc_PEND
*****************

while .t. 

    sc_PEND()

    setcolor(COR_CONS)
    @ 24,00 say "INCLUS"+chr(142)+"O"
    @ 24,73 say "ESC-FIM"
    setcolor(COR_TEXT)

    Select PENDENC
    ordsetfocus('SINC001')
    Go Bottom

    WCODIGO    = CODIGO + 1            
    WDESCRICAO = Space(30)

    @ 08,12 Say Strzero(WCODIGO,4)

    while .t. 

        @ 10,12 get WDESCRICAO
        read_f()

        if lastkey() == 27
           return .t.
        endif

        if confirma(24,"Dados Corretos...(S/N)?","S") == "S"

           select PENDENC
           ordsetfocus('SINC001')
           Go Bottom

           WCODIGO = CODIGO + 1

           set deleted off
           go top
           if deleted()
              if ! reglock()
                 loop
              endif

              recall
           else
              if ! inc_reg()
                 loop
              endif
           endif
           set deleted on

           save_PEND()

           Exit

        Endif

    Enddo

Enddo

Return .T.


*****************
Function alt_PEND
*****************

while .t. 

   sc_PEND()

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "ALTERA"+chr(128)+chr(142)+"O"
   setcolor(COR_TEXT)

   WCODIGO = 0

   @ 08,12 get WCODIGO picture "@Z 9999" 
   read_f()

   if empty(WCODIGO) .or. lastkey() == 27
      return .t.
   endif

   Select PENDENC
   ordsetfocus('SINC001')
   seek WCODIGO

   if ! found()
      centro(24,"C"+chr(162)+"digo n"+chr(132)+"o cadastrado!")
      inkey(5)
      @ 24,10 clear to 24,79
      loop
   endif

   WDESCRICAO = DESCRICAO     

   while .t.

       @ 10,12 get WDESCRICAO
       read_f() 

       if lastkey() == 27
          return .t.
       endif

       if confirma(24,"Dados corretos...(S/N)?","S") == "S"
          save_PEND()
          exit
       endif

   Enddo

Enddo

Return .T.


*****************
Function con_PEND
*****************

while .t.

    OPC4 = 5

    sc_PEND()

    setcolor(COR_CONS)
    @ 24,00 say "CONSULTA"
    @ 24,73 say "ESC-FIM"
    setcolor(COR_TEXT)

    SOMBRA(10,14,18,67,8)

    setcolor(COR_CONS)
    @ 09,12 clear to 17,66
    @ 09,12 to 17,66 

    @ 10,14 say "Consulta a partir do in"+chr(161)+"cio do arquivo..........[1]"
    @ 11,14 say "Consulta a partir do final do arquivo...........[2]"
    @ 12,14 say "Consulta a partir do c"+chr(162)+"digo.....................[3]"
    @ 13,14 say "Consulta a partir da descri"+Chr(135)+Chr(132)+"o..................[4]"
    @ 14,14 say "Retorno ao menu principal.......................[5]"

    setcolor(COR_TEXT)
    @ 16,14 say "Escolha sua op"+chr(135)+chr(132)+"o de consulta...................[ ]"
    @ 16,63 get OPC4 picture "9" range 1,5
    read_f()

    if OPC4 == 5
       return .t.
    endif

    while .t.

      sc_PEND()

      if OPC4 == 1
         select PENDENC
         ordsetfocus('SINC001')
         go top

      elseif OPC4 == 2
         select PENDENC
         ordsetfocus('SINC001')
         go bottom

      elseif OPC4 == 3

         WCODIGO = 0

         @ 08,12 get WCODIGO picture "@Z 9999"
         read_f()

         if lastkey() == 27
            exit
         endif

         select PENDENC
         ordsetfocus('SINC001')
         seek WCODIGO

         if ! found()
            centro(24,"C"+chr(162)+"digo n"+chr(132)+"o cadastrado!")
            inkey(5)
            @ 24,10 clear to 24,79
            loop
         endif

      elseif OPC4 == 4

         WDESCRICAO = Space(30)

         @ 10,12 get WDESCRICAO
         read_f()

         if lastkey() == 27
            exit
         endif

         select PENDENC
         ordsetfocus('SINC002')
         seek upper(WDESCRICAO)

      endif

      centro(24,"PgDw-Pr"+chr(162)+"ximo   PgUp-Anterior")

      KEY = 0

      while KEY  <> 27

         @ 08,12 say Strzero(CODIGO,4)
         @ 10,12 say DESCRICAO

         KEY = inkey(0)

         view_USER()

         if KEY == -7
            scalc()
         elseif KEY == -6
            calendario()
         elseif KEY == 3
            skip 
            if eof()
               skip -1
            endif
         elseif KEY == 18
            skip -1
         elseif KEY == 27
            exit
         endif
      enddo

      if OPC4 == 1 .or. OPC4 == 2 
         exit
      endif

   enddo

enddo

return .t.


*****************
Function exc_PEND
*****************

while .t. 

   sc_PEND()

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "EXCLUS"+chr(142)+"O"
   setcolor(COR_TEXT)

   WCODIGO = 0

   @ 08,12 get WCODIGO picture "@Z 9999"
   read_f()
   @ 08,12 say Strzero(WCODIGO,4)

   if empty(WCODIGO) .or. lastkey() == 27
      return .t.
   endif

   select PENDENC
   ordsetfocus('SINC001')
   seek WCODIGO

   if ! found()
      centro(24,"C"+chr(162)+"digo n"+chr(132)+"o cadastrado!")
      inkey(5)
      @ 24,10 clear to 24,79
      loop
   endif

   @ 10,12 say DESCRICAO

   if confirma(24,"Confirma EXCLUS"+chr(142)+"O...(S/N)?","N") == "S"

      select PENDENC

      if ! reglock()
         loop
      endif                         

      replace CODIGO    with 0        ,;
              DESCRICAO with Space(30),;
              USUARIO   with Space(20),;
              DATE      with Ctod(Space(08)),;
              TIME      with Space(08)

      delete

      grava()

   Endif

Enddo

Return .T.


******************
Function save_PEND
******************

Select PENDENC

If ! REGLOCK()
   Return .F.
Endif
   
Replace CODIGO    With WCODIGO   ,;    
        DESCRICAO With WDESCRICAO,;
        USUARIO   With WPUSUARIO ,;
        TIME      With TIME()    ,;
        DATE      With DATE()

GRAVA()

Return .T.


***************
Function GRUPOS
***************
OPC3 = 1

while .t.

   setcolor(COR_CAB) 
   @ 03,42 say "GRUPOS                             " 
   setcolor(COR_TEXT)

   private OPCAO[5]
   OPCAO[1] = " Inclus"+chr(132)+"o   " 
   OPCAO[2] = " Altera"+chr(135)+chr(132)+"o  " 
   OPCAO[3] = " Consulta   " 
   OPCAO[4] = " Exclus"+chr(132)+"o   " 
   OPCAO[5] = " Esc - Fim  " 

   private ACESSO[5]
   ACESSO[1] = .t.
   ACESSO[2] = .t.
   ACESSO[3] = .t.
   ACESSO[4] = .t.
   ACESSO[5] = .t.

   private LETRA[5]
   LETRA[1] = "I"  
   LETRA[2] = "A"
   LETRA[3] = "C"
   LETRA[4] = "E"
   LETRA[5] = "F"

   private POS[5] 
   POS[1] = 1
   POS[2] = 1
   POS[3] = 1
   POS[4] = 1
   POS[5] = 7

   private OBS_MENU[5] 
   OBS_MENU[1] = "Inclus"+chr(132)+"o de grupos!" 
   OBS_MENU[2] = "Altera"+chr(135)+chr(132)+"o de grupos!" 
   OBS_MENU[3] = "Consulta aos grupos!"                         
   OBS_MENU[4] = "Exclus"+chr(132)+"o de grupos!"  
   OBS_MENU[5] = "Retorno ao menu principal!"    

   OPC3 = achoise(OPC3,09,32,15,45,2,OPCAO,ACESSO,LETRA,POS,OBS_MENU,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)
   
   if OPC3 == 0 .or. OPC3 == 5
      return .t.
   elseif OPC3 == 1
      inc_GRUPOS()
   elseif OPC3 == 2
      alt_GRUPOS()
   elseif OPC3 == 3
      con_GRUPOS()  
   elseif OPC3 == 4
      exc_GRUPOS()
   endif

Enddo

Return .T.


******************
Function SC_GRUPOS
******************

@ 05,00 Clear To 22,79
@ 24,00 Clear To 24,79

@ 08,01 say "C"+Chr(162)+"digo...:"
@ 10,01 say "Descri"+Chr(128)+Chr(142)+"o:" 

Return .T.


*******************
Function inc_GRUPOS
*******************

while .t. 

    sc_GRUPOS()

    setcolor(COR_CONS)
    @ 24,00 say "INCLUS"+chr(142)+"O"
    @ 24,73 say "ESC-FIM"
    setcolor(COR_TEXT)

    Select GRUPO
    ordsetfocus('SINC001')

    WCODIGO    = Space(03)             
    WDESCRICAO = Space(30)

    While .T. 

        @ 08,12 Get WCODIGO Picture "@!" Valid seek_GRUPO()
        @ 10,12 Get WDESCRICAO
        read_f()

        If Lastkey() == 27
           Return .T.
        Endif

        If CONFIRMA(24,"Dados Corretos...(S/N)?","S") == "S"

           Select GRUPO

           set deleted off
           go top
           if deleted()
              if ! reglock()
                 loop
              endif

              recall
           else
              if ! inc_reg()
                 loop
              endif
           endif
           set deleted on

           save_GRUPOS()

           Exit

        Endif

    Enddo

Enddo

Return .T.


*******************
Function seek_GRUPO
*******************

If Lastkey() = 27
   Return .T.
Endif

If Empty(WCODIGO)
   Return .F.
Endif

Select GRUPO
ordsetfocus('SINC001')
Seek WCODIGO

If Found()
   centro(24,"C"+chr(162)+"digo de grupo j"+chr(160)+" cadastrado!")
   inkey(5)
   @ 24,10 clear to 24,79
   Return .F.
Endif

Return .T.


*******************
Function alt_GRUPOS
*******************

while .t. 

   sc_GRUPOS()

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "ALTERA"+chr(128)+chr(142)+"O"
   setcolor(COR_TEXT)

   WCODIGO = Space(03)

   @ 08,12 get WCODIGO picture "@!" 
   read_f()

   if empty(WCODIGO) .or. lastkey() == 27
      return .t.
   endif

   Select GRUPO
   ordsetfocus('SINC001')
   seek WCODIGO

   if ! found()
      centro(24,"C"+chr(162)+"digo de grupo n"+chr(132)+"o cadastrado!")
      inkey(5)
      @ 24,10 clear to 24,79
      loop
   endif

   WDESCRICAO = DESCRICAO     

   while .t.

       @ 10,12 get WDESCRICAO
       read_f() 

       if lastkey() == 27
          return .t.
       endif

       if confirma(24,"Dados corretos...(S/N)?","S") == "S"
          save_GRUPOS()
          exit
       endif

   Enddo

Enddo

Return .T.


*******************
Function con_GRUPOS
*******************

while .t.

    OPC4 = 5

    sc_GRUPOS()

    setcolor(COR_CONS)
    @ 24,00 say "CONSULTA"
    @ 24,73 say "ESC-FIM"
    setcolor(COR_TEXT)

    SOMBRA(10,14,18,67,8)

    setcolor(COR_CONS)
    @ 09,12 clear to 17,66
    @ 09,12 to 17,66 

    @ 10,14 say "Consulta a partir do in"+chr(161)+"cio do arquivo..........[1]"
    @ 11,14 say "Consulta a partir do final do arquivo...........[2]"
    @ 12,14 say "Consulta a partir do c"+chr(162)+"digo.....................[3]"
    @ 13,14 say "Consulta a partir da descri"+Chr(135)+Chr(132)+"o..................[4]"
    @ 14,14 say "Retorno ao menu principal.......................[5]"

    setcolor(COR_TEXT)
    @ 16,14 say "Escolha sua op"+chr(135)+chr(132)+"o de consulta...................[ ]"
    @ 16,63 get OPC4 picture "9" range 1,5
    read_f()

    if OPC4 == 5
       return .t.
    endif

    while .t.

      sc_GRUPOS()

      if OPC4 == 1
         select GRUPO
         ordsetfocus('SINC001')
         go top

      elseif OPC4 == 2
         select GRUPO
         ordsetfocus('SINC001')
         go bottom

      elseif OPC4 == 3

         WCODIGO = Space(03)

         @ 08,12 get WCODIGO picture "@!"
         read_f()

         if lastkey() == 27
            exit
         endif

         select GRUPO
         ordsetfocus('SINC001')
         seek WCODIGO

         if ! found()
            centro(24,"C"+chr(162)+"digo de grupo n"+chr(132)+"o cadastrado!")
            inkey(5)
            @ 24,10 clear to 24,79
            loop
         endif

      elseif OPC4 == 4

         WDESCRICAO = Space(30)

         @ 10,12 get WDESCRICAO
         read_f()

         if lastkey() == 27
            exit
         endif

         select GRUPO
         ordsetfocus('SINC002')
         seek upper(WDESCRICAO)

      endif

      centro(24,"PgDw-Pr"+chr(162)+"ximo   PgUp-Anterior")

      KEY = 0

      while KEY  <> 27

         @ 08,12 say CODIGO
         @ 10,12 say DESCRICAO

         KEY = inkey(0)

         view_USER()

         if KEY == -7
            scalc()
         elseif KEY == -6
            calendario()
         elseif KEY == 3
            skip 
            if eof()
               skip -1
            endif
         elseif KEY == 18
            skip -1
         elseif KEY == 27
            exit
         endif
      enddo

      if OPC4 == 1 .or. OPC4 == 2 
         exit
      endif

   enddo

enddo

return .t.


*******************
Function exc_GRUPOS
*******************

while .t. 

   sc_GRUPOS()

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "EXCLUS"+chr(142)+"O"
   setcolor(COR_TEXT)

   WCODIGO = Space(03)

   @ 08,12 get WCODIGO picture "@!"
   read_f()

   if empty(WCODIGO) .or. lastkey() == 27
      return .t.
   endif

   select GRUPO
   ordsetfocus('SINC001')
   seek WCODIGO

   if ! found()
      centro(24,"C"+chr(162)+"digo de grupo n"+chr(132)+"o cadastrado!")
      inkey(5)
      @ 24,10 clear to 24,79
      loop
   endif

   @ 10,12 say DESCRICAO

   if confirma(24,"Confirma EXCLUS"+chr(142)+"O...(S/N)?","N") == "S"

      select GRUPO

      if ! reglock()
         loop
      endif                         

      replace CODIGO    with Space(03),;
              DESCRICAO with Space(30),;
              USUARIO   with Space(20),;
              DATE      with Ctod(Space(08)),;
              TIME      with Space(08)

      delete

      grava()

   Endif

Enddo

Return .T.


********************
Function save_GRUPOS
********************

Select GRUPO

If ! REGLOCK()
   Return .F.
Endif
   
Replace CODIGO    With WCODIGO   ,;    
        DESCRICAO With WDESCRICAO,;
        USUARIO   With WPUSUARIO ,;
        TIME      With TIME()    ,;
        DATE      With DATE()

GRAVA()

Return .T.


**************
Function LOTES
**************
OPC3 = 1

while .t.

   setcolor(COR_CAB) 
   @ 03,42 say "SUB-GRUPO                          " 
   setcolor(COR_TEXT)

   private OPCAO[5]
   OPCAO[1] = " Inclus"+chr(132)+"o   " 
   OPCAO[2] = " Altera"+chr(135)+chr(132)+"o  " 
   OPCAO[3] = " Consulta   " 
   OPCAO[4] = " Exclus"+chr(132)+"o   " 
   OPCAO[5] = " Esc - Fim  " 

   private ACESSO[5]
   ACESSO[1] = .t.
   ACESSO[2] = .t.
   ACESSO[3] = .t.
   ACESSO[4] = .t.
   ACESSO[5] = .t.

   private LETRA[5]
   LETRA[1] = "I"  
   LETRA[2] = "A"
   LETRA[3] = "C"
   LETRA[4] = "E"
   LETRA[5] = "F"

   private POS[5] 
   POS[1] = 1
   POS[2] = 1
   POS[3] = 1
   POS[4] = 1
   POS[5] = 7

   private OBS_MENU[5] 
   OBS_MENU[1] = "Inclus"+chr(132)+"o de sub-grupo!" 
   OBS_MENU[2] = "Altera"+chr(135)+chr(132)+"o de sub-grupo!" 
   OBS_MENU[3] = "Consulta aos sub-grupos!"                         
   OBS_MENU[4] = "Exclus"+chr(132)+"o de sub-grupo!"  
   OBS_MENU[5] = "Retorno ao menu principal!"    

   OPC3 = achoise(OPC3,09,32,15,45,2,OPCAO,ACESSO,LETRA,POS,OBS_MENU,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)
   
   if OPC3 == 0 .or. OPC3 == 5
      return .t.
   elseif OPC3 == 1
      inc_LOTES()
   elseif OPC3 == 2
      alt_LOTES()
   elseif OPC3 == 3
      con_LOTES()  
   elseif OPC3 == 4
      exc_LOTES()
   endif

Enddo

Return .T.


*****************
Function SC_LOTES
*****************

@ 05,00 Clear To 22,79
@ 24,00 Clear To 24,79

@ 08,01 say "C"+Chr(162)+"digo...:"
@ 10,01 say "Descri"+Chr(128)+Chr(142)+"o:" 

Return .T.


******************
Function inc_LOTES
******************

while .t. 

    sc_LOTES()

    setcolor(COR_CONS)
    @ 24,00 say "INCLUS"+chr(142)+"O"
    @ 24,73 say "ESC-FIM"
    setcolor(COR_TEXT)

    Select LOTE
    ordsetfocus('SINC001')

    WCODIGO    = Space(03)             
    WDESCRICAO = Space(30)

    While .T. 

        @ 08,12 Get WCODIGO Picture "@!" Valid seek_LOTE()
        @ 10,12 Get WDESCRICAO
        read_f()

        If Lastkey() == 27
           Return .T.
        Endif

        If CONFIRMA(24,"Dados Corretos...(S/N)?","S") == "S"

           Select LOTE

           set deleted off
           go top
           if deleted()
              if ! reglock()
                 loop
              endif

              recall
           else
              if ! inc_reg()
                 loop
              endif
           endif
           set deleted on

           save_LOTES()

           Exit

        Endif

    Enddo

Enddo

Return .T.


******************
Function seek_LOTE
******************

If Lastkey() = 27
   Return .T.
Endif

If Empty(WCODIGO)
   Return .F.
Endif

Select LOTE
ordsetfocus('SINC001')
Seek WCODIGO

If Found()
   centro(24,"C"+chr(162)+"digo de sub-grupo j"+chr(160)+" cadastrado!")
   inkey(5)
   @ 24,10 clear to 24,79
   Return .T.
Endif

Return .T.


******************
Function alt_LOTES
******************

while .t. 

   sc_LOTES()

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "ALTERA"+chr(128)+chr(142)+"O"
   setcolor(COR_TEXT)

   WCODIGO = Space(03)

   @ 08,12 get WCODIGO picture "@!" 
   read_f()

   if empty(WCODIGO) .or. lastkey() == 27
      return .t.
   endif

   Select LOTE
   ordsetfocus('SINC001')
   seek WCODIGO

   if ! found()
      centro(24,"C"+chr(162)+"digo de sub-grupo n"+chr(132)+"o cadastrado!")
      inkey(5)
      @ 24,10 clear to 24,79
      loop
   endif

   WDESCRICAO = DESCRICAO     

   while .t.

       @ 10,12 get WDESCRICAO
       read_f() 

       if lastkey() == 27
          return .t.
       endif

       if confirma(24,"Dados corretos...(S/N)?","S") == "S"
          save_LOTES()
          exit
       endif

   Enddo

Enddo

Return .T.


******************
Function con_LOTES
******************

while .t.

    OPC4 = 5

    sc_LOTES()

    setcolor(COR_CONS)
    @ 24,00 say "CONSULTA"
    @ 24,73 say "ESC-FIM"
    setcolor(COR_TEXT)

    SOMBRA(10,14,18,67,8)

    setcolor(COR_CONS)
    @ 09,12 clear to 17,66
    @ 09,12 to 17,66 

    @ 10,14 say "Consulta a partir do in"+chr(161)+"cio do arquivo..........[1]"
    @ 11,14 say "Consulta a partir do final do arquivo...........[2]"
    @ 12,14 say "Consulta a partir do c"+chr(162)+"digo.....................[3]"
    @ 13,14 say "Consulta a partir da descri"+Chr(135)+Chr(132)+"o..................[4]"
    @ 14,14 say "Retorno ao menu principal.......................[5]"

    setcolor(COR_TEXT)
    @ 16,14 say "Escolha sua op"+chr(135)+chr(132)+"o de consulta...................[ ]"
    @ 16,63 get OPC4 picture "9" range 1,5
    read_f()

    if OPC4 == 5
       return .t.
    endif

    while .t.

      sc_LOTES()

      if OPC4 == 1
         select LOTE 
         ordsetfocus('SINC001')
         go top

      elseif OPC4 == 2
         select LOTE
         ordsetfocus('SINC001')
         go bottom

      elseif OPC4 == 3

         WCODIGO = Space(03)

         @ 08,12 get WCODIGO picture "@!"
         read_f()

         if lastkey() == 27
            exit
         endif

         select LOTE
         ordsetfocus('SINC001')
         seek WCODIGO

         if ! found()
            centro(24,"C"+chr(162)+"digo de lote n"+chr(132)+"o cadastrado!")
            inkey(5)
            @ 24,10 clear to 24,79
            loop
         endif

      elseif OPC4 == 4

         WDESCRICAO = Space(30)

         @ 10,12 get WDESCRICAO
         read_f()

         if lastkey() == 27
            exit
         endif

         select LOTE
         ordsetfocus('SINC002')
         seek upper(WDESCRICAO)

      endif

      centro(24,"PgDw-Pr"+chr(162)+"ximo   PgUp-Anterior")

      KEY = 0

      while KEY  <> 27

         @ 08,12 say CODIGO
         @ 10,12 say DESCRICAO

         KEY = inkey(0)

         view_USER()

         if KEY == -7
            scalc()
         elseif KEY == -6
            calendario()
         elseif KEY == 3
            skip 
            if eof()
               skip -1
            endif
         elseif KEY == 18
            skip -1
         elseif KEY == 27
            exit
         endif
      enddo

      if OPC4 == 1 .or. OPC4 == 2 
         exit
      endif

   enddo

enddo

return .t.


******************
Function exc_LOTES
******************

while .t. 

   sc_LOTES()

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "EXCLUS"+chr(142)+"O"
   setcolor(COR_TEXT)

   WCODIGO = Space(03)

   @ 08,12 get WCODIGO picture "@!"
   read_f()

   if empty(WCODIGO) .or. lastkey() == 27
      return .t.
   endif

   select LOTE
   ordsetfocus('SINC001')
   seek WCODIGO

   if ! found()
      centro(24,"C"+chr(162)+"digo de sub-grupo n"+chr(132)+"o cadastrado!")
      inkey(5)
      @ 24,10 clear to 24,79
      loop
   endif

   @ 10,12 say DESCRICAO

   if confirma(24,"Confirma EXCLUS"+chr(142)+"O...(S/N)?","N") == "S"

      select LOTE

      if ! reglock()
         loop
      endif                         

      replace CODIGO    with Space(06),;
              DESCRICAO with Space(30),;
              USUARIO   with Space(20),;
              DATE      with Ctod(Space(08)),;
              TIME      with Space(08)

      delete

      grava()

   Endif

Enddo

Return .T.


*******************
Function save_LOTES
*******************

Select LOTE

If ! REGLOCK()
   Return .F.
Endif
   
Replace CODIGO    With WCODIGO   ,;    
        DESCRICAO With WDESCRICAO,;
        USUARIO   With WPUSUARIO ,;
        TIME      With TIME()    ,;
        DATE      With DATE()

GRAVA()

Return .T.


**************
Function BANCO
**************
OPC3 = 1

while .t.

   setcolor(COR_CAB) 
   @ 03,42 say "BANCO DE S"+Chr(136)+"MEN                     " 
   setcolor(COR_TEXT)

   private OPCAO[5]
   OPCAO[1] = " Inclus"+chr(132)+"o   " 
   OPCAO[2] = " Altera"+chr(135)+chr(132)+"o  " 
   OPCAO[3] = " Consulta   " 
   OPCAO[4] = " Exclus"+chr(132)+"o   " 
   OPCAO[5] = " Esc - Fim  " 

   private ACESSO[5]
   ACESSO[1] = .T.
   ACESSO[2] = .T.
   ACESSO[3] = .T.
   ACESSO[4] = .T.
   ACESSO[5] = .T.

   private LETRA[5]
   LETRA[1] = "I"  
   LETRA[2] = "A"
   LETRA[3] = "C"
   LETRA[4] = "E"
   LETRA[5] = "F"

   private POS[5] 
   POS[1] = 1
   POS[2] = 1
   POS[3] = 1
   POS[4] = 1
   POS[5] = 7

   private OBS_MENU[5]  
   OBS_MENU[1] = "Inclus"+chr(132)+"o banco de s"+Chr(136)+"men!" 
   OBS_MENU[2] = "Altera"+chr(135)+chr(132)+"o banco de s"+Chr(136)+"men!"
   OBS_MENU[3] = "Consulta banco de s"+Chr(136)+"men!"                    
   OBS_MENU[4] = "Exclus"+chr(132)+"o banco de s"+Chr(136)+"men!"  
   OBS_MENU[5] = "Retorno ao menu principal!"            

   OPC3 = achoise(OPC3,10,32,16,45,2,OPCAO,ACESSO,LETRA,POS,OBS_MENU,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)
   
   If OPC3 == 0 .Or. OPC3 == 5
      Return .T.
   Elseif OPC3 == 1
      inc_BANCO()
   Elseif OPC3 == 2
      alt_BANCO()
   Elseif OPC3 == 3
      con_BANCO()
   Elseif OPC3 == 4
      exc_BANCO()
   Endif

Enddo

Return .T.


*****************
Function sc_BANCO
*****************

@ 05,00 Clear To 22,79
@ 24,00 Clear To 24,79

@ 06,01 say "Lote......:"
@ 08,01 say "Data......:" 
@ 10,01 say "Doador....:"
@ 12,01 say "Observa"+Chr(135)+Chr(132)+"o:"
@ 13,01 say "          :"
@ 14,01 say "          :"
@ 15,01 say "          :"
@ 17,01 say "Quantidade:"
@ 19,01 say "Baixa (IA):"
@ 21,01 say "Saldo.....:"

Return .T.


******************
Function inc_BANCO
******************

While .T. 

    Select BANCO

    sc_BANCO()

    setcolor(COR_CONS)
    @ 24,00 say "INCLUS"+chr(142)+"O"
    @ 24,73 say "ESC-FIM"
    setcolor(COR_TEXT)

    WLOTE   = Space(06)
    WDATA   = DATE()   
    WDOADOR = Space(40)
    WOBS1   = Space(40)
    WOBS2   = Space(40)
    WOBS3   = Space(40)
    WOBS4   = Space(40)
    WQTDE   = 0
    WBAIXA  = 0

    While .T.

        @ 06,13 Get WLOTE   Picture "@!" Valid view_LOTE()              
        @ 08,13 Get WDATA                Valid ! Empty(WDATA)
        @ 10,13 Get WDOADOR              Valid ! Empty(WDOADOR)
        @ 12,13 Get WOBS1 
        @ 13,13 Get WOBS2 
        @ 14,13 Get WOBS3 
        @ 15,13 Get WOBS4 
        @ 17,13 Get WQTDE            Picture "9999"
        @ 19,13 Get WBAIXA           Picture "9999"
        READ_F()

        if lastkey() == 27
           return .t.
        endif

        @ 21,13 Say (WQTDE - WBAIXA) Picture "9999"

        if confirma(24,"Dados Corretos...(S/N)?","S") == "S"

           select BANCO

           set deleted off
           go top
           if deleted()
              if ! reglock()
                 loop
              endif

              recall
           else
              if ! inc_reg()
                 loop
              endif
           endif
           set deleted on

           save_BANCO()

           Exit

        endif

    Enddo

Enddo

Return .T.


******************
Function view_LOTE
******************

If Lastkey() = 27
   Return .T.
Endif

If Empty(WLOTE)
   Return .F.
Endif

Select BANCO
ordsetfocus('SINC001')
Seek WLOTE

If Found()
   @ 24,10 Clear To 24,79
   CENTRO(24,"N"+Chr(163)+"mero de lote j"+Chr(160)+" cadastrado...")
   Inkey(5)
   Return .F.
Endif

Return .T.


******************
Function alt_BANCO
******************

While .T. 

   sc_BANCO()

   WLOTE = Space(06)

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "ALTERA"+chr(128)+chr(142)+"O"
   setcolor(COR_TEXT)

   @ 06,13 Get WLOTE Picture "@!"                                
   READ_F()

   @ 24,10 Clear To 24,79

   If Empty(WLOTE) .Or. Lastkey() == 27
      Return .T.
   Endif

   Select BANCO
   ordsetfocus('SINC001')
   Seek WLOTE

   If ! Found()
      @ 24,10 Clear To 24,79
      CENTRO(24,"N"+Chr(163)+"mero de lote n"+Chr(132)+"o cadastrado...")
      Inkey(5)
      Loop
   Endif
   
   WDATA   = DATA
   WDOADOR = DOADOR
   WOBS1   = OBS1
   WOBS2   = OBS2
   WOBS3   = OBS3
   WOBS4   = OBS4
   WQTDE   = QTDE
   WBAIXA  = BAIXA

   while .t.

      setcolor(COR_CONS)
      @ 24,00 say "ALTERA"+chr(138)+chr(142)+"O"
      @ 24,73 say "ESC-FIM"
      setcolor(COR_TEXT)

      @ 08,13 Get WDATA                Valid ! Empty(WDATA)
      @ 10,13 Get WDOADOR              Valid ! Empty(WDOADOR)
      @ 12,13 Get WOBS1 
      @ 13,13 Get WOBS2 
      @ 14,13 Get WOBS3 
      @ 15,13 Get WOBS4 
      @ 17,13 Get WQTDE            Picture "9999"
      @ 19,13 Get WBAIXA           Picture "9999"
      READ_F()

      if lastkey() == 27
         return .t.
      endif

      @ 21,13 Say (WQTDE - WBAIXA) Picture "9999"

      if confirma(24,"Dados Corretos...(S/N)?","S") == "S"
         select BANCO
         save_BANCO()
         exit
      endif

   enddo

enddo

return .t.


******************
Function con_BANCO
******************

While .T.

   @ 05,00 clear to 22,79
   @ 24,00 clear to 24,79

   setcolor(COR_CONS)
   @ 24,00 say "CONSULTA"
   @ 24,73 say "ESC-FIM"
   setcolor(COR_TEXT)

   OPC4 = 6

   SOMBRA(09,14,18,67,8)

   setcolor(COR_CONS)
   @ 08,12 clear to 17,66
   @ 08,12 to 17,66 

   @ 09,14 say "Consulta a partir do in"+chr(161)+"cio do arquivo..........[1]"
   @ 10,14 say "Consulta a partir do final do arquivo...........[2]"
   @ 11,14 say "Consulta a partir do n"+Chr(163)+"mero de lote.............[3]"
   @ 12,14 say "Consulta a partir da doador.....................[4]"
   @ 13,14 say "Consulta a partir da data do lote...............[5]"
   @ 14,14 say "Retorno ao menu principal.......................[6]"

   setcolor(COR_TEXT)
   @ 16,14 say "Escolha sua op"+chr(135)+chr(132)+"o de consulta...................[ ]"
   @ 16,63 get OPC4 picture "9" range 1,6
   read_f()

   if OPC4 == 6
      return .t.
   endif

   while .t.

      sc_BANCO()

      if OPC4 == 1
         select BANCO
         ordsetfocus('SINC001')
         go top

      elseif OPC4 == 2
         select BANCO
         ordsetfocus('SINC001')
         go bottom

      elseif OPC4 == 3

         WLOTE = Space(06)  

         @ 06,13 Get WLOTE Picture "@!" 
         read_f()

         if lastkey() == 27
            exit
         endif

         select BANCO
         ordsetfocus('SINC001')
         seek WLOTE

         if ! found()
            CENTRO(24,"N"+Chr(163)+"mero de lote n"+Chr(132)+"o cadastrado...")
            inkey(5)
            loop
         endif

      elseif OPC4 == 4

         WDOADOR = Space(40)

         @ 10,13 Get WDOADOR      
         read_f()

         if lastkey() == 27
            exit
         endif

         select BANCO
         ordsetfocus('SINC003')
         seek Upper(WDOADOR)

         If EOF()
            Skip -1
         Endif

      elseif OPC4 == 5

         WDATA = DATE()          

         @ 08,13 Get WDATA         
         read_f()

         if lastkey() == 27
            exit
         endif

         select BANCO
         ordsetfocus('SINC002')
         seek WDATA                    

         If EOF()
            Skip -1
         Endif

      endif

      centro(24,"PgDw-Pr"+chr(162)+"ximo   PgUp-Anterior")

      KEY = 0

      while KEY  <> 27

         @ 06,13 Say LOTE           Picture "@!" 
         @ 08,13 Say DATA              
         @ 10,13 Say DOADOR            
         @ 12,13 Say OBS1 
         @ 13,13 Say OBS2 
         @ 14,13 Say OBS3 
         @ 15,13 Say OBS4 
         @ 17,13 Say QTDE           Picture "9999"
         @ 19,13 Say BAIXA          Picture "9999"  
         @ 21,13 Say (QTDE - BAIXA) Picture "9999"

         KEY = inkey(0)

         view_USER()

         if KEY == -7
            scalc()
         elseif KEY == -6
            calendario()
         elseif KEY == 3
            skip 
            if eof()
               skip -1
            endif
         elseif KEY == 18
            skip -1
         elseif KEY == 27
            @ 24,10 clear to 24,70
            exit
         endif

      enddo

      if OPC4 == 1 .or. OPC4 == 2 
         exit
      endif

   enddo

enddo

return .t.


******************
Function exc_BANCO
******************

while .t. 

   sc_BANCO()

   WLOTE = Space(06)

   setcolor(COR_CONS)
   @ 24,73 say "ESC-FIM"
   @ 24,00 say "EXCLUS"+chr(142)+"O"
   setcolor(COR_TEXT)

   @ 06,13 Get WLOTE Picture "@!" 
   read_f()

   @ 24,10 clear to 24,79

   if empty(WLOTE) .or. lastkey() == 27
      return .t.
   endif

   select BANCO
   ordsetfocus('SINC001')
   seek WLOTE

   if ! found()
      CENTRO(24,"N"+Chr(163)+"mero de lote n"+Chr(132)+"o cadastrado...")
      inkey(5)
      loop
   endif

   @ 08,13 Say DATA              
   @ 10,13 Say DOADOR            
   @ 12,13 Say OBS1 
   @ 13,13 Say OBS2 
   @ 14,13 Say OBS3 
   @ 15,13 Say OBS4 
   @ 17,13 Say QTDE           Picture "9999"
   @ 19,13 Say BAIXA          Picture "9999"  
   @ 21,13 Say (QTDE - BAIXA) Picture "9999"

   if confirma(24,"Confirma EXCLUS"+chr(142)+"O...(S/N)?","N") == "S"

      if ! reglock()
         loop
      endif                         

      replace LOTE    with Space(06)      ,;        
              DATA    with ctod(space(08)),;   
              DOADOR  with space(40)      ,;
              OBS1    with space(40)      ,;
              OBS2    with space(40)      ,;
              OBS3    with space(40)      ,;
              OBS4    with space(40)      ,;
              QTDE    with 0              ,;
              BAIXA   with 0              ,;
              USUARIO with space(20)      ,;
              DATE    with ctod(space(08)),;
              TIME    with space(08)

      delete

      grava()

   endif

enddo

return .t.


*******************
Function save_BANCO
*******************

Select BANCO

if ! reglock()
   return .f.
endif
   
replace LOTE    with WLOTE    ,;    
        DATA    with WDATA    ,;
        DOADOR  with WDOADOR  ,;
        OBS1    with WOBS1    ,;
        OBS2    with WOBS2    ,;
        OBS3    with WOBS3    ,;
        OBS4    with WOBS4    ,;
        QTDE    with WQTDE    ,;
        BAIXA   with WBAIXA   ,;
        USUARIO with WPUSUARIO,;
        TIME    with TIME()   ,;
        DATE    with DATE()

GRAVA()

Return .T.

