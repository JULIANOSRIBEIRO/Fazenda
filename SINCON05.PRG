******************************************************************************
***  Sistema......: SINCON - Sistema Integrado Controle                    ***
***  Programa.....: SINCON05.PRG                                           ***
***  Descrição....: Módulo ESTOQUE                                         ***
***  Programador..: Juliano Ribeiro                                        ***
***                                                                        ***
***                     M U L T I  -  U S U A R I O                        ***
***                                                                        ***
***                    Copyright(C) SERIAL SOFTWARE.                       ***
***                                                                        ***
***  Compilador...: CLIPPER (R) Versão 5.01                                ***
***  Linkeditor...: RTLink (R)  Versão 3.13                                ***
***                                                                        ***
******************************************************************************

****************
function ESTOQUE
****************
WIN40 = 1

While .T.

    Setcolor(COR_CAB)   
    @ 03,42 Say "ESTOQUE                            " 
    Setcolor(COR_TEXT)

    Private Opcao[6]

    Opcao[1] = " Grupos              "
    Opcao[2] = " Produtos            "
    Opcao[3] = " Entradas (Compra)   "
    Opcao[4] = " Saidas (Requisi"+Chr(135)+Chr(132)+"o) "
    Opcao[5] = " Relat"+Chr(162)+"rios          "
    Opcao[6] = " Fim                 "

    private ACESSO[6]
    ACESSO[1] = .T.
    ACESSO[2] = .T.
    ACESSO[3] = .T.
    ACESSO[4] = .T.
    ACESSO[5] = .T.
    ACESSO[6] = .T.

    Private Letra[6]

    Letra[1] = "G"  
    Letra[2] = "P"  
    Letra[3] = "E"  
    Letra[4] = "S"  
    Letra[5] = "R"  
    Letra[6] = "F"  

    Private Pos[6]

    Pos[1] = 1
    Pos[2] = 1
    Pos[3] = 1 
    Pos[4] = 1
    Pos[5] = 1
    Pos[6] = 1

    Private Obs[6]

    Obs[1] = "Cadastro de grupos de produtos!" 
    Obs[2] = "Cadastro de produtos!"
    Obs[3] = "Cadastro de entradas (compras)!"
    Obs[4] = "Cadastro de saidas (requisi"+Chr(135)+Chr(148)+"es)!"
    Obs[5] = "Relat"+Chr(162)+"rios de movimenta"+Chr(135)+Chr(132)+"o!"
    Obs[6] = "Retorno ao menu principal!"
 
    WIN40 = ACHOISE(WIN40,08,29,15,51,1,OPCAO,ACESSO,LETRA,POS,OBS,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)

    If WIN40= 0                     
       Return .T.
    Elseif WIN40 = 1 
       GRUPO()
    Elseif WIN40 = 2 
       PRODUTOS()
    Elseif WIN40 = 3 
       ENTRADAS()
    Elseif WIN40 = 4 
       SAIDAS()
    Elseif WIN40 = 5 
       rpr_MOVIMENT()
    Elseif WIN40 = 6
       Return .T.
    Endif

Enddo

Return .T.


**************
Function GRUPO
**************
WIN105 = 1

While .T.

    Setcolor(COR_CAB)   
    @ 03,42 Say "GRUPOS DE PRODUTOS                 " 
    Setcolor(COR_TEXT)

    Private Opcao[5]

    Opcao[1] = " Inclus"+Chr(132)+"o   "  
    Opcao[2] = " Altera"+Chr(135)+Chr(132)+"o  "
    Opcao[3] = " Consulta   "
    Opcao[4] = " Exclus"+Chr(132)+"o   "
    Opcao[5] = " Fim        "  

    private ACESSO[5]
    ACESSO[1] = .T.
    ACESSO[2] = .T.
    ACESSO[3] = .T.
    ACESSO[4] = .T.
    ACESSO[5] = .T.

    Private Letra[5]

    Letra[1] = "I"  
    Letra[2] = "A"
    Letra[3] = "C"
    Letra[4] = "E"
    Letra[5] = "F"  

    Private Pos[5]

    Pos[1] = 1 
    Pos[2] = 1 
    Pos[3] = 1
    Pos[4] = 1
    Pos[5] = 1 

    Private Obs[5]

    Obs[1] = "Inclus"+Chr(132)+"o de grupos!"
    Obs[2] = "Altera"+Chr(135)+Chr(132)+"o de grupos!"
    Obs[3] = "Consulta aos grupos!"
    Obs[4] = "Exclus"+Chr(132)+"o de grupos!"
    Obs[5] = "Retorno ao menu principal!"

    WIN105 = ACHOISE(WIN105,10,33,16,46,2,OPCAO,ACESSO,LETRA,POS,OBS,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)

    If WIN105 = 0 
       Return .T.
    Elseif WIN105 = 1 
       iGRUPOS()
    Elseif WIN105 = 2 
       aGRUPOS(2)
    Elseif WIN105 = 3 
       aGRUPOS(3)
    Elseif WIN105 = 4 
       aGRUPOS(4)
    Elseif WIN105 = 5
       Return .T.
    Endif

Enddo

Return .T.


****************
Function iGRUPOS
****************

While .T.

    Setcolor(COR_TEXT)
    @ 05,00 Clear To 24,79
    @ 23,00 To 23,79 Double

    SOMBRA(12,25,16,69,8)

    Setcolor("N/W")
    @ 11,23 Clear To 15,68
    @ 11,23 To 15,68

    @ 12,25 Say "C"+Chr(162)+"digo....:"     
    @ 14,25 Say "Grupo.....:"
                 
    Setcolor(COR_CONS)
    @ 24,00 Say "INCLUS"+Chr(142)+"O "
    @ 24,72 Say " ESC-FIM"

    Setcolor("W/N")
    @ 12,37 Say Space(03) 
    @ 14,37 Say Space(30)

    WCODIGO = Space(03)
    WGRUPO  = Space(30)

    While .T.

        Setcolor("N/W,W+/N,,,N/W")

        @ 12,37 Get WCODIGO Picture "!!!" Valid find_GROUP()
        @ 14,37 Get WGRUPO
        READ_F()

        If Empty(WCODIGO) .Or. Empty(WGRUPO) .Or. Lastkey() = 27
           Return .T.
        Endif

        Setcolor(COR_TEXT)

        If CONFIRMA(24,"Dados Corretos...(S/N)?","S") = "S"
    
           Select GRUPOS

           Set Deleted Off
           Go Top
           If Deleted()
              If ! REGLOCK()
                 Loop
              Endif
              Recall
           Else
              If ! iSEQUENCIAL()
                 Loop
              Endif
           Endif
           Set Deleted On

           Replace CODIGO With WCODIGO,;
                   GRUPO  With WGRUPO

           GRAVA()

           Exit

        Endif

    Enddo

Enddo

Return .T.


*******************
Function find_GROUP
*******************

Select GRUPOS
ordsetfocus('SINC001')
Seek WCODIGO

If Found()
   ERROR("  C"+Chr(162)+"digo de grupo j"+Chr(160)+" cadastrado!  ",0)
   Return .F.
Endif

Return .T.


****************
Function aGRUPOS
****************
Parameters ESC

Setcolor(COR_TEXT)
@ 05,00 Clear To 24,79
@ 23,01 To 23,79 Double

SOMBRA(07,21,21,63,8)

Setcolor("N/W")
@ 06,20 Clear To 20,62
@ 06,20 To 20,62

@ 07,22 Say "C"+Chr(162)+"digo"
@ 07,31 Say "Grupo" 

@ 08,21 To 08,61
@ 07,29 To 19,29

@ 08,20 Say Chr(195)
@ 08,62 Say Chr(180)
@ 06,29 Say Chr(194)
@ 20,29 Say Chr(193)
@ 08,29 Say Chr(197)

Setcolor(COR_TEXT)

Select GRUPOS
ordsetfocus('SINC001')
Go Top

If ESC = 2

   CENTRO(24,"PgDw-Pr"+Chr(162)+"ximo    PgUp-Anterior    ENTER-Alterar")  

   Setcolor(COR_CONS)
   @ 24,00 Say "ALTERA"+Chr(128)+Chr(142)+"O "
   @ 24,72 Say " ESC-FIM"

Elseif ESC = 3

   CENTRO(24,"PgDw-Pr"+Chr(162)+"ximo    PgUp-Anterior")  

   Setcolor(COR_CONS)
   @ 24,00 Say "CONSULTA "
   @ 24,72 Say " ESC-FIM"

Elseif ESC = 4

   CENTRO(24,"PgDw-Pr"+Chr(162)+"ximo    PgUp-Anterior    ENTER-Excluir")  

   Setcolor(COR_CONS)
   @ 24,00 Say "EXCLUS"+Chr(142)+"O "
   @ 24,72 Say " ESC-FIM"

Endif

Declare FLD[1]

FLD[1] = "Space(02)+CODIGO+Space(03)+Chr(179)+Space(01)+GRUPO+Space(01)"

Setcolor("N/W,W+/N,,,N/W")

DBEDIT(09,21,19,61,FLD,"GUIA2","","","","")

Return .T.


**************
Function GUIA2
**************
Parameters MODO, OPC

LIN = ROW()

Setcolor("N/W")

@ 08,21 To 08,61
@ 07,29 To 19,29

@ 08,20 Say Chr(195)
@ 08,62 Say Chr(180)
@ 06,29 Say Chr(194)
@ 20,29 Say Chr(193)
@ 08,29 Say Chr(197)

If MODO < 4 
   Return 1
Elseif Lastkey() = 13 .And. ESC = 2 
 
   If ! REGLOCK()
      Return 2
   Endif

   Setcolor(COR_TEXT)
   @ 24,12 Clear To 24,70

   While .T.

       Setcolor("N/W,W+/N,,,N/W")

       @ LIN,23 Say CODIGO
       @ LIN,31 Get GRUPO
       READ_F()

       Setcolor(COR_TEXT)         

       If CONFIRMA(24,"Dados corretos...(S/N)?","S") = "S"

          @ 24,00 Clear To 24,79
          CENTRO(24,"PgDw-Pr"+Chr(162)+"ximo    PgUp-Anterior    ENTER-Alterar ")  

          Setcolor(COR_CONS)
          @ 24,00 Say "ALTERA"+Chr(128)+Chr(142)+"O "
          @ 24,72 Say " ESC-FIM"

          Return 2

       Endif 
   
   Enddo

Elseif Lastkey() = 13 .And. ESC = 4

   If ! REGLOCK()
      Return 2
   Endif

   Setcolor("N+*/W")  

   @ LIN,23 Say CODIGO     
   @ LIN,31 Say GRUPO           

   Setcolor(COR_TEXT)
   @ 24,12 Clear To 24,70
         
   If CONFIRMA(24,"Voc"+Chr(136)+" deseja excluir este grupo...(S/N)?","N") = "S"

      @ 24,00 Clear To 24,79

      Select GRUPOS

      Replace CODIGO With Space(03),;
              GRUPO  With Space(30)

      Delete

      GRAVA()

      CENTRO(24,"PgDw-Pr"+Chr(162)+"ximo    PgUp-Anterior    ENTER-Excluir")  

      Setcolor(COR_CONS)
      @ 24,00 Say "EXCLUS"+Chr(142)+"O "
      @ 24,72 Say " ESC-FIM"

      Skip
 
      Return 2
 
   Else
      CENTRO(24,"PgDw-Pr"+Chr(162)+"ximo    PgUp-Anterior    ENTER-Excluir")  
      Return 1
   Endif

Elseif Lastkey() = 27
   Return 0
Else
   Return 1
Endif


*****************
Function PRODUTOS
*****************
WIN106 = 1

While .T.

    Setcolor(COR_CAB)   
    @ 03,42 Say "PRODUTOS DE ESTOQUE                " 
    Setcolor(COR_TEXT)

    Private Opcao[5]

    Opcao[1] = " Inclus"+Chr(132)+"o   "  
    Opcao[2] = " Altera"+Chr(135)+Chr(132)+"o  "
    Opcao[3] = " Consulta   "
    Opcao[4] = " Exclus"+Chr(132)+"o   "
    Opcao[5] = " Fim        "

    private ACESSO[5]
    ACESSO[1] = .T.
    ACESSO[2] = .T.
    ACESSO[3] = .T.
    ACESSO[4] = .T.
    ACESSO[5] = .T.

    Private Letra[5]

    Letra[1] = "I"  
    Letra[2] = "A"
    Letra[3] = "C"
    Letra[4] = "E"
    Letra[5] = "F"

    Private Pos[5]

    Pos[1] = 1 
    Pos[2] = 1 
    Pos[3] = 1
    Pos[4] = 1
    Pos[5] = 1

    Private Obs[5]

    Obs[1] = "Inclus"+Chr(132)+"o de produtos!"
    Obs[2] = "Altera"+Chr(135)+Chr(132)+"o de produtos!"
    Obs[3] = "Consulta aos produtos!"
    Obs[4] = "Exclus"+Chr(132)+"o de produtos!"
    Obs[5] = "Retorno ao menu principal!"

    WIN106 = ACHOISE(WIN106,10,33,16,46,2,OPCAO,ACESSO,LETRA,POS,OBS,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)

    If WIN106 = 0 
       Return .T.
    Elseif WIN106 = 1  
       i_PRODUTOS()    
    Elseif WIN106 = 2  
       a_PRODUTOS()    
    Elseif WIN106 = 3  
       c_PRODUTOS()    
    Elseif WIN106 = 4  
       e_PRODUTOS()
    Elseif WIN106 = 5
       Return .T.
    Endif

Enddo

Return .T.


*********************
Function screenPRODUT
*********************

Setcolor(COR_TEXT)
@ 00,00 Clear To 24,79

Setcolor(COR_CONS)
@ 00,00 Clear To 00,79
@ 24,00 Clear To 24,79

@ 00,30 Say "CADASTRO DE PRODUTOS" 

Setcolor(COR_TEXT)

@ 02,01 Say "C"+Chr(162)+"digo.......:"

@ 04,01 Say "Produto......:"
@ 05,01 Say "Refer"+Chr(136)+"ncia...:"
@ 06,01 Say "Unidade......:"

@ 08,01 Say "Grupo........:     -"

@ 10,01 Say "Sd.Anterior..:"
@ 11,01 Say "Aquisi"+Chr(135)+Chr(132)+"o....:"
@ 12,01 Say "Baixas.......:"
@ 13,01 Say "Sd.Atual.....:"

@ 15,01 Say "M"+Chr(161)+"nimo.......:"
@ 16,01 Say "Ressuprimento:"

@ 18,01 Say "Custo........:"

Return .T.


*******************
Function i_PRODUTOS
*******************
Save Screen

While .T.

    screenPRODUT()      

    WCODIGO  = Space(08)
    WPRODUTO = Space(40)  
    WREF     = Space(20)
    WUN      = Space(03)
    WCOD_GRP = Space(03)
    WSDANT   = 0 
    WAQUIS   = 0
    WBAIXAS  = 0 
    WMINIMO  = 0
    WRESSUP  = 0
    WCUSTO   = 0

    Setcolor(COR_CONS)
    @ 24,00 Say "INCLUS"+Chr(142)+"O" 
    @ 24,73 Say "ESC-FIM"

    Select PRODUTOS
    ordsetfocus('SINC004')
    Go Bottom

    WCODIGO = Left(Alltrim(Str(Val(CODIGO)+1,8))+Space(08),8)

    While .T.

        Setcolor(COR_TEXT+","+COR_GETS+",,,"+COR_TEXT)

        @ 02,16 Say WCODIGO       Picture "99999999"

        @ 04,16 Get WPRODUTO 
        @ 05,16 Get WREF                     
        @ 06,16 Get WUN        

        @ 08,16 Get WCOD_GRP Picture "999"          Valid view_GRP()

        @ 10,16 Get WSDANT   Picture "99999"        Valid sd_ATUAL()
        @ 11,16 Get WAQUIS   Picture "99999"        Valid sd_ATUAL()
        @ 12,16 Get WBAIXAS  Picture "99999"        Valid sd_ATUAL()

        @ 15,16 Get WMINIMO  Picture "99999"
        @ 16,16 Get WRESSUP  Picture "99999"

        @ 18,16 Get WCUSTO   Picture "@R@E 999,999.9999"
        READ_F()

        If Empty(WPRODUTO) .Or. Lastkey() = 27
           Restore Screen
           Return .T.
        Endif

        Setcolor(COR_CONS)
        If CONFIRMA(24,"Dados corretos...(S/N)?","S") = "S"
                       
           Select PRODUTOS
           ordsetfocus('SINC004')
           Go Bottom

           WCODIGO = Left(Alltrim(Str(Val(CODIGO)+1,8))+Space(08),8)

           Set Deleted Off
           Go Top
           If Deleted()
              If ! REGLOCK()
                 Loop
              Endif
              Recall
           Else
              If ! iSEQUENCIAL()
                 Loop
              Endif
           Endif
           Set Deleted On

           Replace CODIGO With WCODIGO

           save_PRODUTOS()

           GRAVA()

           Exit
    
        Endif

    Enddo

Enddo

Return .T.


*****************
Function view_GRP
*****************

If Empty(WCOD_GRP)
   help_GRP() 
Endif

Select GRUPOS
ordsetfocus('SINC001')
Seek WCOD_GRP

If ! Found()
   ERROR("  C"+Chr(162)+"digo de grupo n"+Chr(132)+"o cadastrado!  ",0)
   Return .F.
Endif

Setcolor(COR_TEXT)

@ 08,22 Say Space(32)
@ 08,22 Say "<"+Alltrim(GRUPO)+">"

Return .T.


*****************
Function sd_ATUAL
*****************

@ 13,16 Say ((WSDANT + WAQUIS) - WBAIXAS) Picture "99999"

Return .T.


*******************
Function a_PRODUTOS
*******************
Save Screen

While .T.

    screenPRODUT()     

    WCODIGO = Space(08)

    Setcolor(COR_CONS)
    @ 24,00 Say "ALTERA"+Chr(128)+Chr(142)+"O" 
    @ 24,73 Say "ESC-FIM"

    Setcolor(COR_TEXT+","+COR_GETS+",,,"+COR_TEXT)

    @ 02,16 Get WCODIGO Picture "99999999"
    READ_F()

    If Empty(WCODIGO) .Or. Lastkey() = 27
       Restore Screen
       Return .T.
    Endif

    Select PRODUTOS
    ordsetfocus('SINC001')
    Seek WCODIGO

    If ! Found()
       ERROR("  C"+Chr(162)+"digo de produto n"+Chr(132)+"o cadastrado!  ",0)
       Loop
    Endif


    WPRODUTO = PRODUTO 
    WREF     = REF            
    WUN      = UN      
    WCOD_GRP = COD_GRP 
    WSDANT   = SDANT   
    WAQUIS   = AQUIS   
    WBAIXAS  = BAIXAS  
    WMINIMO  = MINIMO  
    WRESSUP  = RESSUP  
    WCUSTO   = CUSTO   

    Setcolor(COR_TEXT)
    @ 13,16 Say ((WSDANT + WAQUIS) - WBAIXAS) Picture "99999"

    Select GRUPOS   
    ordsetfocus('SINC001')
    Seek WCOD_GRP

    @ 08,22 Say Space(32)
    @ 08,22 Say "<"+Alltrim(GRUPO)+">" 

    While .T.

        Setcolor(COR_TEXT+","+COR_GETS+",,,"+COR_TEXT)

        @ 04,16 Get WPRODUTO 
        @ 05,16 Get WREF                     
        @ 06,16 Get WUN     

        @ 08,16 Get WCOD_GRP      Picture "999"          Valid view_GRP()

        @ 10,16 Get WSDANT        Picture "99999"        Valid sd_ATUAL()
        @ 11,16 Get WAQUIS        Picture "99999"        Valid sd_ATUAL()
        @ 12,16 Get WBAIXAS       Picture "99999"        Valid sd_ATUAL()

        @ 15,16 Get WMINIMO       Picture "99999"
        @ 16,16 Get WRESSUP       Picture "99999"

        @ 18,16 Get WCUSTO        Picture "@R@E 999,999.9999"
        READ_F()                                                         

        If Empty(WPRODUTO) .Or. Lastkey() = 27
           Restore Screen
           Return .T.
        Endif

        Setcolor(COR_CONS)
        If CONFIRMA(24,"Dados Corretos...(S/N)?","S") = "S"
                       
           Select PRODUTOS

           If ! REGLOCK()
              Loop
           Endif

           save_PRODUTOS()

           GRAVA()

           Exit
    
        Endif

    Enddo                                                                             

Enddo

Return .T.


*******************
Function c_PRODUTOS
*******************
Save Screen

While .T.

    OPC   = 3
    TRAVA = .F.

    screenPRODUT()     

    Setcolor(COR_CONS)
    @ 24,00 Say "CONSULTA" 
    @ 24,73 Say "ESC-FIM"

    SOMBRA(10,14,16,70,8)

    @ 09,12 Clear To 15,69
    @ 09,12 To 15,69

    @ 10,14 Say "Consulta a partir do c"+Chr(162)+"digo do produto.............[1]"
    @ 11,14 Say "Consulta a partir da descri"+Chr(135)+Chr(132)+"o do produto..........[2]" 
    @ 12,14 Say "Retorno ao menu principal..........................[3]" 

    Setcolor("W/N,W/N,,,W/N")
    @ 14,14 Say "Escolha sua op"+Chr(135)+Chr(132)+"o..................................[ ]"
    @ 14,66 Get OPC Picture "9" Range 1,3
    READ_F()   

    Setcolor(COR_TEXT)

    If OPC = 3 .Or. Lastkey() = 27
       Restore Screen
       Return .T.
    Endif

    screenPRODUT()     

    Setcolor(COR_CONS)
    @ 24,00 Say "CONSULTA " 
    @ 24,73 Say "ESC-FIM"

    Setcolor(COR_TEXT+","+COR_GETS+",,,"+COR_TEXT)

    If OPC = 1

       While .T.

           WCODIGO = Space(08)

           @ 02,16 Get WCODIGO Picture "99999999"
           READ_F()

           If ! Empty(WCODIGO) .Or. Lastkey() <> 27

              Select PRODUTOS
              ordsetfocus('SINC004')
              Seek Val(WCODIGO)

              If ! Found()
                 ERROR("  C"+Chr(162)+"digo de produto n"+Chr(132)+"o cadastrado!  ",0)
                 Loop
              Endif

           Else
              TRAVA = .T.
           Endif

       Exit
       Enddo

    Elseif OPC = 2

       WPRODUTO = Space(40)

       @ 04,16 Get WPRODUTO 
       READ_F()

       If ! Empty(WPRODUTO) .Or. Lastkey() <> 27
          
          Select PRODUTOS
          ordsetfocus('SINC002')
          Go Top
          Set Softseek On
          Seek Upper(WPRODUTO)
          Set Softseek Off

          If Eof()
             Skip -1
          Endif

       Else
          TRAVA = .T.
       Endif

    Endif

    If TRAVA
       Loop
    Endif

    screenPRODUT()     

    While .T.

        Setcolor(COR_CONS)
        @ 24,00 Say "CONSULTA " 
        @ 24,73 Say "ESC-FIM"

        CENTRO(24,"PgDw-Pr"+Chr(162)+"ximo    PgUp-Anterior    ENTER-Alterar")

        Setcolor(COR_TEXT)

        @ 02,16 Say CODIGO 
        @ 04,16 Say PRODUTO 
        @ 05,16 Say REF                    
        @ 06,16 Say UN        

        @ 08,16 Say COD_GRP                    Picture "999"

        @ 10,16 Say SDANT                      Picture "99999"   
        @ 11,16 Say AQUIS                      Picture "99999"   
        @ 12,16 Say BAIXAS                     Picture "99999"   
        @ 13,16 Say ((SDANT + AQUIS) - BAIXAS) Picture "99999"

        @ 15,16 Say MINIMO                     Picture "99999"
        @ 16,16 Say RESSUP                     Picture "99999"

        @ 18,16 Say CUSTO                      Picture "@R@E 999,999.9999"

        WCOD_GRP = COD_GRP 

        Select GRUPOS   
        ordsetfocus('SINC001')
        Seek WCOD_GRP

        @ 08,22 Say Space(32)
        @ 08,22 Say "<"+Alltrim(GRUPO)+">" 

        Select PRODUTOS

        Inkey(0)

        If Lastkey() = 03
           Skip
 
           If Eof()
              Skip -1
           Endif

        Elseif Lastkey() = 18
           Skip -1

        Elseif Lastkey() = 27
           Exit

        Elseif Lastkey() = 13

           If ! REGLOCK()
              Loop
           Endif

           Setcolor(COR_CONS)                                                                
           @ 24,00 Clear To 24,79

           @ 24,00 Say "ALTERA"+Chr(128)+Chr(142)+"O"
           @ 24,73 Say "ESC-FIM"                                                            
                                                                                             
           WPRODUTO = PRODUTO 
           WREF     = REF            
           WUN      = UN      
           WCOD_GRP = COD_GRP 
           WSDANT   = SDANT   
           WAQUIS   = AQUIS   
           WBAIXAS  = BAIXAS  
           WMINIMO  = MINIMO  
           WRESSUP  = RESSUP  
           WCUSTO   = CUSTO   

           While .T.                                                                         
                                                                                             
               Setcolor(COR_TEXT+","+COR_GETS+",,,"+COR_TEXT)                   
                                                                                
               @ 04,16 Get WPRODUTO 
               @ 05,16 Get WREF                     
               @ 06,16 Get WUN        

               @ 08,16 Get WCOD_GRP      Picture "999"          Valid view_GRP()

               @ 10,16 Get WSDANT        Picture "99999"        Valid sd_ATUAL()
               @ 11,16 Get WAQUIS        Picture "99999"        Valid sd_ATUAL()
               @ 12,16 Get WBAIXAS       Picture "99999"        Valid sd_ATUAL()

               @ 15,16 Get WMINIMO       Picture "99999"
               @ 16,16 Get WRESSUP       Picture "99999"
  
               @ 18,16 Get WCUSTO        Picture "@R@E 999,999.9999"
               READ_F()                                                         
                                                                                             
               Setcolor(COR_CONS)
               If CONFIRMA(24,"Dados corretos...(S/N)?","S") = "S"                          
                                                                                             
                  Select PRODUTOS                                                            
                                                                                             
                  save_PRODUTOS()
                                                                                             
                  GRAVA()

                  Exit                                                                       
                                                                                             
               Endif                                                                         
                                                                                             
           Enddo                                                                             

        Endif

    Enddo

Enddo

Return .T.       
             
            
*******************
Function e_PRODUTOS
*******************
Save Screen

While .T.

    screenPRODUT()     

    WCODIGO = Space(08)

    Setcolor(COR_CONS)
    @ 24,00 Say "EXCLUS"+Chr(142)+"O" 
    @ 24,73 Say "ESC-FIM"

    Setcolor(COR_TEXT+","+COR_GETS+",,,"+COR_TEXT)

    @ 02,16 Get WCODIGO Picture "99999999"
    READ_F()

    If Empty(WCODIGO) .Or. Lastkey() = 27
       Restore Screen
       Return .T.
    Endif

    Select PRODUTOS
    ordsetfocus('SINC001')
    Seek WCODIGO

    If ! Found()
       ERROR("  C"+Chr(162)+"digo de produto n"+Chr(132)+"o cadastrado!  ",0)
       Loop
    Endif

    Setcolor(COR_TEXT)

    @ 04,16 Say PRODUTO 
    @ 05,16 Say REF
    @ 06,16 Say UN        

    @ 08,16 Say COD_GRP                    Picture "999"

    @ 10,16 Say SDANT                      Picture "99999"   
    @ 11,16 Say AQUIS                      Picture "99999"   
    @ 12,16 Say BAIXAS                     Picture "99999"   
    @ 13,16 Say ((SDANT + AQUIS) - BAIXAS) Picture "99999"

    @ 15,16 Say MINIMO                     Picture "99999"
    @ 16,16 Say RESSUP                     Picture "99999"

    @ 18,16 Say CUSTO                      Picture "@R@E 999,999.9999"

    WCOD_GRP = COD_GRP

    Select GRUPOS   
    ordsetfocus('SINC001')
    Seek WCOD_GRP

    @ 08,22 Say Space(32)
    @ 08,22 Say "<"+Alltrim(GRUPO)+">" 

    Setcolor(COR_CONS)
    If CONFIRMA(24,"Voc"+Chr(136)+" deseja excluir este produto...(S/N)?","N") = "S"

       Select PRODUTOS

       If ! REGLOCK()
          Loop
       Endif
                       
       Replace CODIGO  With Space(08),;   
               PRODUTO With Space(40),;   
               REF     With Space(20),;   
               UN      With Space(02),;   
               COD_GRP With Space(03),;   
               SDANT   With 0        ,;   
               AQUIS   With 0        ,;   
               BAIXAS  With 0        ,;  
               MINIMO  With 0        ,;   
               RESSUP  With 0        ,;   
               CUSTO   With 0        

       Delete

       GRAVA()
  
    Endif

Enddo

Return .T.

	
**********************
Function save_PRODUTOS
**********************

Replace PRODUTO With WPRODUTO ,;
        REF     With WREF     ,;
        UN      With WUN      ,;
        COD_GRP With WCOD_GRP ,;
        SDANT   With WSDANT   ,;
        AQUIS   With WAQUIS   ,;
        BAIXAS  With WBAIXAS  ,;
        MINIMO  With WMINIMO  ,;
        RESSUP  With WRESSUP  ,;
        CUSTO   With WCUSTO

Return .T.


*****************
Function ENTRADAS
*****************
WIN450 = 1

While .T.

    Setcolor(COR_CAB)   
    @ 03,42 Say "ENTRADA DE ESTOQUE (COMPRAS)       " 
    Setcolor(COR_TEXT)

    Private Opcao[5]

    Opcao[1] = " Inclus"+Chr(132)+"o  " 
    Opcao[2] = " Altera"+Chr(135)+Chr(132)+"o " 
    Opcao[3] = " Consulta  " 
    Opcao[4] = " Exclus"+Chr(132)+"o  " 
    Opcao[5] = " Fim       " 

    private ACESSO[5]
    ACESSO[1] = .T.
    ACESSO[2] = .T.
    ACESSO[3] = .T.
    ACESSO[4] = .T.
    ACESSO[5] = .T.

    Private Letra[5]

    Letra[1] = "I"  
    Letra[2] = "A"
    Letra[3] = "C"
    Letra[4] = "E"
    Letra[5] = "F"

    Private Pos[5]

    Pos[1] = 1
    Pos[2] = 1
    Pos[3] = 1
    Pos[4] = 1
    Pos[5] = 1

    Private Obs[5]

    Obs[1] = "Inclus"+Chr(132)+"o de entradas de produtos!"     
    Obs[2] = "Altera"+Chr(135)+Chr(132)+"o de entradas de produtos!"
    Obs[3] = "Consulta as entradas de produtos!"
    Obs[4] = "Exclus"+Chr(132)+"o de entradas de produtos!"
    Obs[5] = "Retorno ao menu principal!"                   

    WIN450 = ACHOISE(WIN450,10,33,16,45,2,OPCAO,ACESSO,LETRA,POS,OBS,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)
   
    If WIN450 = 0
       Return .T.
    Elseif WIN450 = 1  
       i_OUTRAS_E()    
    Elseif WIN450 = 2  
       a_OUTRAS_E()    
    Elseif WIN450 = 3  
       c_OUTRAS_E()    
    Elseif WIN450 = 4  
       e_OUTRAS_E()
    Elseif WIN450 = 5
       Return .T.
    Endif

Enddo

Return .T.


******************
Function screen_OE
******************

Setcolor(COR_TEXT)
@ 00,00 Clear To 24,79

Setcolor(COR_CONS)
@ 00,00 Clear To 00,79
@ 24,00 Clear To 24,79

@ 00,26 Say "ENTRADAS DE PRODUTOS NO ESTOQUE"

Setcolor(COR_TEXT)
@ 01,00 Clear To 23,79

@ 02,01 Say "Sequ"+Chr(136)+"ncia..:                   Documento:                  Entrada:"
@ 04,01 Say "Fornecedor.:                                                       "

SOMBRA(07,03,23,79,8)

Setcolor(COR_CONS)
@ 06,01 Clear To 22,78
@ 06,01 Clear To 10,78

@ 06,01 Say "CODIGO   PRODUTO                          UN     QTDE     UNITARIO       TOTAL" 

@ 06,08 Say Chr(179)
@ 06,42 Say Chr(179)
@ 06,45 Say Chr(179)
@ 06,54 Say Chr(179)
@ 06,67 Say Chr(179)

@ 07,08 To 22,08
@ 07,42 To 22,42
@ 07,45 To 22,45
@ 07,54 To 22,54
@ 07,67 To 22,67

@ 07,01 To 07,78 Double

@ 07,08 Say Chr(216)
@ 07,42 Say Chr(216)
@ 07,45 Say Chr(216)
@ 07,54 Say Chr(216)
@ 07,67 Say Chr(216)

Setcolor(COR_TEXT)

Return .T.


*******************
Function i_OUTRAS_E
*******************
Save Screen

While .T.

    screen_OE()

    Setcolor(COR_CONS)
    @ 24,00 Say "INCLUS"+Chr(142)+"O "
    @ 24,72 Say " ESC-FIM"

    WSEQ      = 0 
    WDOCNO    = Space(10)
    WENTRADA  = DATE()
    WHISTORIC = Space(54)
    WCCUSTO   = Space(01)

    Declare vCODIGO[14] 
    Declare vQTDE[14] 
    Declare vUNITARIO[14] 

    Afill(vCODIGO,Space(08))
    Afill(vQTDE,0)
    Afill(vUNITARIO,0)

    SET CENTURY ON

    Setcolor(COR_TEXT)
    @ 02,14 Say "??????"

    While .T.

        Setcolor(COR_TEXT+","+COR_GETS+",,,"+COR_TEXT)

        @ 02,43 Get WDOCNO
        @ 02,69 Get WENTRADA
        @ 04,14 Get WHISTORIC     Picture "@S44"
      * @ 04,69 Get WCCUSTO       Picture "!"                                           Valid win_CCUSTO()

        Setcolor(COR_CONS+",W+/R,,,"+COR_CONS)

        @ 08,01 Get vCODIGO[1]    Picture "@S7"                                                                        Valid seek_P(1)
        @ 08,46 Get vQTDE[1]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[1]) .And. vCODIGO[1] <> "99999999" 
        @ 08,55 Get vUNITARIO[1]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[1]) .And. vCODIGO[1] <> "99999999" Valid total_ENTRADA(1)

        @ 09,01 Get vCODIGO[2]    Picture "@S7"                 When ! Empty(vCODIGO[1]) .And. vCODIGO[1] <> "99999999" Valid seek_P(2)
        @ 09,46 Get vQTDE[2]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[2]) .And. vCODIGO[2] <> "99999999"
        @ 09,55 Get vUNITARIO[2]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[2]) .And. vCODIGO[2] <> "99999999" Valid total_ENTRADA(2)

        @ 10,01 Get vCODIGO[3]    Picture "@S7"                 When ! Empty(vCODIGO[2]) .And. vCODIGO[2] <> "99999999" Valid seek_P(3)
        @ 10,46 Get vQTDE[3]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[3]) .And. vCODIGO[3] <> "99999999" 
        @ 10,55 Get vUNITARIO[3]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[3]) .And. vCODIGO[3] <> "99999999" Valid total_ENTRADA(3) 

        @ 11,01 Get vCODIGO[4]    Picture "@S7"                 When ! Empty(vCODIGO[3]) .And. vCODIGO[3] <> "99999999" Valid seek_P(4)
        @ 11,46 Get vQTDE[4]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[4]) .And. vCODIGO[4] <> "99999999" 
        @ 11,55 Get vUNITARIO[4]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[4]) .And. vCODIGO[4] <> "99999999" Valid total_ENTRADA(4)

        @ 12,01 Get vCODIGO[5]    Picture "@S7"                 When ! Empty(vCODIGO[4]) .And. vCODIGO[4] <> "99999999" Valid seek_P(5)
        @ 12,46 Get vQTDE[5]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[5]) .And. vCODIGO[5] <> "99999999" 
        @ 12,55 Get vUNITARIO[5]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[5]) .And. vCODIGO[5] <> "99999999" Valid total_ENTRADA(5)

        @ 13,01 Get vCODIGO[6]    Picture "@S7"                 When ! Empty(vCODIGO[5]) .And. vCODIGO[5] <> "99999999" Valid seek_P(6)
        @ 13,46 Get vQTDE[6]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[6]) .And. vCODIGO[6] <> "99999999" 
        @ 13,55 Get vUNITARIO[6]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[6]) .And. vCODIGO[6] <> "99999999" Valid total_ENTRADA(6)

        @ 14,01 Get vCODIGO[7]    Picture "@S7"                 When ! Empty(vCODIGO[6]) .And. vCODIGO[6] <> "99999999" Valid seek_P(7)
        @ 14,46 Get vQTDE[7]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[7]) .And. vCODIGO[7] <> "99999999" 
        @ 14,55 Get vUNITARIO[7]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[7]) .And. vCODIGO[7] <> "99999999" Valid total_ENTRADA(7)

        @ 15,01 Get vCODIGO[8]    Picture "@S7"                 When ! Empty(vCODIGO[7]) .And. vCODIGO[7] <> "99999999" Valid seek_P(8)
        @ 15,46 Get vQTDE[8]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[8]) .And. vCODIGO[8] <> "99999999" 
        @ 15,55 Get vUNITARIO[8]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[8]) .And. vCODIGO[8] <> "99999999" Valid total_ENTRADA(8)

        @ 16,01 Get vCODIGO[9]    Picture "@S7"                 When ! Empty(vCODIGO[8]) .And. vCODIGO[8] <> "99999999" Valid seek_P(9)
        @ 16,46 Get vQTDE[9]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[9]) .And. vCODIGO[9] <> "99999999" 
        @ 16,55 Get vUNITARIO[9]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[9]) .And. vCODIGO[9] <> "99999999" Valid total_ENTRADA(9)

        @ 17,01 Get vCODIGO[10]   Picture "@S7"                 When ! Empty(vCODIGO[9])  .And. vCODIGO[9]  <> "99999999"  Valid seek_P(10)
        @ 17,46 Get vQTDE[10]     Picture "@Z 99999.99"         When ! Empty(vCODIGO[10]) .And. vCODIGO[10] <> "99999999"  
        @ 17,55 Get vUNITARIO[10] Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[10]) .And. vCODIGO[10] <> "99999999"  Valid total_ENTRADA(10)

        @ 18,01 Get vCODIGO[11]   Picture "@S7"                 When ! Empty(vCODIGO[10]) .And. vCODIGO[10] <> "99999999" Valid seek_P(11)
        @ 18,46 Get vQTDE[11]     Picture "@Z 99999.99"         When ! Empty(vCODIGO[11]) .And. vCODIGO[11] <> "99999999" 
        @ 18,55 Get vUNITARIO[11] Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[11]) .And. vCODIGO[11] <> "99999999" Valid total_ENTRADA(11)

        @ 19,01 Get vCODIGO[12]   Picture "@S7"                 When ! Empty(vCODIGO[11]) .And. vCODIGO[11] <> "99999999" Valid seek_P(12)
        @ 19,46 Get vQTDE[12]     Picture "@Z 99999.99"         When ! Empty(vCODIGO[12]) .And. vCODIGO[12] <> "99999999" 
        @ 19,55 Get vUNITARIO[12] Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[12]) .And. vCODIGO[13] <> "99999999" Valid total_ENTRADA(12)
   
        @ 20,01 Get vCODIGO[13]   Picture "@S7"                 When ! Empty(vCODIGO[12]) .And. vCODIGO[12] <> "99999999" Valid seek_P(13)
        @ 20,46 Get vQTDE[13]     Picture "@Z 99999.99"         When ! Empty(vCODIGO[13]) .And. vCODIGO[13] <> "99999999" 
        @ 20,55 Get vUNITARIO[13] Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[13]) .And. vCODIGO[13] <> "99999999" Valid total_ENTRADA(13)

        @ 21,01 Get vCODIGO[14]   Picture "@S7"                 When ! Empty(vCODIGO[13]) .And. vCODIGO[13] <> "99999999" Valid seek_P(14)
        @ 21,46 Get vQTDE[14]     Picture "@Z 99999.99"         When ! Empty(vCODIGO[14]) .And. vCODIGO[14] <> "99999999" 
        @ 21,55 Get vUNITARIO[14] Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[14]) .And. vCODIGO[14] <> "99999999" Valid total_ENTRADA(14)
        READ_F()

        If Lastkey() = 27
           SET CENTURY OFF
           Restore Screen
           Return .T.
        Endif
                                                                                                                            
        Setcolor(COR_CONS)

        If CONFIRMA(24,"Dados corretos...(S/N)?","S") = "S"                                                               

           While .T.

               Select OUTRAS_E
               ordsetfocus('SINC001')
               Go Bottom
   
               WSEQ = SEQ + 1

               Seek WSEQ
   
               If Found()            
                  Loop
               Endif 
                
           Exit
           Enddo

           Set Deleted Off
           Go Top
           If Deleted()
              If ! REGLOCK()
                 Loop
              Endif
              Recall
           Else
              If ! iSEQUENCIAL()
                 Loop
              Endif
           Endif
           Set Deleted On

           Replace SEQ With WSEQ  

           save_OUT_E()

           For I=1 To 14

               If ! Empty(vCODIGO[I]) .And. vCODIGO[I] <> "99999999" .And. vQTDE[I] > 0

                  While .T.

                      Select ITENS_E

                      Set Deleted Off
                      Go Top
                      If Deleted()
                         If ! REGLOCK()
                            Loop
                         Endif
                         Recall
                      Else
                         If ! iSEQUENCIAL()
                            Loop
                         Endif
                      Endif
                      Set Deleted On

                  Exit
                  Enddo

                  Replace SEQ      With WSEQ      ,;
                          CODIGO   With vCODIGO[I],;
                          QTDE     With vQTDE[I]  ,;
                          UNITARIO With vUNITARIO[I]

                  // entrada de produtos no estoque 

                  Select PRODUTOS
                  ordsetfocus('SINC001')
                  Seek vCODIGO[I]

                  While .T.

                      If ! REGLOCK()
                         Loop
                      Endif

                  Exit
                  Enddo

                  Replace AQUIS With AQUIS + vQTDE[I],;
                          CUSTO With vUNITARIO[I]    

                  GRAVA()

               Endif

           Next I

           Tone(1001,0.7)
           Tone(1002,0.4)

           Setcolor(COR_TEXT+"*")
           @ 02,14 Say Strzero(WSEQ,6)
           Setcolor(COR_TEXT)

           Inkey(3)

           Exit

        Endif

    Enddo

Enddo

Restscreen(00,00,24,79,SAVE)

Return .T.


*******************
Function win_CCUSTO
*******************

If Lastkey() = 27 
   Return .T.
Endif

If WCCUSTO = "E"
   Setcolor(COR_TEXT)
   @ 04,69 Say "Eq"+Chr(129)+"ino"
Elseif WCCUSTO = "B"
   Setcolor(COR_TEXT)
   @ 04,69 Say "Bovino"
Else
   Setcolor(COR_TEXT)
   @ 04,69 Say "      "
   ERROR("Centro de Custo: B-Bovino ou E-Equino",0)
   Return .F.
Endif

Return .T.


******************
Function seek_P(I)
******************
Local LIN := ROW()

If Lastkey() = 27
   Return .T.
Endif

If vCODIGO[I] = Replicate("9",8)
   Setcolor(COR_CONS)
   @ LIN,10 Say Space(32)           
   @ LIN,43 Say Space(02)
   @ LIN,46 Say Space(08)
   @ LIN,55 Say Space(12)
   @ LIN,68 Say Space(11)
   Return .T.
Endif

If Empty(vCODIGO[I])
   help_PRODUTOS()
Endif

Select PRODUTOS
ordsetfocus('SINC001')
Seek vCODIGO[I]

If ! Found()
   ERROR("  C"+Chr(162)+"digo de produto n"+Chr(132)+"o cadastrado!  ",0)
   Return .F.
Endif

If vUNITARIO[I] = 0
   vUNITARIO[I] = CUSTO      
Endif

Setcolor(COR_CONS)

@ LIN,10 Say Left(PRODUTO,32)
@ LIN,43 Say UN           Picture "@S2"
@ LIN,55 Say vUNITARIO[9] Picture "@R@E@Z 999,999.9999" 

Keyboard ""

Return .T.


*************************
Function total_ENTRADA(I)
*************************

Setcolor(COR_CONS)
@ ROW(),68 Say (vQTDE[I] * vUNITARIO[I]) Picture "@R@E@Z 9999,999.99"

tTOTAL = 0

For I=1 To 14
    tTOTAL = tTOTAL + (vQTDE[I] * vUNITARIO[I])
Next

Setcolor("W+/N")
@ 23,68 Say tTOTAL Picture "@R@E 9999,999.99" 
  
Setcolor(COR_CONS)

Return .T.


*******************
Function save_OUT_E
*******************

Replace DOCNO    With WDOCNO   ,;
        ENTRADA  With WENTRADA ,;
        HISTORIC With WHISTORIC,;
        CCUSTO   With WCCUSTO  

Return .T.


*******************
Function a_OUTRAS_E
*******************
Save Screen

While .T.

    screen_OE()

    Setcolor(COR_CONS)
    @ 24,00 Say "ALTERA"+Chr(128)+Chr(142)+"O "
    @ 24,72 Say " ESC-FIM"

    WSEQ = 0 

    Declare vCODIGO[14] 
    Declare vQTDE[14] 
    Declare vUNITARIO[14] 

    Afill(vCODIGO,Space(08))
    Afill(vQTDE,0)
    Afill(vUNITARIO,0)

    SET CENTURY ON

    Setcolor(COR_TEXT+","+COR_GETS+",,,"+COR_TEXT)

    @ 02,14 Get WSEQ Picture "@Z 999999"
    READ_F()
    @ 02,14 Say Strzero(WSEQ,6)

    If Empty(WSEQ) .Or. Lastkey() = 27
       SET CENTURY OFF
       Restore Screen
       Return .T.
    Endif

    Select OUTRAS_E
    ordsetfocus('SINC001')
    Seek WSEQ

    If ! Found()
       ERROR("  Sequ"+Chr(136)+"ncia de controle n"+Chr(132)+"o cadastrado!  ",0)
       Return .T.
    Endif

    WDOCNO    = DOCNO
    WENTRADA  = ENTRADA
    WHISTORIC = HISTORIC
    WCCUSTO   = CCUSTO

  * If CCUSTO = "E"
  *    @ 04,69 Say "Eq"+Chr(129)+"ino"
  * Elseif CCUSTO = "B"
  *    @ 04,69 Say "Bovino"
  * Else
  *    @ 04,69 Say "      "
  * Endif

    I = 1
    L = 8

    Select ITENS_E
    ordsetfocus('SINC001')
    Seek WSEQ

    While SEQ == WSEQ .And. ! Eof()

        vCODIGO[I]   = CODIGO
        vQTDE[I]     = QTDE
        vUNITARIO[I] = UNITARIO

        Setcolor(COR_CONS)

        @ L,01 Say vCODIGO[I]                Picture "@S7"
        @ L,46 Say vQTDE[I]                  Picture "@Z 99999.99" 
        @ L,55 Say vUNITARIO[I]              Picture "@Z 999,999.9999" 
        @ L,68 Say (vQTDE[I] * vUNITARIO[I]) Picture "@R@EZ 9999,999.99"

        Select PRODUTOS
        ordsetfocus('SINC001')
        Seek vCODIGO[I]

        @ L,10 Say Left(PRODUTO,32)
        @ L,43 Say UN Picture "@S2"

        I++
        L++

        If I > 14
           Exit
        Endif

        Select ITENS_E
        Skip

    Enddo

    While .T.

        Setcolor(COR_TEXT+","+COR_GETS+",,,"+COR_TEXT)

        @ 02,43 Get WDOCNO
        @ 02,69 Get WENTRADA
        @ 04,14 Get WHISTORIC     Picture "@S44"
      * @ 04,69 Get WCCUSTO       Picture "!"                                           Valid win_CCUSTO()

        Setcolor(COR_CONS+",W+/R,,,"+COR_CONS)

        @ 08,01 Get vCODIGO[1]    Picture "@S7"                                                                        Valid seek_P(1)
        @ 08,46 Get vQTDE[1]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[1]) .And. vCODIGO[1] <> "99999999" 
        @ 08,55 Get vUNITARIO[1]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[1]) .And. vCODIGO[1] <> "99999999" Valid total_ENTRADA(1)

        @ 09,01 Get vCODIGO[2]    Picture "@S7"                 When ! Empty(vCODIGO[1]) .And. vCODIGO[1] <> "99999999" Valid seek_P(2)
        @ 09,46 Get vQTDE[2]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[2]) .And. vCODIGO[2] <> "99999999"
        @ 09,55 Get vUNITARIO[2]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[2]) .And. vCODIGO[2] <> "99999999" Valid total_ENTRADA(2)

        @ 10,01 Get vCODIGO[3]    Picture "@S7"                 When ! Empty(vCODIGO[2]) .And. vCODIGO[2] <> "99999999" Valid seek_P(3)
        @ 10,46 Get vQTDE[3]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[3]) .And. vCODIGO[3] <> "99999999" 
        @ 10,55 Get vUNITARIO[3]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[3]) .And. vCODIGO[3] <> "99999999" Valid total_ENTRADA(3) 

        @ 11,01 Get vCODIGO[4]    Picture "@S7"                 When ! Empty(vCODIGO[3]) .And. vCODIGO[3] <> "99999999" Valid seek_P(4)
        @ 11,46 Get vQTDE[4]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[4]) .And. vCODIGO[4] <> "99999999" 
        @ 11,55 Get vUNITARIO[4]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[4]) .And. vCODIGO[4] <> "99999999" Valid total_ENTRADA(4)

        @ 12,01 Get vCODIGO[5]    Picture "@S7"                 When ! Empty(vCODIGO[4]) .And. vCODIGO[4] <> "99999999" Valid seek_P(5)
        @ 12,46 Get vQTDE[5]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[5]) .And. vCODIGO[5] <> "99999999" 
        @ 12,55 Get vUNITARIO[5]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[5]) .And. vCODIGO[5] <> "99999999" Valid total_ENTRADA(5)

        @ 13,01 Get vCODIGO[6]    Picture "@S7"                 When ! Empty(vCODIGO[5]) .And. vCODIGO[5] <> "99999999" Valid seek_P(6)
        @ 13,46 Get vQTDE[6]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[6]) .And. vCODIGO[6] <> "99999999" 
        @ 13,55 Get vUNITARIO[6]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[6]) .And. vCODIGO[6] <> "99999999" Valid total_ENTRADA(6)

        @ 14,01 Get vCODIGO[7]    Picture "@S7"                 When ! Empty(vCODIGO[6]) .And. vCODIGO[6] <> "99999999" Valid seek_P(7)
        @ 14,46 Get vQTDE[7]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[7]) .And. vCODIGO[7] <> "99999999" 
        @ 14,55 Get vUNITARIO[7]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[7]) .And. vCODIGO[7] <> "99999999" Valid total_ENTRADA(7)

        @ 15,01 Get vCODIGO[8]    Picture "@S7"                 When ! Empty(vCODIGO[7]) .And. vCODIGO[7] <> "99999999" Valid seek_P(8)
        @ 15,46 Get vQTDE[8]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[8]) .And. vCODIGO[8] <> "99999999" 
        @ 15,55 Get vUNITARIO[8]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[8]) .And. vCODIGO[8] <> "99999999" Valid total_ENTRADA(8)

        @ 16,01 Get vCODIGO[9]    Picture "@S7"                 When ! Empty(vCODIGO[8]) .And. vCODIGO[8] <> "99999999" Valid seek_P(9)
        @ 16,46 Get vQTDE[9]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[9]) .And. vCODIGO[9] <> "99999999" 
        @ 16,55 Get vUNITARIO[9]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[9]) .And. vCODIGO[9] <> "99999999" Valid total_ENTRADA(9)

        @ 17,01 Get vCODIGO[10]   Picture "@S7"                 When ! Empty(vCODIGO[9])  .And. vCODIGO[9]  <> "99999999"  Valid seek_P(10)
        @ 17,46 Get vQTDE[10]     Picture "@Z 99999.99"         When ! Empty(vCODIGO[10]) .And. vCODIGO[10] <> "99999999"  
        @ 17,55 Get vUNITARIO[10] Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[10]) .And. vCODIGO[10] <> "99999999"  Valid total_ENTRADA(10)

        @ 18,01 Get vCODIGO[11]   Picture "@S7"                 When ! Empty(vCODIGO[10]) .And. vCODIGO[10] <> "99999999" Valid seek_P(11)
        @ 18,46 Get vQTDE[11]     Picture "@Z 99999.99"         When ! Empty(vCODIGO[11]) .And. vCODIGO[11] <> "99999999" 
        @ 18,55 Get vUNITARIO[11] Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[11]) .And. vCODIGO[11] <> "99999999" Valid total_ENTRADA(11)

        @ 19,01 Get vCODIGO[12]   Picture "@S7"                 When ! Empty(vCODIGO[11]) .And. vCODIGO[11] <> "99999999" Valid seek_P(12)
        @ 19,46 Get vQTDE[12]     Picture "@Z 99999.99"         When ! Empty(vCODIGO[12]) .And. vCODIGO[12] <> "99999999" 
        @ 19,55 Get vUNITARIO[12] Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[12]) .And. vCODIGO[13] <> "99999999" Valid total_ENTRADA(12)
   
        @ 20,01 Get vCODIGO[13]   Picture "@S7"                 When ! Empty(vCODIGO[12]) .And. vCODIGO[12] <> "99999999" Valid seek_P(13)
        @ 20,46 Get vQTDE[13]     Picture "@Z 99999.99"         When ! Empty(vCODIGO[13]) .And. vCODIGO[13] <> "99999999" 
        @ 20,55 Get vUNITARIO[13] Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[13]) .And. vCODIGO[13] <> "99999999" Valid total_ENTRADA(13)

        @ 21,01 Get vCODIGO[14]   Picture "@S7"                 When ! Empty(vCODIGO[13]) .And. vCODIGO[13] <> "99999999" Valid seek_P(14)
        @ 21,46 Get vQTDE[14]     Picture "@Z 99999.99"         When ! Empty(vCODIGO[14]) .And. vCODIGO[14] <> "99999999" 
        @ 21,55 Get vUNITARIO[14] Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[14]) .And. vCODIGO[14] <> "99999999" Valid total_ENTRADA(14)
        READ_F()
                                                                                                                            
        If Lastkey() = 27
           SET CENTURY OFF
           Restore Screen
           Return .T.
        Endif

        Setcolor(COR_CONS)

        If CONFIRMA(24,"Dados Corretos...(S/N)?","S") = "S"                                                               

           Select OUTRAS_E

           If ! REGLOCK() 
              Loop
           Endif

           save_OUT_E()

           // deleta todas as entradas 

           Select ITENS_E
           ordsetfocus('SINC001')
           Seek WSEQ

           While SEQ == WSEQ .And. ! Eof()

               If ! REGLOCK()
                  Skip
                  Loop
               Endif             

               WWCODIGO = CODIGO
               WWQTDE   = QTDE

               Replace SEQ      With 0        ,;
                       CODIGO   With Space(08),; 
                       QTDE     With 0        ,;
                       UNITARIO With 0        
      
               Delete

               GRAVA()  

               // retira as entradas de produtos do estoque 

               Select PRODUTOS
               ordsetfocus('SINC001')
               Seek WWCODIGO

               If Found()

                  While .T.

                      If ! REGLOCK()
                         Loop
                      Endif
   
                  Exit
                  Enddo

                  Replace AQUIS With AQUIS - WWQTDE

                  If AQUIS < 0
                     Replace AQUIS With 0
                  Endif

                  GRAVA()

               Endif

               Select ITENS_E
               Seek WSEQ

           Enddo

           For I=1 To 14

               If ! Empty(vCODIGO[I]) .And. vCODIGO[I] <> "99999999" .And. ! Empty(vQTDE[I])

                  While .T.

                      Select ITENS_E

                      Set Deleted Off
                      Go Top
                      If Deleted()
                         If ! REGLOCK()
                            Loop
                         Endif
                         Recall
                      Else
                         If ! iSEQUENCIAL()
                            Loop
                         Endif
                      Endif
                      Set Deleted On

                  Exit
                  Enddo

                  Replace SEQ      With WSEQ      ,;
                          CODIGO   With vCODIGO[I],;
                          QTDE     With vQTDE[I]  ,;
                          UNITARIO With vUNITARIO[I]

                  // entrada de produtos no estoque 

                  Select PRODUTOS
                  ordsetfocus('SINC001')
                  Seek vCODIGO[I]

                  While .T.

                      If ! REGLOCK()
                         Loop
                      Endif

                  Exit
                  Enddo

                  Replace AQUIS With AQUIS + vQTDE[I],;
                          CUSTO With vUNITARIO[I]

                  GRAVA()

               Endif

           Next I

           Exit

        Endif

    Enddo

Enddo

Restscreen(00,00,24,79,SAVE)

Return .T.


*******************
Function c_OUTRAS_E
*******************
Save Screen

SET CENTURY ON

While .T.

    OPC   = 4
    TRAVA = .F.

    screen_OE()

    Setcolor(COR_CONS)
    @ 24,00 Say "CONSULTA "
    @ 24,72 Say " ESC-FIM"

    SOMBRA(10,14,17,67,8)

    @ 09,12 Clear To 16,66
    @ 09,12 To 16,66 

    @ 10,14 Say "Consulta a partir da sequ"+Chr(136)+"ncia de controle......[1]"
    @ 11,14 Say "Consulta a partir do n"+Chr(163)+"mero do documento........[2]"
    @ 12,14 Say "Consulta a partir da data de emiss"+Chr(132)+"o............[3]"
    @ 13,14 Say "Retorno ao menu principal.......................[4]"

    Setcolor("W+/N")
    @ 15,14 Say "Escolha sua op"+Chr(135)+Chr(132)+"o...............................[ ]"
    @ 15,63 Get OPC Picture "9" Range 1,4
    READ_F()

    If OPC = 4 .Or. Lastkey() = 27
       SET CENTURY OFF
       Restore Screen
       Return .T.
    Endif

    screen_OE()

    Setcolor(COR_CONS)
    @ 24,00 Say "CONSULTA "
    @ 24,72 Say " ESC-FIM"

    If OPC = 1

       While .T.

           WSEQ = 0             
 
           Setcolor(COR_TEXT+","+COR_GETS+",,,"+COR_TEXT)

           @ 02,14 Get WSEQ Picture "@Z 999999"
           READ_F()

           If ! Empty(WSEQ) .Or. Lastkey() <> 27

              Select OUTRAS_E
              ordsetfocus('SINC001')
              Seek WSEQ

              If ! Found()
                 ERROR("  Sequ"+Chr(136)+"ncia de controle n"+Chr(132)+"o cadastrada!  ",0)
                 Loop
              Endif

           Else
              TRAVA = .T.
           Endif

       Exit
       Enddo

    Elseif OPC = 2

       While .T.

           WDOCNO = Space(10)
 
           Setcolor(COR_TEXT+","+COR_GETS+",,,"+COR_TEXT)

           @ 02,43 Get WDOCNO
           READ_F()

           If ! Empty(WDOCNO) .Or. Lastkey() <> 27

              Select OUTRAS_E
              ordsetfocus('SINC002')
              Seek WDOCNO

              If ! Found()
                 ERROR("  N"+Chr(163)+"mero de documento n"+Chr(132)+"o cadastrado!  ",0)
                 Loop
              Endif

           Else
              TRAVA = .T.
           Endif

       Exit
       Enddo

    Elseif OPC = 3

       WENTRADA = DATE()

       Setcolor(COR_TEXT+","+COR_GETS+",,,"+COR_TEXT)
 
       @ 02,69 Get WENTRADA
       READ_F()

       If ! Empty(WENTRADA) .Or. Lastkey() <> 27
 
          Select OUTRAS_E
          ordsetfocus('SINC003')
          Set Softseek On
          Seek WENTRADA
          Set Softseek Off

          If Eof()
             Skip -1
          Endif

       Else
          TRAVA = .T.
       Endif

    Endif

    If TRAVA
       Loop
    Endif

    While .T.                      

        screen_OE()

        Setcolor(COR_CONS)
        @ 24,00 Say "CONSULTA "
        @ 24,72 Say " ESC-FIM"

        CENTRO(24,"PgDw-Pr"+Chr(162)+"ximo    PgUp-Anterior")

        Setcolor(COR_TEXT)       

        @ 02,14 Say Strzero(SEQ,6)
        @ 02,43 Say DOCNO
        @ 02,69 Say ENTRADA
        @ 04,14 Say HISTORIC Picture "@S44"

        WSEQ   = SEQ  
        L      = 8
        tTOTAL = 0

        Select ITENS_E
        ordsetfocus('SINC001')
        Seek WSEQ

        While SEQ == WSEQ .And. ! Eof()

            Setcolor(COR_CONS)

            @ L,01 Say CODIGO            Picture "@S7"
            @ L,46 Say QTDE              Picture "@Z 99999.99" 
            @ L,55 Say UNITARIO          Picture "@Z 999,999.9999" 
            @ L,68 Say (QTDE * UNITARIO) Picture "@R@EZ 9999,999.99"

            WWCOD  = CODIGO
            tTOTAL = tTOTAL + (QTDE * UNITARIO)

            Select PRODUTOS
            ordsetfocus('SINC001')
            Seek WWCOD

            @ L,10 Say Left(PRODUTO,32)
            @ L,43 Say UN Picture "@S2"

            L++

            If L > 22
               Exit
            Endif

            Select ITENS_E
            Skip

        Enddo

        Setcolor("W+/N")
        @ 23,68 Say tTOTAL Picture "@R@E 9999,999.99" 
       
        Select OUTRAS_E

        Inkey(0)

        If Lastkey() = 28
           SCALC()

        Elseif Lastkey() = -1
           CALENDARIO()

        Elseif Lastkey() = 3
           Skip

           If Eof()
              Skip -1
           Endif

        Elseif Lastkey() = 18
           Skip -1

        Elseif Lastkey() = 27
           Exit

        Endif

    Enddo

Enddo

Return .T.


*******************
Function e_OUTRAS_E
*******************
Save Screen

While .T.

    screen_OE()

    Setcolor(COR_CONS)
    @ 24,00 Say "EXCLUS"+Chr(142)+"O "
    @ 24,72 Say " ESC-FIM"

    WSEQ = 0 

    SET CENTURY ON

    Setcolor(COR_TEXT+","+COR_GETS+",,,"+COR_TEXT)

    @ 02,14 Get WSEQ Picture "@Z 999999"
    READ_F()
    @ 02,14 Say Strzero(WSEQ,6)

    If Empty(WSEQ) .Or. Lastkey() = 27
       SET CENTURY OFF
       Restore Screen
       Return .T.
    Endif

    Select OUTRAS_E
    ordsetfocus('SINC001')
    Seek WSEQ

    If ! Found()
       ERROR("  Sequ"+Chr(136)+"ncia de controle n"+Chr(132)+"o cadastrado!  ",0)
       Loop
    Endif

    @ 02,43 Say DOCNO
    @ 02,69 Say ENTRADA
    @ 04,14 Say HISTORIC Picture "@S44"

    I = 1
    L = 8

    Select ITENS_E
    ordsetfocus('SINC001')
    Seek WSEQ

    While SEQ == WSEQ .And. ! Eof()

        Setcolor(COR_CONS)

        @ L,01 Say CODIGO            Picture "@S7"
        @ L,46 Say QTDE              Picture "@Z 99999.99" 
        @ L,55 Say UNITARIO          Picture "@Z 999,999.9999" 
        @ L,68 Say (QTDE * UNITARIO) Picture "@R@EZ 9999,999.99"

        WWCOD = CODIGO

        Select PRODUTOS
        ordsetfocus('SINC001')
        Seek WWCOD

        @ L,10 Say Left(PRODUTO,32)
        @ L,43 Say UN Picture "@S2"

        I++
        L++

        If I > 14
           Exit
        Endif

        Select ITENS_E
        Skip

    Enddo

    Setcolor(COR_CONS)

    If CONFIRMA(24,"Voc"+Chr(136)+" deseja excluir este documento...(S/N)?","N") = "S"                                                               

       Select OUTRAS_E

       If ! REGLOCK() 
          Loop
       Endif

       Replace SEQ      With 0              ,;
               DOCNO    With Space(10)      ,;
               ENTRADA  With Ctod(Space(08)),;
               HISTORIC With Space(54)      ,;
               CCUSTO   With Space(01)

       Delete

       GRAVA()

       // deleta todas as entradas 

       Select ITENS_E
       ordsetfocus('SINC001')
       Seek WSEQ

       While SEQ == WSEQ .And. ! Eof()

           If ! REGLOCK()
              Skip
              Loop
           Endif             

           WWCODIGO = CODIGO
           WWQTDE   = QTDE

           Replace SEQ      With 0        ,;
                   CODIGO   With Space(08),; 
                   QTDE     With 0        ,;
                   UNITARIO With 0        
    
           Delete

           GRAVA()

           // retira as entradas de produtos do estoque 

           Select PRODUTOS
           ordsetfocus('SINC001')
           Seek WWCODIGO

           While .T.

              If ! REGLOCK()
                 Loop
              Endif

          Exit
          Enddo

          Replace AQUIS With AQUIS + WWQTDE

          If AQUIS < 0
             Replace AQUIS With 0
          Endif

          GRAVA()

          Select ITENS_E
          Seek WSEQ

       Enddo

    Endif

Enddo

Return .T.


***************
Function SAIDAS
***************
WIN451 = 1

While .T.

    Setcolor(COR_CAB)   
    @ 03,42 Say "SAIDA DE ESTOQUE (REQUISI"+Chr(128)+Chr(153)+"ES)     " 
    Setcolor(COR_TEXT)

    Private Opcao[5]

    Opcao[1] = " Inclus"+Chr(132)+"o  " 
    Opcao[2] = " Altera"+Chr(135)+Chr(132)+"o " 
    Opcao[3] = " Consulta  " 
    Opcao[4] = " Exclus"+Chr(132)+"o  " 
    Opcao[5] = " Fim       " 

    private ACESSO[5]
    ACESSO[1] = .T.
    ACESSO[2] = .T.
    ACESSO[3] = .T.
    ACESSO[4] = .T.
    ACESSO[5] = .T.

    Private Letra[5]

    Letra[1] = "I"  
    Letra[2] = "A"
    Letra[3] = "C"
    Letra[4] = "E"
    Letra[5] = "F"

    Private Pos[5]

    Pos[1] = 1
    Pos[2] = 1
    Pos[3] = 1
    Pos[4] = 1
    Pos[5] = 1

    Private Obs[5]

    Obs[1] = "Inclus"+Chr(132)+"o de saidas de produtos!"     
    Obs[2] = "Altera"+Chr(135)+Chr(132)+"o de saidas de produtos!"
    Obs[3] = "Consulta as outras saidas de produtos!"
    Obs[4] = "Exclus"+Chr(132)+"o de saidas de produtos!"
    Obs[5] = "Retorno ao menu principal!"                   

    WIN451 = ACHOISE(WIN451,10,33,16,45,2,OPCAO,ACESSO,LETRA,POS,OBS,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)
   
    If WIN451 = 0
       Return .T.
    Elseif WIN451 = 1 
       i_OUTRAS_S()   
    Elseif WIN451 = 2 
       a_OUTRAS_S()   
    Elseif WIN451 = 3 
       c_OUTRAS_S()   
    Elseif WIN451 = 4 
       e_OUTRAS_S() 
    Elseif WIN451 = 5
       Return .T.
    Endif

Enddo

Return .T.


******************
Function screen_OS
******************

Setcolor(COR_TEXT)
@ 00,00 Clear To 24,79

Setcolor(COR_CONS)
@ 00,00 Clear To 00,79
@ 24,00 Clear To 24,79

@ 00,27 Say "SAIDAS DE PRODUTOS DO ESTOQUE"

Setcolor(COR_TEXT)
@ 01,00 Clear To 23,79

@ 02,01 Say "Sequ"+Chr(136)+"ncia..:                   Documento:                  Saida..:"
@ 04,01 Say "Requisitado:                                               C.Custo:"

SOMBRA(07,03,23,79,8)

Setcolor(COR_CONS)
@ 06,01 Clear To 22,78
@ 06,01 Clear To 10,78

@ 06,01 Say "CODIGO   PRODUTO/SERVI"+Chr(128)+"O                  UN     QTDE     UNITARIO       TOTAL"

@ 06,08 Say Chr(179)
@ 06,42 Say Chr(179)
@ 06,45 Say Chr(179)
@ 06,54 Say Chr(179)
@ 06,67 Say Chr(179)

@ 07,08 To 22,08
@ 07,42 To 22,42
@ 07,45 To 22,45
@ 07,54 To 22,54
@ 07,67 To 22,67

@ 07,01 To 07,78 Double

@ 07,08 Say Chr(216)
@ 07,42 Say Chr(216)
@ 07,45 Say Chr(216)
@ 07,54 Say Chr(216)
@ 07,67 Say Chr(216)

Setcolor(COR_TEXT)

Return .T.


*******************
Function i_OUTRAS_S
*******************
Save Screen

While .T.

    screen_OS()

    Setcolor(COR_CONS)
    @ 24,00 Say "INCLUS"+Chr(142)+"O "
    @ 24,72 Say " ESC-FIM"

    WSEQ      = 0 
    WDOCNO    = Space(10)
    WSAIDA    = DATE()
    WHISTORIC = Space(54)
    WCCUSTO   = Space(01)

    Declare vCODIGO[14] 
    Declare vQTDE[14] 
    Declare vUNITARIO[14] 

    Afill(vCODIGO,Space(08))
    Afill(vQTDE,0)
    Afill(vUNITARIO,0)

    SET CENTURY ON

    Setcolor(COR_TEXT)
    @ 02,14 Say "??????"

    While .T.

        Setcolor(COR_TEXT+","+COR_GETS+",,,"+COR_TEXT)

        @ 02,43 Get WDOCNO
        @ 02,69 Get WSAIDA  
        @ 04,14 Get WHISTORIC     Picture "@S44"                                        
        @ 04,69 Get WCCUSTO       Picture "!"                                           Valid win_CCUSTO()

        Setcolor(COR_CONS+",W+/R,,,"+COR_CONS)

        @ 08,01 Get vCODIGO[1]    Picture "@S7"                                                                          Valid seek_PD(1)         
        @ 08,46 Get vQTDE[1]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[1])  .And. vCODIGO[1] <> "99999999"                         
        @ 08,55 Get vUNITARIO[1]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[1])  .And. vCODIGO[1] <> "99999999"  Valid total_SAIDA(1)  
                                                                                                                
        @ 09,01 Get vCODIGO[2]    Picture "@S7"                 When ! Empty(vCODIGO[1])  .And. vCODIGO[1] <> "99999999"  Valid seek_PD(2)         
        @ 09,46 Get vQTDE[2]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[2])  .And. vCODIGO[2] <> "99999999"                         
        @ 09,55 Get vUNITARIO[2]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[2])  .And. vCODIGO[2] <> "99999999"  Valid total_SAIDA(2)  

        @ 10,01 Get vCODIGO[3]    Picture "@S7"                 When ! Empty(vCODIGO[2])  .And. vCODIGO[2] <> "99999999"  Valid seek_PD(3)         
        @ 10,46 Get vQTDE[3]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[3])  .And. vCODIGO[3] <> "99999999"                          
        @ 10,55 Get vUNITARIO[3]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[3])  .And. vCODIGO[3] <> "99999999"  Valid total_SAIDA(3)  
                                                                                                                
        @ 11,01 Get vCODIGO[4]    Picture "@S7"                 When ! Empty(vCODIGO[3])  .And. vCODIGO[3] <> "99999999"  Valid seek_PD(4)         
        @ 11,46 Get vQTDE[4]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[4])  .And. vCODIGO[4] <> "99999999"                          
        @ 11,55 Get vUNITARIO[4]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[4])  .And. vCODIGO[4] <> "99999999"  Valid total_SAIDA(4)  
                                                                                                                
        @ 12,01 Get vCODIGO[5]    Picture "@S7"                 When ! Empty(vCODIGO[4])  .And. vCODIGO[4] <> "99999999"  Valid seek_PD(5)         
        @ 12,46 Get vQTDE[5]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[5])  .And. vCODIGO[5] <> "99999999"                          
        @ 12,55 Get vUNITARIO[5]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[5])  .And. vCODIGO[5] <> "99999999"  Valid total_SAIDA(5)  
                                                                                                                
        @ 13,01 Get vCODIGO[6]    Picture "@S7"                 When ! Empty(vCODIGO[5])  .And. vCODIGO[5] <> "99999999"  Valid seek_PD(6)         
        @ 13,46 Get vQTDE[6]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[6])  .And. vCODIGO[6] <> "99999999"                          
        @ 13,55 Get vUNITARIO[6]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[6])  .And. vCODIGO[6] <> "99999999"  Valid total_SAIDA(6)  
                                                                                                                
        @ 14,01 Get vCODIGO[7]    Picture "@S7"                 When ! Empty(vCODIGO[6])  .And. vCODIGO[6] <> "99999999"  Valid seek_PD(7)         
        @ 14,46 Get vQTDE[7]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[7])  .And. vCODIGO[7] <> "99999999"                          
        @ 14,55 Get vUNITARIO[7]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[7])  .And. vCODIGO[7] <> "99999999"  Valid total_SAIDA(7)  
                                                                                                               
        @ 15,01 Get vCODIGO[8]    Picture "@S7"                 When ! Empty(vCODIGO[7])  .And. vCODIGO[7] <> "99999999"  Valid seek_PD(8)         
        @ 15,46 Get vQTDE[8]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[8])  .And. vCODIGO[8] <> "99999999"                          
        @ 15,55 Get vUNITARIO[8]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[8])  .And. vCODIGO[8] <> "99999999"  Valid total_SAIDA(8)  
                                                                                                                
        @ 16,01 Get vCODIGO[9]    Picture "@S7"                 When ! Empty(vCODIGO[8])  .And. vCODIGO[8] <> "99999999"  Valid seek_PD(9)         
        @ 16,46 Get vQTDE[9]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[9])  .And. vCODIGO[9] <> "99999999"                          
        @ 16,55 Get vUNITARIO[9]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[9])  .And. vCODIGO[9] <> "99999999"  Valid total_SAIDA(9)  
                                                                                                                
        @ 17,01 Get vCODIGO[10]   Picture "@S7"                 When ! Empty(vCODIGO[9])  .And. vCODIGO[9]  <> "99999999" Valid seek_PD(10)       
        @ 17,46 Get vQTDE[10]     Picture "@Z 99999.99"         When ! Empty(vCODIGO[10]) .And. vCODIGO[10] <> "99999999"                         
        @ 17,55 Get vUNITARIO[10] Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[10]) .And. vCODIGO[10] <> "99999999" Valid total_SAIDA(10)
                                                                                                               
        @ 18,01 Get vCODIGO[11]   Picture "@S7"                 When ! Empty(vCODIGO[10]) .And. vCODIGO[10] <> "99999999" Valid seek_PD(11)       
        @ 18,46 Get vQTDE[11]     Picture "@Z 99999.99"         When ! Empty(vCODIGO[11]) .And. vCODIGO[11] <> "99999999"                        
        @ 18,55 Get vUNITARIO[11] Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[11]) .And. vCODIGO[11] <> "99999999" Valid total_SAIDA(11)
                                                                                                                
        @ 19,01 Get vCODIGO[12]   Picture "@S7"                 When ! Empty(vCODIGO[11]) .And. vCODIGO[11] <> "99999999" Valid seek_PD(12)       
        @ 19,46 Get vQTDE[12]     Picture "@Z 99999.99"         When ! Empty(vCODIGO[12]) .And. vCODIGO[12] <> "99999999"                        
        @ 19,55 Get vUNITARIO[12] Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[12]) .And. vCODIGO[12] <> "99999999" Valid total_SAIDA(12)
                                                                                                                
        @ 20,01 Get vCODIGO[13]   Picture "@S7"                 When ! Empty(vCODIGO[12]) .And. vCODIGO[12] <> "99999999" Valid seek_PD(13)       
        @ 20,46 Get vQTDE[13]     Picture "@Z 99999.99"         When ! Empty(vCODIGO[13]) .And. vCODIGO[13] <> "99999999"                        
        @ 20,55 Get vUNITARIO[13] Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[13]) .And. vCODIGO[13] <> "99999999" Valid total_SAIDA(13)
                                                                                                                
        @ 21,01 Get vCODIGO[14]   Picture "@S7"                 When ! Empty(vCODIGO[13]) .And. vCODIGO[13] <> "99999999" Valid seek_PD(14)       
        @ 21,46 Get vQTDE[14]     Picture "@Z 99999.99"         When ! Empty(vCODIGO[14]) .And. vCODIGO[14] <> "99999999"                        
        @ 21,55 Get vUNITARIO[14] Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[14]) .And. vCODIGO[14] <> "99999999" Valid total_SAIDA(14)
        READ_F()

        If Lastkey() = 27
           SET CENTURY OFF
           Restore Screen
           Return .T.
        Endif
                                                                                                                            
        Setcolor(COR_CONS)

        If CONFIRMA(24,"Dados corretos...(S/N)?","S") = "S"                                                               

           While .T.

               Select OUTRAS_S
               ordsetfocus('SINC001')
               Go Bottom
   
               WSEQ = SEQ + 1

               Seek WSEQ
   
               If Found()            
                  Loop
               Endif 
                
           Exit
           Enddo

           Set Deleted Off
           Go Top
           If Deleted()
              If ! REGLOCK()
                 Loop
              Endif
              Recall
           Else
              If ! iSEQUENCIAL()
                 Loop
              Endif
           Endif
           Set Deleted On

           Replace SEQ With WSEQ  

           save_OUT_S()

           For I=1 To 14

               If ! Empty(vCODIGO[I]) .And. vCODIGO[I] <> "99999999" .And. vQTDE[I] <> 0

                  While .T.

                      Select ITENS_S

                      Set Deleted Off
                      Go Top
                      If Deleted()
                         If ! REGLOCK()
                            Loop
                         Endif
                         Recall
                      Else
                         If ! iSEQUENCIAL()
                            Loop
                         Endif
                      Endif
                      Set Deleted On

                  Exit
                  Enddo

                  Replace SEQ      With WSEQ       ,;
                          CODIGO   With vCODIGO[I] ,;
                          QTDE     With vQTDE[I]   ,;
                          UNITARIO With vUNITARIO[I]

                  // saidas de produtos no estoque 

                  Select PRODUTOS
                  ordsetfocus('SINC001')
                  Seek vCODIGO[I]

                  While .T.

                      If ! REGLOCK()
                         Loop
                      Endif

                  Exit
                  Enddo

                  Replace BAIXAS With BAIXAS + vQTDE[I]

                  GRAVA()

               Endif

           Next I

           Tone(1001,0.7)
           Tone(1002,0.4)

           Setcolor(COR_TEXT+"*")
           @ 02,14 Say Strzero(WSEQ,6)
           Setcolor(COR_TEXT)

           GRAVA()

           Inkey(3)

           Exit

        Endif

    Enddo

Enddo

Restscreen(00,00,24,79,SAVE)

Return .T.


*******************
Function seek_PD(I)
*******************

LIN = ROW()

If Lastkey() = 27
   Return .T.
Endif

If vCODIGO[I] = Replicate("9",8)
   Setcolor(COR_CONS)
   @ LIN,10 Say Space(32)           
   @ LIN,43 Say Space(02)
   @ LIN,46 Say Space(08)
   @ LIN,55 Say Space(12)
   @ LIN,68 Say Space(11)
   Return .T.
Endif

If Empty(vCODIGO[I])
   help_PRODUTOS()
Endif

Select PRODUTOS
ordsetfocus('SINC001')
Seek vCODIGO[I]

If ! Found()
   ERROR("  C"+Chr(162)+"digo de produto n"+Chr(132)+"o cadastrado!  ",0)
   Return .F.
Endif

vUNITARIO[I] = CUSTO

Setcolor(COR_CONS)

@ LIN,10 Say Left(PRODUTO,32)
@ LIN,43 Say UN    Picture "@S2"
@ LIN,55 Say CUSTO Picture "@R@E@Z 999,999.9999" 

Return .T.


***********************
Function total_SAIDA(I)
***********************

Setcolor(COR_CONS)
@ ROW(),68 Say (vQTDE[I] * vUNITARIO[I]) Picture "@R@E@Z 9999,999.99"

tTOTAL = 0

For I=1 To 14
    tTOTAL = tTOTAL + (vQTDE[I] * vUNITARIO[I])
Next

Setcolor("W+/N")
@ 23,68 Say tTOTAL Picture "@R@E 9999,999.99" 
  
Setcolor(COR_CONS)

Return .T.


*******************
Function save_OUT_S
*******************

Replace DOCNO    With WDOCNO   ,;
        SAIDA    With WSAIDA   ,;
        HISTORIC With WHISTORIC,;
        CCUSTO   With WCCUSTO  

Return .T.


*******************
Function a_OUTRAS_S
*******************
Save Screen

While .T.

    screen_OS()

    Setcolor(COR_CONS)
    @ 24,00 Say "ALTERA"+Chr(128)+Chr(142)+"O "
    @ 24,72 Say " ESC-FIM"

    WSEQ = 0 

    Declare vCODIGO[14] 
    Declare vQTDE[14] 
    Declare vUNITARIO[14] 

    Afill(vCODIGO,Space(08))
    Afill(vQTDE,0)
    Afill(vUNITARIO,0)

    SET CENTURY ON

    Setcolor(COR_TEXT+","+COR_GETS+",,,"+COR_TEXT)

    @ 02,14 Get WSEQ Picture "@Z 999999"
    READ_F()
    @ 02,14 Say Strzero(WSEQ,6)

    If Empty(WSEQ) .Or. Lastkey() = 27
       SET CENTURY OFF
       Restore Screen
       Return .T.
    Endif

    Select OUTRAS_S
    ordsetfocus('SINC001')
    Seek WSEQ

    If ! Found()
       ERROR("  Sequ"+Chr(136)+"ncia de controle n"+Chr(132)+"o cadastrado!  ",0)
       Return .T.
    Endif

    If ! REGLOCK() 
       Loop
    Endif

    WDOCNO    = DOCNO
    WSAIDA    = SAIDA  
    WHISTORIC = HISTORIC
    WCCUSTO   = CCUSTO  

    If CCUSTO = "E"
       @ 04,69 Say "Eq"+Chr(129)+"ino"
    Elseif CCUSTO = "B"
       @ 04,69 Say "Bovino"
    Else
       @ 04,69 Say "      "
    Endif

    I = 1
    L = 8

    Select ITENS_S
    ordsetfocus('SINC001')
    Seek WSEQ

    While SEQ == WSEQ .And. ! Eof()

        vCODIGO[I]   = CODIGO
        vQTDE[I]     = QTDE
        vUNITARIO[I] = UNITARIO

        Setcolor(COR_CONS)

        @ L,01 Say vCODIGO[I]                Picture "@S7"
        @ L,46 Say vQTDE[I]                  Picture "@Z 99999.99" 
        @ L,55 Say vUNITARIO[I]              Picture "@Z 999,999.9999" 
        @ L,68 Say (vQTDE[I] * vUNITARIO[I]) Picture "@R@EZ 9999,999.99"

        Select PRODUTOS
        ordsetfocus('SINC001')
        Seek vCODIGO[I]

        @ L,10 Say Left(PRODUTO,32)
        @ L,43 Say UN Picture "@S2"

        I++
        L++

        If I > 14
           Exit
        Endif

        Select ITENS_S
        Skip

    Enddo

    While .T.

        Setcolor(COR_TEXT+","+COR_GETS+",,,"+COR_TEXT)

        @ 02,43 Get WDOCNO
        @ 02,69 Get WSAIDA  
        @ 04,14 Get WHISTORIC     Picture "@S44"                                        
        @ 04,69 Get WCCUSTO       Picture "!"                                           Valid win_CCUSTO()

        Setcolor(COR_CONS+",W+/R,,,"+COR_CONS)

        @ 08,01 Get vCODIGO[1]    Picture "@S7"                                                                          Valid seek_PD(1)         
        @ 08,46 Get vQTDE[1]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[1])  .And. vCODIGO[1] <> "99999999"                         
        @ 08,55 Get vUNITARIO[1]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[1])  .And. vCODIGO[1] <> "99999999"  Valid total_SAIDA(1)  
                                                                                                                
        @ 09,01 Get vCODIGO[2]    Picture "@S7"                 When ! Empty(vCODIGO[1])  .And. vCODIGO[1] <> "99999999"  Valid seek_PD(2)         
        @ 09,46 Get vQTDE[2]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[2])  .And. vCODIGO[2] <> "99999999"                         
        @ 09,55 Get vUNITARIO[2]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[2])  .And. vCODIGO[2] <> "99999999"  Valid total_SAIDA(2)  

        @ 10,01 Get vCODIGO[3]    Picture "@S7"                 When ! Empty(vCODIGO[2])  .And. vCODIGO[2] <> "99999999"  Valid seek_PD(3)         
        @ 10,46 Get vQTDE[3]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[3])  .And. vCODIGO[3] <> "99999999"                          
        @ 10,55 Get vUNITARIO[3]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[3])  .And. vCODIGO[3] <> "99999999"  Valid total_SAIDA(3)  
                                                                                                                
        @ 11,01 Get vCODIGO[4]    Picture "@S7"                 When ! Empty(vCODIGO[3])  .And. vCODIGO[3] <> "99999999"  Valid seek_PD(4)         
        @ 11,46 Get vQTDE[4]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[4])  .And. vCODIGO[4] <> "99999999"                          
        @ 11,55 Get vUNITARIO[4]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[4])  .And. vCODIGO[4] <> "99999999"  Valid total_SAIDA(4)  
                                                                                                                
        @ 12,01 Get vCODIGO[5]    Picture "@S7"                 When ! Empty(vCODIGO[4])  .And. vCODIGO[4] <> "99999999"  Valid seek_PD(5)         
        @ 12,46 Get vQTDE[5]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[5])  .And. vCODIGO[5] <> "99999999"                          
        @ 12,55 Get vUNITARIO[5]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[5])  .And. vCODIGO[5] <> "99999999"  Valid total_SAIDA(5)  
                                                                                                                
        @ 13,01 Get vCODIGO[6]    Picture "@S7"                 When ! Empty(vCODIGO[5])  .And. vCODIGO[5] <> "99999999"  Valid seek_PD(6)         
        @ 13,46 Get vQTDE[6]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[6])  .And. vCODIGO[6] <> "99999999"                          
        @ 13,55 Get vUNITARIO[6]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[6])  .And. vCODIGO[6] <> "99999999"  Valid total_SAIDA(6)  
                                                                                                                
        @ 14,01 Get vCODIGO[7]    Picture "@S7"                 When ! Empty(vCODIGO[6])  .And. vCODIGO[6] <> "99999999"  Valid seek_PD(7)         
        @ 14,46 Get vQTDE[7]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[7])  .And. vCODIGO[7] <> "99999999"                          
        @ 14,55 Get vUNITARIO[7]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[7])  .And. vCODIGO[7] <> "99999999"  Valid total_SAIDA(7)  
                                                                                                                
        @ 15,01 Get vCODIGO[8]    Picture "@S7"                 When ! Empty(vCODIGO[7])  .And. vCODIGO[7] <> "99999999"  Valid seek_PD(8)         
        @ 15,46 Get vQTDE[8]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[8])  .And. vCODIGO[8] <> "99999999"                          
        @ 15,55 Get vUNITARIO[8]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[8])  .And. vCODIGO[8] <> "99999999"  Valid total_SAIDA(8)  
                                                                                                                
        @ 16,01 Get vCODIGO[9]    Picture "@S7"                 When ! Empty(vCODIGO[8])  .And. vCODIGO[8] <> "99999999"  Valid seek_PD(9)         
        @ 16,46 Get vQTDE[9]      Picture "@Z 99999.99"         When ! Empty(vCODIGO[9])  .And. vCODIGO[9] <> "99999999"                          
        @ 16,55 Get vUNITARIO[9]  Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[9])  .And. vCODIGO[9] <> "99999999"  Valid total_SAIDA(9)  
                                                                                                                
        @ 17,01 Get vCODIGO[10]   Picture "@S7"                 When ! Empty(vCODIGO[9])  .And. vCODIGO[9]  <> "99999999" Valid seek_PD(10)       
        @ 17,46 Get vQTDE[10]     Picture "@Z 99999.99"         When ! Empty(vCODIGO[10]) .And. vCODIGO[10] <> "99999999"                         
        @ 17,55 Get vUNITARIO[10] Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[10]) .And. vCODIGO[10] <> "99999999" Valid total_SAIDA(10)
                                                                                                                
        @ 18,01 Get vCODIGO[11]   Picture "@S7"                 When ! Empty(vCODIGO[10]) .And. vCODIGO[10] <> "99999999" Valid seek_PD(11)       
        @ 18,46 Get vQTDE[11]     Picture "@Z 99999.99"         When ! Empty(vCODIGO[11]) .And. vCODIGO[11] <> "99999999"                        
        @ 18,55 Get vUNITARIO[11] Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[11]) .And. vCODIGO[11] <> "99999999" Valid total_SAIDA(11)
                                                                                                                
        @ 19,01 Get vCODIGO[12]   Picture "@S7"                 When ! Empty(vCODIGO[11]) .And. vCODIGO[11] <> "99999999" Valid seek_PD(12)       
        @ 19,46 Get vQTDE[12]     Picture "@Z 99999.99"         When ! Empty(vCODIGO[12]) .And. vCODIGO[12] <> "99999999"                        
        @ 19,55 Get vUNITARIO[12] Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[12]) .And. vCODIGO[12] <> "99999999" Valid total_SAIDA(12)
                                                                                                                
        @ 20,01 Get vCODIGO[13]   Picture "@S7"                 When ! Empty(vCODIGO[12]) .And. vCODIGO[12] <> "99999999" Valid seek_PD(13)       
        @ 20,46 Get vQTDE[13]     Picture "@Z 99999.99"         When ! Empty(vCODIGO[13]) .And. vCODIGO[13] <> "99999999"                        
        @ 20,55 Get vUNITARIO[13] Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[13]) .And. vCODIGO[13] <> "99999999" Valid total_SAIDA(13)
                                                                                                                
        @ 21,01 Get vCODIGO[14]   Picture "@S7"                 When ! Empty(vCODIGO[13]) .And. vCODIGO[13] <> "99999999" Valid seek_PD(14)       
        @ 21,46 Get vQTDE[14]     Picture "@Z 99999.99"         When ! Empty(vCODIGO[14]) .And. vCODIGO[14] <> "99999999"                        
        @ 21,55 Get vUNITARIO[14] Picture "@R@E@Z 999,999.9999" When ! Empty(vCODIGO[14]) .And. vCODIGO[14] <> "99999999" Valid total_SAIDA(14)
        READ_F()
                                                                                                                            
        If Lastkey() = 27
           SET CENTURY OFF
           Restore Screen
           Return .T.
        Endif

        Setcolor(COR_CONS)

        If CONFIRMA(24,"Dados corretos...(S/N)?","S") = "S"                                                               

           Select OUTRAS_S

           save_OUT_S()

           // deleta todas as saidas   

           Select ITENS_S
           ordsetfocus('SINC001')
           Seek WSEQ

           While SEQ == WSEQ .And. ! Eof()

               If ! REGLOCK()
                  Skip
                  Loop
               Endif             

               WWCODIGO = CODIGO
               WWQTDE   = QTDE

               Replace SEQ      With 0        ,;
                       CODIGO   With Space(08),; 
                       QTDE     With 0        ,;
                       UNITARIO With 0        
       
               Delete

               GRAVA()

               // retira as baixas de produtos do estoque 

               Select PRODUTOS
               ordsetfocus('SINC001')
               Seek WWCODIGO

               While .T.

                   If ! REGLOCK()
                      Loop
                   Endif

               Exit
               Enddo

               Replace BAIXAS With BAIXAS - WWQTDE

               If BAIXAS < 0
                  Replace BAIXAS With 0
               Endif

               GRAVA()

               Select ITENS_S
               Seek WSEQ

           Enddo

           For I=1 To 14

               If ! Empty(vCODIGO[I]) .And. vCODIGO[I] <> "99999999" .And. vQTDE[I] > 0

                  While .T.

                      Select ITENS_S

                      Set Deleted Off
                      Go Top
                      If Deleted()
                         If ! REGLOCK()
                            Loop
                         Endif
                         Recall
                      Else
                         If ! iSEQUENCIAL()
                            Loop
                         Endif
                      Endif
                      Set Deleted On

                  Exit
                  Enddo

                  Replace SEQ      With WSEQ        ,;
                          CODIGO   With vCODIGO[I]  ,;
                          QTDE     With vQTDE[I]    ,;
                          UNITARIO With vUNITARIO[I]

                  // baixa de produtos no estoque 

                  Select PRODUTOS
                  ordsetfocus('SINC001')
                  Seek vCODIGO[I]

                  While .T.

                      If ! REGLOCK()
                         Loop
                      Endif

                  Exit
                  Enddo

                  Replace BAIXAS With BAIXAS + vQTDE[I]

                  GRAVA()

               Endif

           Next I

           Exit

        Endif

    Enddo

Enddo

Return .T.


*******************
Function c_OUTRAS_S
*******************
Save Screen

SET CENTURY ON

While .T.

    OPC   = 4
    TRAVA = .F.

    screen_OS()

    Setcolor(COR_CONS)
    @ 24,00 Say "CONSULTA "
    @ 24,72 Say " ESC-FIM"

    SOMBRA(10,14,17,67,8)

    @ 09,12 Clear To 16,66
    @ 09,12 To 16,66 

    @ 10,14 Say "Consulta a partir da sequ"+Chr(136)+"ncia de controle......[1]"
    @ 11,14 Say "Consulta a partir do n"+Chr(163)+"mero do documento........[2]"
    @ 12,14 Say "Consulta a partir da data de emiss"+Chr(132)+"o............[3]"
    @ 13,14 Say "Retorno ao menu principal.......................[4]"

    Setcolor("W+/N")
    @ 15,14 Say "Escolha sua op"+Chr(135)+Chr(132)+"o...............................[ ]"
    @ 15,63 Get OPC Picture "9" Range 1,4
    READ_F()

    If OPC = 4 .Or. Lastkey() = 27
       SET CENTURY OFF
       Restore Screen
       Return .T.
    Endif

    screen_OS()

    Setcolor(COR_CONS)
    @ 24,00 Say "CONSULTA "
    @ 24,72 Say " ESC-FIM"

    If OPC = 1

       While .T.

           WSEQ = 0             
 
           Setcolor(COR_TEXT+","+COR_GETS+",,,"+COR_TEXT)

           @ 02,14 Get WSEQ Picture "@Z 999999"
           READ_F()

           If ! Empty(WSEQ) .Or. Lastkey() <> 27

              Select OUTRAS_S
              ordsetfocus('SINC001')
              Seek WSEQ

              If ! Found()
                 ERROR("  Sequ"+Chr(136)+"ncia de controle n"+Chr(132)+"o cadastrada!  ",0)
                 Loop
              Endif

           Else
              TRAVA = .T.
           Endif

       Exit
       Enddo

    Elseif OPC = 2

       While .T.

           WDOCNO = Space(10)
 
           Setcolor(COR_TEXT+","+COR_GETS+",,,"+COR_TEXT)

           @ 02,43 Get WDOCNO
           READ_F()

           If ! Empty(WDOCNO) .Or. Lastkey() <> 27

              Select OUTRAS_S
              ordsetfocus('SINC002')
              Seek WDOCNO

              If ! Found()
                 ERROR("  N"+Chr(163)+"mero de documento n"+Chr(132)+"o cadastrado!  ",0)
                 Loop
              Endif

           Else
              TRAVA = .T.
           Endif

       Exit
       Enddo

    Elseif OPC = 3

       WENTRADA = DATE()

       Setcolor(COR_TEXT+","+COR_GETS+",,,"+COR_TEXT)
 
       @ 02,69 Get WENTRADA
       READ_F()

       If ! Empty(WENTRADA) .Or. Lastkey() <> 27
 
          Select OUTRAS_S
          ordsetfocus('SINC003')
          Go Top
          Set Softseek On
          Seek WENTRADA
          Set Softseek Off

          If Eof()
             Skip -1
          Endif

       Else
          TRAVA = .T.
       Endif

    Endif

    If TRAVA
       Loop
    Endif

    While .T.                      

        screen_OS()

        Setcolor(COR_CONS)
        @ 24,00 Say "CONSULTA "
        @ 24,72 Say " ESC-FIM"

        CENTRO(24,"PgDw-Pr"+Chr(162)+"ximo    PgUp-Anterior")

        Setcolor(COR_TEXT)       

        @ 02,14 Say Strzero(SEQ,6)
        @ 02,43 Say DOCNO
        @ 02,69 Say SAIDA
        @ 04,14 Say HISTORIC Picture "@S44"

        If CCUSTO = "E"
           @ 04,69 Say "Eq"+Chr(129)+"ino"
        Elseif CCUSTO = "B"
           @ 04,69 Say "Bovino"
        Else
           @ 04,69 Say "      "
        Endif

        WSEQ   = SEQ  
        L      = 8
        tTOTAL = 0

        Select ITENS_S
        ordsetfocus('SINC001')
        Seek WSEQ

        While SEQ == WSEQ .And. ! Eof()

            Setcolor(COR_CONS)

            @ L,01 Say CODIGO            Picture "@S7"
            @ L,46 Say QTDE              Picture "@Z 99999.99" 
            @ L,55 Say UNITARIO          Picture "@Z 999,999.9999" 
            @ L,68 Say (QTDE * UNITARIO) Picture "@R@EZ 9999,999.99"

            WWCOD  = CODIGO
            tTOTAL = tTOTAL + (QTDE * UNITARIO)

            Select PRODUTOS
            ordsetfocus('SINC001')
            Seek WWCOD

            @ L,10 Say Left(PRODUTO,32)
            @ L,43 Say UN Picture "@S2"

            L++

            If L > 22
               Exit
            Endif

            Select ITENS_S
            Skip

        Enddo

        Setcolor("W+/N")
        @ 23,68 Say tTOTAL Picture "@R@E 9999,999.99" 
       
        Select OUTRAS_S

        Inkey(0)

        If Lastkey() = 28
           SCALC()

        Elseif Lastkey() = -1
           CALENDARIO()

        Elseif Lastkey() = 3
           Skip

           If Eof()
              Skip -1
           Endif

        Elseif Lastkey() = 18
           Skip -1

        Elseif Lastkey() = 27
           Exit

        Endif

    Enddo

Enddo

Return .T.


*******************
Function e_OUTRAS_S
*******************
Save Screen

While .T.

    screen_OS()

    Setcolor(COR_CONS)
    @ 24,00 Say "EXCLUS"+Chr(142)+"O "
    @ 24,72 Say " ESC-FIM"

    WSEQ = 0 

    SET CENTURY ON

    Setcolor(COR_TEXT+","+COR_GETS+",,,"+COR_TEXT)

    @ 02,14 Get WSEQ Picture "@Z 999999"
    READ_F()
    @ 02,14 Say Strzero(WSEQ,6)

    If Empty(WSEQ) .Or. Lastkey() = 27
       SET CENTURY OFF
       Restore Screen
       Return .T.
    Endif

    Select OUTRAS_S
    ordsetfocus('SINC001')
    Seek WSEQ

    If ! Found()
       ERROR("  Sequ"+Chr(136)+"ncia de controle n"+Chr(132)+"o cadastrado!  ",0)
       Loop
    Endif

    @ 02,43 Say DOCNO
    @ 02,69 Say SAIDA  
    @ 04,14 Say HISTORIC Picture "@S44"

    If CCUSTO = "E"
       @ 04,69 Say "Eq"+Chr(129)+"ino"
    Elseif CCUSTO = "B"
       @ 04,69 Say "Bovino"
    Else
       @ 04,69 Say "      "
    Endif

    I = 1
    L = 8

    Select ITENS_S
    ordsetfocus('SINC001')
    Seek WSEQ

    While SEQ == WSEQ .And. ! Eof()

        Setcolor(COR_CONS)

        @ L,01 Say CODIGO            Picture "@S7"
        @ L,46 Say QTDE              Picture "@Z 99999.99" 
        @ L,55 Say UNITARIO          Picture "@Z 999,999.9999" 
        @ L,68 Say (QTDE * UNITARIO) Picture "@R@EZ 9999,999.99"

        WWCOD = CODIGO

        Select PRODUTOS
        ordsetfocus('SINC001')
        Seek WWCOD

        @ L,10 Say Left(PRODUTO,32)
        @ L,43 Say UN Picture "@S2"

        I++
        L++

        If I > 14
           Exit
        Endif

        Select ITENS_S
        Skip

    Enddo

    Setcolor(COR_CONS)

    If CONFIRMA(24,"Voc"+Chr(136)+" deseja excluir este documento...(S/N)?","N") = "S"                                                               

       Select OUTRAS_S

       If ! REGLOCK() 
          Loop
       Endif

       Replace SEQ      With 0              ,;
               DOCNO    With Space(10)      ,;
               SAIDA    With Ctod(Space(08)),;
               HISTORIC With Space(54)      ,;
               CCUSTO   With Space(01)
       Delete

       GRAVA()

       // deleta todas as saidas   

       Select ITENS_S
       ordsetfocus('SINC001')
       Seek WSEQ

       While SEQ == WSEQ .And. ! Eof()

           If ! REGLOCK()
              Skip
              Loop
           Endif             

           WWCODIGO = CODIGO
           WWQTDE   = QTDE

           Replace SEQ      With 0        ,;
                   CODIGO   With Space(08),; 
                   QTDE     With 0        ,;
                   UNITARIO With 0        
    
           Delete

           GRAVA()

           // retira as baixas de produtos do estoque 

           Select PRODUTOS
           ordsetfocus('SINC001')
           Seek WWCODIGO

           While .T.

               If ! REGLOCK()
                  Loop
               Endif

           Exit
           Enddo

           Replace BAIXAS With BAIXAS - WWQTDE

           If BAIXAS < 0
              Replace BAIXAS With 0
           Endif

           GRAVA()

           Select ITENS_S
           Seek WSEQ

       Enddo

    Endif

Enddo

Return .T.


*********************
Function rpr_MOVIMENT
*********************
WIN_M = 1

While .T.

    Setcolor(COR_CAB)   
    @ 03,42 Say "RELATORIO DE MOVIMENTA"+Chr(128)+Chr(142)+"O ESTOQUE  " 
    Setcolor(COR_TEXT)

    Private Opcao[3]

    Opcao[1] = " Entradas "
    Opcao[2] = " Saidas   "
    Opcao[3] = " Fim      "

    private ACESSO[3]
    ACESSO[1] = .T.
    ACESSO[2] = .T.
    ACESSO[3] = .T.

    Private Letra[3]

    Letra[1] = "E"  
    Letra[2] = "S"  
    Letra[3] = "F"  

    Private Pos[3]

    Pos[1] = 1
    Pos[2] = 1
    Pos[3] = 1 

    Private Obs[3]

    Obs[1] = "Emiss"+Chr(132)+"o da rela"+Chr(135)+Chr(132)+"o de entradas!" 
    Obs[2] = "Emiss"+Chr(132)+"o da rela"+Chr(135)+Chr(132)+"o de saidas!"
    Obs[3] = "Retorno ao menu principal!"
 
    WIN_M = ACHOISE(WIN_M,10,35,14,46,2,OPCAO,ACESSO,LETRA,POS,OBS,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)

    If WIN_M = 0                     
       Return .T.
    Elseif WIN_M = 1  
       rpr_ENTRADAS()                         
    Elseif WIN_M = 2  
       rpr_SAIDAS()                           
    Elseif WIN_M = 3  
       Return .T.     
    Endif             

Enddo

Return .T.


*********************
Function rpr_ENTRADAS
*********************
Setcolor(COR_CAB)   
@ 03,42 Say "RELATORIO DE ENTRADAS (COMPRAS)    " 
Setcolor(COR_TEXT)

SET CENTURY OFF

While .T.

    iENTRADA = DATE()
    fENTRADA = DATE()             
    WGRUPO   = "999"
    CONT     = 60
    tTOTAL   = 0

    Setcolor(COR_TEXT)
    @ 05,00 Clear To 22,79
    @ 24,00 Clear To 24,79

    Setcolor(COR_CONS)
    @ 24,72 Say " ESC-FIM"

    SOMBRA(09,16,17,63,8)

    Setcolor("N/W,W+/R,,,N/W")
    @ 08,14 Clear To 16,62
    @ 08,14 To 16,62

    @ 09,16 Say "Data da entrada inicial........:"
    @ 11,16 Say "Data da entrada final..........:"
    @ 13,16 Say "Grupo..........................:"
    @ 15,16 Say "Saida para a impressora...(S/N)?"

    @ 09,49 Get iENTRADA                  
    @ 11,49 Get fENTRADA              Valid fENTRADA >= iENTRADA
    @ 13,49 Get WGRUPO  Picture "999" Valid win_GRP()
    READ_F()

    If Empty(iENTRADA) .Or. Empty(fENTRADA) .Or. Lastkey() = 27
       Return .T.
    Endif   

    PRN = REDUCAO(15,16,"Saida para a impressora...(S/N)?","N") 

    Setcolor(COR_TEXT)
    If CONFIRMA(24,"Confirma...(S/N)?","S") = "N"
       Loop
    Endif
    
    If PRN == "S"
       If ! chck_PRN()
          Loop
       Endif
    Endif

    If PRN == "N"
       WARQRPR = Alltrim(Str(RANDOM()%99999999,8))
       @ 24,01 Say "[ "+Right(Replicate(chr(177),8)+WARQRPR,8)+" ]"
       WARQRPR = "RELAT\"+WARQRPR+".TXT"
    Endif
      
    Setcolor(COR_TEXT+"*")
    @ 24,70 Say "Aguarde..!"
    Setcolor(COR_TEXT)

    Select 0
    Use TEMP\MOV_ENT.TMP Alias MOVIMENT EXCLUSIVE

    if NETERR() 
       ERROR("  O arquivo tempor"+Chr(160)+"rio est"+Chr(160)+" sendo usado por outro terminal!  ",0)
       loop
    endif
    index on CODIGO        tag SINC001
    Index On GRUPO+PRODUTO tag SINC002
    ZAP

    Setcolor(COR_TEXT+"*")
    @ 24,70 Say "Aguarde..!"
    Setcolor(COR_TEXT)

    Select OUTRAS_E
    ordsetfocus('SINC003')
    Set Softseek On
    Seek iENTRADA
    Set Softseek Off

    While ENTRADA <= fENTRADA .And. ! Eof()

        WSEQ = SEQ

        Select ITENS_E
        ordsetfocus('SINC001')
        Seek WSEQ

        While SEQ == WSEQ .And. ! Eof()

            WCODIGO = CODIGO
            WQTDE   = QTDE
            WTOTAL  = (QTDE * UNITARIO)

            Select PRODUTOS
            ordsetfocus('SINC001')
            Seek WCODIGO

            WPRODUTO = PRODUTO
            WUN      = UN
            WCOD_GRP = COD_GRP
            WNOM_GRP = "N/C"

            If WGRUPO <> "999"
              If WCOD_GRP <> WGRUPO
                 Select ITENS_E
                 Skip
                 Loop
              Endif
            Endif
           
            Select GRUPOS
            ordsetfocus('SINC001')
            Seek WCOD_GRP

            If Found()
               WNOM_GRP = GRUPO
            Endif

            Select MOVIMENT
            ordsetfocus('SINC001')
            seek WCODIGO

            If Found()

               Replace QTDE  With QTDE  + WQTDE,;
                       TOTAL With TOTAL + WTOTAL

            Else

               DBAPPEND()

               Replace CODIGO  With WCODIGO ,;
                       PRODUTO With WPRODUTO,;
                       UN      With WUN     ,;
                       QTDE    With WQTDE   ,;
                       TOTAL   With WTOTAL  ,;
                       GRUPO   With WNOM_GRP

            Endif

            Select ITENS_E
            Skip

        Enddo

        Select OUTRAS_E
        Skip

    Enddo

    PAG    = 0
    tSUB   = 0
    tTOTAL = 0

    SET CENTURY OFF

    If PRN == "S"
       Set Print On
    Else
       Set Print To &WARQRPR   
    Endif

    Set Device To Print
    Set Console Off

    If PRN == "S"
       @ Prow(),Pcol() Say Chr(15)
    Endif

    SETPRC(0,0)

    GRP = .T.
    CAB = .T.

    Select MOVIMENT
    ordsetfocus('SINC002')
    Go Top 

    While ! Eof()

        If CAB

           PAG++            

           If PRN = "S"
              @ Prow(),Pcol() Say Chr(18)         
           Endif

           @ Prow(),00  Say "SERIAL SOFTWARE"
           CENTRO_PRN(80,Alltrim(CFG_EMPRES))
           @ Prow(),67  Say "Pag. "+Str(PAG,5)
           @ Prow()+1,0 Say ""

           If PAG == 1
              CENTRO_PRN(80,"RELACAO DE ENTRADAS (Compras) - ["+Alltrim(CRI)+"]")
           Else
              CENTRO_PRN(80,"RELACAO DE ENTRADAS (Compras)")
           Endif

           @ Prow()+1,0 Say ""
           CENTRO_PRN(80,"Periodo: "+Dtoc(iENTRADA)+" a "+Dtoc(fENTRADA))

           @ Prow()+1,0 Say Replicate("=",80)
           @ Prow()+1,0 Say "CODIGO  PRODUTO                                   UN       QTDE          TOTAL"
           @ Prow()+1,0 Say ""

           CAB = .F.

        Endif

        If GRP
           @ Prow()+1,0 Say ""
           CENTRO_PRN(80,"*** "+Alltrim(GRUPO)+" ***")
           @ Prow()+1,0 Say ""
           GRP = .F.
        Endif

        @ Prow()+1,0 Say CODIGO  
        @ Prow(),08  Say PRODUTO     
        @ Prow(),50  Say UN       
        @ Prow(),55  Say QTDE    Picture "99999.99"
        @ Prow(),64  Say TOTAL   Picture "@R@E 999,999,999.99"

        tSUB   = tSUB   + TOTAL
        tTOTAL = tTOTAL + TOTAL
        WGROUP = GRUPO

        Skip

        If GRUPO <> WGROUP .Or. EOF()
           @ Prow()+1,64 Say Replicate("-",14)
           @ Prow()+1,64 Say tSUB Picture "@R@E 999,999,999.99"
           tSUB = 0
           GRP = .T.
        Endif

        If Prow() > 58
           If PRN = "S"
              EJECT
           Else
              @ Prow()+10,0 Say ""
           Endif
           CAB = .T.
           GRP = .T.
           SETPRC(0,0)
        Endif

    Enddo
 
    If tTOTAL <> 0 
       @ Prow()+1,0 Say Replicate("-",80)
       @ Prow()+1,0 Say "T O T A L :"
       @ Prow(),64  Say tTOTAL Picture "@R@E 999,999,999.99"
    Endif

    If PRN == "S"
   
       EJECT
    
       Set Print Off
       Set Device To Screen
       Set Console On
    Else
       @ Prow()+8,00 Say ""
       Setprc(0,0)

       Set Print To 
       Set Device To Screen
       Set Console On
    
       browse_REL(WARQRPR,00,00,22,79,COR_TEXT,.F.,.T.)
    Endif

    Select MOVIMENT
    USE

    Select OUTRAS_E

Enddo

Return .T.


****************
Function win_GRP
****************
WL = ROW()

If Lastkey() = 27
   Return .T.
Endif

If WGRUPO = "999"
   Setcolor("N/W")
   @ WL,52 Say "-GERAL   "
   Return .T.
Endif

If Empty(WGRUPO)
   help_GRP() 
Endif

Select GRUPOS
ordsetfocus('SINC001')
Seek WGRUPO

If ! Found()
   ERROR("  C"+Chr(162)+"digo de grupo n"+Chr(132)+"o cadastrado!  ",0)
   Return .F.
Endif

Setcolor("N/W")
@ WL,52 Say "-"+Left(GRUPO,08)

Return .T.


***************
Function win_CC
***************

If Lastkey() = 27
   Return .T.
Endif

Setcolor("N/W")

If WCCUSTO = "B"
   @ 14,49 Say "B-Bovino"                              
Elseif WCCUSTO = "E"
   @ 14,49 Say "E-Equino"                              
Elseif WCCUSTO = "9"
   @ 14,49 Say "9-Geral "                              
Else
   Return .F.
Endif

Return .T.


*******************
Function rpr_SAIDAS
*******************
SET CENTURY OFF
                      
Setcolor(COR_CAB)   
@ 03,42 Say "RELATORIO DE SAIDAS (REQUISI"+Chr(128)+Chr(153)+"ES)  " 
Setcolor(COR_TEXT)

While .T.

    iSAIDA  = DATE()
    fSAIDA  = DATE()             
    WGRUPO  = "999"
    WCCUSTO = "9"
    CONT    = 60

    Setcolor(COR_TEXT)
    @ 05,00 Clear To 22,79
    @ 24,00 Clear To 24,79

    Setcolor(COR_CONS)
    @ 24,72 Say " ESC-FIM"

    SOMBRA(08,16,18,63,8)

    Setcolor("N/W,W+/R,,,N/W")
    @ 07,14 Clear To 17,62
    @ 07,14 To 17,62

    @ 08,16 Say "Data da saida inicial..........:"
    @ 10,16 Say "Data da saida final............:"
    @ 12,16 Say "Grupo..........................:"
    @ 14,16 Say "Centro de Custo.........(B/E/9)?"
    @ 16,16 Say "Saida para a impressora...(S/N)?"

    @ 08,49 Get iSAIDA                    
    @ 10,49 Get fSAIDA                Valid fSAIDA >= iSAIDA
    @ 12,49 Get WGRUPO  Picture "999" Valid win_GRP()
    @ 14,49 Get WCCUSTO Picture "@!"  Valid win_CC()
    READ_F()

    If Empty(iSAIDA) .Or. Empty(fSAIDA) .Or. Lastkey() = 27
       Return .T.
    Endif   

    PRN = REDUCAO(16,16,"Saida para a impressora...(S/N)?","N") 

    Setcolor(COR_TEXT)
    If CONFIRMA(24,"Confirma...(S/N)?","S") = "N"
       Loop
    Endif
    
    If PRN == "S"
       If ! chck_PRN()
          Loop
       Endif
    Endif

    If PRN == "N"
       WARQRPR = Alltrim(Str(RANDOM()%99999999,8))
       @ 24,01 Say "[ "+Right(Replicate(chr(177),8)+WARQRPR,8)+" ]"
       WARQRPR = "RELAT\"+WARQRPR+".TXT"
    Endif
      
    Setcolor(COR_TEXT+"*")
    @ 24,70 Say "Aguarde..!"
    Setcolor(COR_TEXT)

    Select 0
    Use TEMP\MOV_SAI.TMP Alias MOVIMENT EXCLUSIVE

    If NETERR() 
       ERROR("  O arquivo tempor"+Chr(160)+"rio est"+Chr(160)+" sendo usado por outro terminal!  ",0)
       Loop
    Endif

    index on CODIGO        tag SINC001
    Index On GRUPO+PRODUTO tag SINC002
    ZAP

    Setcolor(COR_TEXT+"*")
    @ 24,70 Say "Aguarde..!"
    Setcolor(COR_TEXT)

    tBOVINO = 0
    tEQUINO = 0

    Select OUTRAS_S
    ordsetfocus('SINC003')
    Set Softseek On
    Seek iSAIDA
    Set Softseek Off

    While SAIDA <= fSAIDA .And. ! Eof()

        If WCCUSTO <> "9"
           If CCUSTO <> WCCUSTO
              Skip
              Loop
           Endif
        Endif

        WSEQ = SEQ
        WCC  = CCUSTO

        Select ITENS_S
        ordsetfocus('SINC001')
        Seek WSEQ

        While SEQ == WSEQ .And. ! Eof()

            WCODIGO = CODIGO
            WQTDE   = QTDE
            WTOTAL  = (QTDE * UNITARIO)

            Select PRODUTOS
            ordsetfocus('SINC001')
            Seek WCODIGO

            WPRODUTO = PRODUTO
            WUN      = UN
            WCOD_GRP = COD_GRP
            WNOM_GRP = "N/C"

            If WGRUPO <> "999"
              If WCOD_GRP <> WGRUPO
                 Select ITENS_E
                 Skip
                 Loop
              Endif
            Endif
           
            Select GRUPOS
            ordsetfocus('SINC001')
            Seek WCOD_GRP

            If Found()
               WNOM_GRP = GRUPO
            Endif

            If WCC = "B"
               tBOVINO = tBOVINO + WTOTAL
            Elseif WCC = "E"
               tEQUINO = tEQUINO + WTOTAL
            Endif

            Select MOVIMENT
            ordsetfocus('SINC001')
            Seek WCODIGO

            If Found()

               Replace QTDE  With QTDE  + WQTDE,;
                       TOTAL With TOTAL + WTOTAL

            Else

               DBAPPEND()

               Replace CODIGO  With WCODIGO ,;
                       PRODUTO With WPRODUTO,;
                       UN      With WUN     ,;
                       QTDE    With WQTDE   ,;
                       TOTAL   With WTOTAL  ,;
                       GRUPO   With WNOM_GRP

            Endif

            Select ITENS_S
            Skip

        Enddo

        Select OUTRAS_S
        Skip

    Enddo

    CAB    = .T.
    PAG    = 0
    tSUB   = 0
    tTOTAL = 0

    SET CENTURY OFF

    If PRN == "S"
       Set Print On
    Else
       Set Print To &WARQRPR   
    Endif

    Set Device To Print
    Set Console Off

    If PRN == "S"
       @ Prow(),Pcol() Say Chr(15)
    Endif

    SETPRC(0,00)
              
    GRP = .T.

    select MOVIMENT
    ordsetfocus('SINC002')
    go top 

    While ! Eof()

        If CAB

           PAG++            

           If PRN = "S"
              @ Prow(),Pcol() Say Chr(18)         
           Endif

           @ Prow(),00  Say "SERIAL SOFTWARE"
           CENTRO_PRN(80,Alltrim(CFG_EMPRES))
           @ Prow(),67  Say "Pag. "+Str(PAG,5)
           @ Prow()+1,0 Say ""

           If PAG == 1
              CENTRO_PRN(80,"RELACAO DE SAIDAS (Requisicoes) - ["+Alltrim(CRI)+"]")
           Else
              CENTRO_PRN(80,"RELACAO DE SAIDAS (Requisicoes)")
           Endif

           @ Prow()+1,0 Say ""
           CENTRO_PRN(80,"Periodo: "+Dtoc(iSAIDA)+" a "+Dtoc(fSAIDA))

           @ Prow(),67  Say "Data "+Dtoc(DATE())

           @ Prow()+1,0 Say Replicate("=",80)
           @ Prow()+1,0 Say "CODIGO  PRODUTO/SERVICO                           UN       QTDE          TOTAL"
           @ Prow()+1,0 Say ""

           CAB = .F.

        Endif

        If GRP
           @ Prow()+1,0 Say ""
           CENTRO_PRN(80,"*** "+Alltrim(GRUPO)+" ***")
           @ Prow()+1,0 Say ""
           GRP = .F.
        Endif

        @ Prow()+1,0 Say CODIGO  
        @ Prow(),08  Say PRODUTO     
        @ Prow(),50  Say UN       
        @ Prow(),55  Say QTDE    Picture "99999.99"
        @ Prow(),64  Say TOTAL   Picture "@R@E 999,999,999.99"

        tSUB   = tSUB   + TOTAL
        tTOTAL = tTOTAL + TOTAL
        WGROUP = GRUPO

        Skip

        If GRUPO <> WGROUP .Or. EOF()
           @ Prow()+1,64 Say Replicate("-",14)
           @ Prow()+1,64 Say tSUB Picture "@R@E 999,999,999.99"
           tSUB = 0
           GRP = .T.
        Endif

        If Prow() > 58
           If PRN = "S"
              EJECT
           Else
              @ Prow()+8,0 Say ""
           Endif
           CAB = .T.
           GRP = .T.
           SETPRC(0,0)
        Endif

    Enddo
 
    If tTOTAL <> 0 

       @ Prow()+1,0 Say Replicate("-",80)
       @ Prow()+1,0 Say "T O T A L :"
       @ Prow(),64  Say tTOTAL Picture "@R@E 999,999,999.99"

       @ Prow()+2,25 Say "         R e s u m o          "
       @ Prow()+1,25 Say "=============================="
       @ Prow()+1,25 Say "Bovino.......R$ "+Transform(tBOVINO,"@R@E 999,999,999.99")
       @ Prow()+1,25 Say "Equino.......R$ "+Transform(tEQUINO,"@R@E 999,999,999.99")
       @ Prow()+1,25 Say "=============================="

    Endif

    If PRN == "S"
   
       EJECT
    
       Set Print Off
       Set Device To Screen
       Set Console On
    Else
       @ Prow()+8,00 Say ""
       Setprc(0,0)

       Set Print To 
       Set Device To Screen
       Set Console On
    
       browse_REL(WARQRPR,00,00,22,79,COR_TEXT,.F.,.T.)
    Endif

    Select MOVIMENT
    USE

    Select OUTRAS_S

Enddo

Return .T.

