****************
Function rpr_DIV
****************
#INCLUDE "INKEY.CH"

OPC2 = 1

While .T.

   Setcolor(COR_CAB) 
   @ 03,42 say "RELATORIOS DIVERSOS                " 
   Setcolor(COR_TEXT)

   private OPCAO[10]
   OPCAO[1]  = " Animais                        " 
   OPCAO[2]  = " CIO                            " 
   OPCAO[3]  = " Cobertura/Insemina"+Chr(135)+Chr(132)+"o/TE       " 
   OPCAO[4]  = " Prenhez                        " 
   OPCAO[5]  = " Aborto                         " 
   OPCAO[6]  = " Parto/Desmame                  " 
   OPCAO[7]  = " Sintomas/Tratamentos           " 
   OPCAO[8]  = " Peso/Condi"+Chr(135)+Chr(132)+"o Corp"+Chr(162)+"rea         " 
   OPCAO[9]  = " Vacina"+Chr(135)+Chr(148)+"es                     " 
   OPCAO[10] = " Esc - Fim                      " 

   private ACESSO[10]
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

   private LETRA[10]
   LETRA[1]   = "A"  
   LETRA[2]   = "I"
   LETRA[3]   = "C"
   LETRA[4]   = "P"
   LETRA[5]   = "B"
   LETRA[6]   = "O"
   LETRA[7]   = "S"
   LETRA[8]   = "E"
   LETRA[9]   = "V"
   LETRA[10]  = "F"

   private POS[10] 
   POS[1]  = 1 
   POS[2]  = 2
   POS[3]  = 1
   POS[4]  = 1
   POS[5]  = 2
   POS[6]  = 5
   POS[7]  = 1
   POS[8]  = 2 
   POS[9]  = 1
   POS[10] = 7

   private OBS_MENU[10] 
   OBS_MENU[1]  = "Relat"+Chr(162)+"rio rela"+Chr(135)+Chr(132)+"o de animais!" 
   OBS_MENU[2]  = "Relat"+Chr(162)+"rio de animais no CIO!" 
   OBS_MENU[3]  = "Relat"+Chr(162)+"rio de cobertura/insemina"+Chr(135)+Chr(132)+"o dos animais!"
   OBS_MENU[4]  = "Relat"+Chr(162)+"rio de prenhez dos animais!"
   OBS_MENU[5]  = "Relat"+Chr(162)+"rio de aborto dos animais!"
   OBS_MENU[6]  = "Relat"+Chr(162)+"rio de parto/desmame dos animais!"
   OBS_MENU[7]  = "Relat"+Chr(162)+"rio de sintomas/tratamentos dos animais!"
   OBS_MENU[8]  = "Relat"+Chr(162)+"rio de peso/condi"+Chr(135)+Chr(132)+"o corporea dos animais!"
   OBS_MENU[9]  = "Relat"+Chr(162)+"rio de vacina"+Chr(135)+Chr(148)+"es dos animais!" 
   OBS_MENU[10] = "Retorno ao menu principal!"                                                                                            

   OPC2 = achoise(OPC2,07,22,18,55,2,OPCAO,ACESSO,LETRA,POS,OBS_MENU,COR_MENU,COR_OPC,COR_DEST,COR_TEXT)
   
   If OPC2 == 0 .Or. OPC2 == 10
      Return .T.
   Elseif OPC2 == 1
      rpr_ANIMAIS()
   Elseif OPC2 == 2
      rpr_CIO()
   Elseif OPC2 == 3
      rpr_REP()
   Elseif OPC2 == 4
      rpr_PRENHEZ()
   Elseif OPC2 == 5
      rpr_ABORTO()
   Elseif OPC2 == 6
      rpr_PARTOS()
   Elseif OPC2 == 7
      rpr_SINT_TRAT()
   Elseif OPC2 == 8
      rpr_PESO()
   Elseif OPC2 == 9
      rpr_VACINAS()
   Endif

Enddo

Return .T.


********************
Function rpr_ANIMAIS
********************

Setcolor(COR_CAB)
@ 03,42 Say "RELA"+Chr(128)+Chr(142)+"O DE ANIMAIS                 "
Setcolor(COR_TEXT)

While .T.

    @ 05,00 Clear To 22,79
    @ 24,00 Clear To 24,79

    WGRUPO  = "999" 
    WNGRUPO = ""
    WLOTE   = "999"         
    WNLOTE  = ""
    WPASTO  = Replicate("9",10)
    WSEXO   = "9"

    @ 07,01 Say "Grupo........................:    -"
    @ 09,01 Say "Lote.........................:    -"
    @ 11,01 Say "Pasto........................:           -"
    @ 13,01 Say "Sexo.........................:  -"

    Setcolor("BG/B")
    @ 14,01 Say "(M-Macho  F-Femea  9-GERAL)"

    Setcolor(COR_TEXT)
    @ 16,01 say "Saida para impressora...(S/N)?"

    @ 07,32 get WGRUPO  Picture "@!"  valid w_GRUPO(07,36)
    @ 09,32 get WLOTE   Picture "@!"  valid w_LOTE(09,36)
    @ 11,32 get WPASTO  Picture "@!"  Valid w_PASTO(11,43)
    @ 13,32 get WSEXO   Picture "@!"  Valid w_SEXO(13,34)
    READ_F()

    If Lastkey() == 27
       @ 05,00 Clear To 22,79
       @ 24,00 Clear To 24,79
       Return .T.
    Endif

    PRN = REDUCAO(16,01,"Saida para impressora...(S/N)?","N") 

    If CONFIRMA(24,"Confirma...(S/N)?","S") == "N"
       Loop
    Endif

    If PRN == "S"
       If ! chck_PRN()
          Loop
       Endif
    Endif

    If PRN == "N"
       WARQRPR = Alltrim(Str(RANDOM()%99999999,8))
       @ 17,40 Say "[ "+Right(Replicate(chr(177),8)+WARQRPR,8)+" ]"
       WARQRPR = "RELAT\"+WARQRPR+".TXT"
    Endif
      
    Setcolor(COR_TEXT+"*")
    @ 24,70 Say "Aguarde..!"
    Setcolor(COR_TEXT)

    CAB = .T.
    PAG = 0
    TNA = 0

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

    Select ANIMAL
    ordsetfocus('SINC002')
    Go Top

    While ! EOF()

        If ! Empty(SAIDA)
           Skip    
           Loop
        Endif

        If WSEXO <> "9"   
           If SEXO <> WSEXO 
              Skip
              Loop
           Endif
        Endif

        If WGRUPO <> "999"
           If GRUPO <> WGRUPO
              Skip  
              Loop
           Endif
        Endif

        If WLOTE <> "999"
           If LOTE <> WLOTE
              Skip
              Loop
           Endif
        Endif

        If WPASTO <> Replicate("9",10)
           If WPASTO <> PASTO
              Skip    
              Loop
           Endif
        Endif

        If Prow() > 58 .And. ! CAB .And. ! EOF()
  
           If PRN = "S"
              Eject
           Else
              @ Prow()+8,00 Say ""
              Setprc(0,0)
           Endif

           CAB = .T.
        Endif

        If CAB

           PAG++            
           CAB = .F.

           @ Prow(),00 Say "SERIAL SOFTWARE"
           CENTRO_PRN(105,Alltrim(CFG_EMPRES))
           @ Prow(),92  Say "Pag. "+Str(PAG,5)
           @ Prow()+1,0 Say ""

           If PAG == 1
              CENTRO_PRN(105,"RELACAO DE ANIMAIS - ["+Alltrim(CRI)+"]")
           Else
              CENTRO_PRN(105,"RELACAO DE ANIMAIS")
           Endif
           @ Prow(),92  Say "Data "+Dtoc(DATE())

           @ Prow()+1,0 Say "Lote: "+Alltrim(WNLOTE)

           CENTRO_PRN(105,"Grupo: "+Alltrim(WNGRUPO))

           @ Prow(),92  Say "Pasto: "+If(WPASTO = Replicate("9",10),"GERAL",Alltrim(WPASTO))

           @ Prow()+1,0 Say Replicate(Chr(205),105)
           @ Prow()+1,0 Say "CODIGO NOME                           SEXO  NASC     CATEG REGISTRO        PAI/MAE"  
           @ Prow()+1,0 Say Replicate(Chr(205),105)

        Endif

        @ Prow()+1,0  Say CODIGO
        @ Prow(),07   Say NOME    
        @ Prow(),38   Say If(SEXO = "F","Femea","Macho")
        @ Prow(),44   Say DTNASC
        @ Prow(),53   Say CATEGORIA
        @ Prow(),59   Say Left(NREG,15)
        @ Prow(),75   Say PAI  
        @ Prow()+1,07 Say Upper(Alltrim(RACA))+", "+Upper(Alltrim(COR))
        @ Prow(),75   Say MAE  
        @ Prow()+1,0  Say Replicate(Chr(196),105)

        TNA++

        Skip 

    Enddo

    @ Prow()+1,0 Say Strzero(TNA,4)+" animais relacionados."
  
    If PRN == "S"
       @ Prow(),Pcol() Say Chr(18)         
   
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

    SET CENTURY ON

Enddo

Return .T.


***********************
Function w_GRUPO(LL,CC)
***********************

If Lastkey() = 27
   Return .T.
Endif

If WGRUPO = "999"
   @ LL,CC Say "G e r a l"
   WNGRUPO = "GERAL"
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

@ LL,CC Say DESCRICAO

WNGRUPO = DESCRICAO

Return .T.


**********************
Function w_LOTE(LL,CC)
**********************

If Lastkey() = 27
   Return .T.
Endif

If WLOTE = "999"
   @ LL,CC Say "G e r a l"
   WNLOTE = "GERAL"
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

@ LL,CC Say DESCRICAO

WNLOTE = DESCRICAO

Return .T.


***********************
Function w_PASTO(LL,CC)
***********************

If Lastkey() = 27
   Return .T.
Endif

If WPASTO = Replicate("9",10)
   @ LL,CC Say "G e r a l"
   Return .T.
Endif

If Empty(WPASTO)
   help_PASTO()
   Return .F.
Endif

Select PASTO
ordsetfocus('SINC001')
Seek WPASTO

If ! Found()
   ERROR("N"+Chr(163)+"mero de pasto n"+Chr(132)+"o cadastrado!",0)
   Return .F.
Endif

@ LL,CC Say DESCRICAO

Return .T.


**********************
Function w_SEXO(LL,CC)
**********************

If Lastkey() = 27
   Return .T.
Endif

If WSEXO = "F"
   @ LL,CC Say "Femea"
Elseif WSEXO = "M"
   @ LL,CC Say "Macho"
Elseif WSEXO = "9"
   @ LL,CC Say "G e r a l"
Else
   ERROR("Op"+Chr(135)+Chr(148)+"es validas: F-Femea  M-Macho  9-Geral",0)
   Keyboard Chr(Lastkey())
   Return .F.
Endif

Return .T.


****************
Function rpr_CIO
****************

Setcolor(COR_CAB)
@ 03,42 Say "RELA"+Chr(128)+Chr(142)+"O DE ANIMAIS NO CIO          "
Setcolor(COR_TEXT)

While .T.

    @ 05,00 Clear To 22,79
    @ 24,00 Clear To 24,79

    WCODIGO = "999999" 
    WDATA1  = DATE()
    WDATA2  = DATE()        

    @ 07,01 Say "Animal...........................:"
    @ 09,01 Say "Data de entrada no cio inicial...:"
    @ 11,01 Say "Data de entrada no cio final.....:"

    Setcolor(COR_TEXT)
    @ 13,01 say "Saida para impressora.......(S/N)?"

    @ 07,36 Get WCODIGO Picture "999999" Valid view_ANIMAL(07,43,"F")
    @ 09,36 get WDATA1  Picture "@!"                         
    @ 11,36 get WDATA2  Picture "@!"     Valid WDATA2 >= WDATA1
    READ_F()

    If Lastkey() == 27
       @ 05,00 Clear To 22,79
       @ 24,00 Clear To 24,79
       Return .T.
    Endif

    PRN = REDUCAO(13,01,"Saida para impressora.......(S/N)?","N") 

    If CONFIRMA(24,"Confirma...(S/N)?","S") == "N"
       Loop
    Endif

    If PRN == "S"
       If ! chck_PRN()
          Loop
       Endif
    Endif

    If PRN == "N"
       WARQRPR = Alltrim(Str(RANDOM()%99999999,8))
       @ 13,44 Say "[ "+Right(Replicate(Chr(177),8)+WARQRPR,8)+" ]"
       WARQRPR = "RELAT\"+WARQRPR+".TXT"
    Endif
      
    Setcolor(COR_TEXT+"*")
    @ 24,70 Say "Aguarde..!"
    Setcolor(COR_TEXT)

    CAB = .T.
    PAG = 0
    TNA = 0

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

    WUNQ = .T.

    Select CIO
    ordsetfocus('SINC003')    
    Seek WDATA1

    While DATA <= WDATA2 .And. ! EOF()

        If WCODIGO <> "999999"
           If ANIMAL <> WCODIGO
              Skip
              Loop
           Endif
        Endif

        If Prow() > 58 .And. ! CAB .And. ! EOF()
  
           If PRN = "S"
              Eject
           Else
              @ Prow()+8,00 Say ""
              Setprc(0,0)
           Endif

           CAB = .T.
        Endif

        If CAB

           PAG++            
           CAB = .F.

           @ Prow(),00  Say "SERIAL SOFTWARE"
           CENTRO_PRN(85,Alltrim(CFG_EMPRES))
           @ Prow(),72  Say "Pag. "+Str(PAG,5)
           @ Prow()+1,0 Say ""

           If PAG == 1
              CENTRO_PRN(85,"RELACAO DE ANIMAIS NO CIO - ["+Alltrim(CRI)+"]")
           Else
              CENTRO_PRN(85,"RELACAO DE ANIMAIS NO CIO")
           Endif
           @ Prow(),72  Say "Data "+Dtoc(DATE())

           @ Prow()+1,0 Say Replicate(Chr(205),85)
           @ Prow()+1,0 Say "SEQ    DATA     NOME                           NASC     PAI/MAE"  
           @ Prow()+1,0 Say Replicate(Chr(205),85)

        Endif

        @ Prow()+1,0  Say Strzero(SEQ,6)
        @ Prow(),07   Say DATA

        WWCOD = ANIMAL

        If WUNQ

           Select ANIMAL
           ordsetfocus('SINC001')
           Seek WWCOD

           @ Prow(),16   Say NOME    
           @ Prow(),47   Say DTNASC
           @ Prow(),56   Say PAI  
           @ Prow()+1,16 Say Upper(Alltrim(RACA))+", "+Upper(Alltrim(COR))
           @ Prow(),56   Say MAE  
           @ Prow()+1,0  Say Replicate(Chr(196),85)
 
           If WCODIGO <> "999999"
              WUNQ = .F.
           Endif

        Endif

        Select CIO

        Skip 

        If ANIMAL <> WWCOD
           WUNQ = .T.
        Endif

    Enddo

    If PRN == "S"
       @ Prow(),Pcol() Say Chr(18)         
   
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

    SET CENTURY ON

Enddo

Return .T.


************************************
Function view_ANIMAL(WROW, WCOL,WSX)
************************************

WVAR = READVAR()

If &WVAR = "999999"
   Return .T.
Endif

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

@ WROW,WCOL say NOME

If WSX = "F" 

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
  
Select &AREA

Return .T.


****************
Function rpr_REP
****************

Setcolor(COR_CAB)
@ 03,42 Say "RELA"+Chr(128)+Chr(142)+"O SERVI"+Chr(128)+"O DE REPRODU"+Chr(128)+Chr(142)+"O      "
Setcolor(COR_TEXT)

While .T.

    @ 05,00 Clear To 22,79
    @ 24,00 Clear To 24,79

    WCODIGO = "999999" 
    WDATA1  = DATE()
    WDATA2  = DATE()        
    WTIPO   = "9"

    @ 07,01 Say "Animal...........................:"
    @ 09,01 Say "Data servi"+Chr(135)+"o inicial.............:"
    @ 11,01 Say "Data servi"+Chr(135)+"o final...............:"
    @ 13,01 Say "Tipo.............................:"

    Setcolor("BG/B")
    @ 14,01 Say "(C-Cobertura I-Inserminacao T-Transplante ou 9-Geral)"

    Setcolor(COR_TEXT)
    @ 16,01 say "Saida para impressora.......(S/N)?"

    @ 07,36 Get WCODIGO Picture "999999" Valid view_ANIMAL(07,51,"F")
    @ 09,36 Get WDATA1  Picture "@!"                         
    @ 11,36 Get WDATA2  Picture "@!"     Valid WDATA2 >= WDATA1
    @ 13,36 Get WTIPO   Picture "@!"     Valid WTIPO $ "CIT9"
    READ_F()

    If Lastkey() == 27
       @ 05,00 Clear To 22,79
       @ 24,00 Clear To 24,79
       Return .T.
    Endif

    PRN = REDUCAO(16,01,"Saida para impressora.......(S/N)?","N") 

    If CONFIRMA(24,"Confirma...(S/N)?","S") == "N"
       Loop
    Endif

    If PRN == "S"
       If ! chck_PRN()
          Loop
       Endif
    Endif

    If PRN == "N"
       WARQRPR = Alltrim(Str(RANDOM()%99999999,8))
       @ 16,44 Say "[ "+Right(Replicate(Chr(177),8)+WARQRPR,8)+" ]"
       WARQRPR = "RELAT\"+WARQRPR+".TXT"
    Endif
      
    Setcolor(COR_TEXT+"*")
    @ 24,70 Say "Aguarde..!"
    Setcolor(COR_TEXT)

    CAB = .T.
    PAG = 0
    TNA = 0

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

    WUNQ = .T.

    Select SERVICO
    ordsetfocus('SINC006')   
    Seek WDATA1

    While DATA <= WDATA2 .And. ! EOF()

        If WCODIGO <> "999999"
           If ANIMAL <> WCODIGO
              Skip
              Loop
           Endif
        Endif

        If WTIPO <> "9"
           If TIPO <> WTIPO
              Skip
              Loop
           Endif
        Endif

        If Prow() > 58 .And. ! CAB .And. ! EOF()
  
           If PRN = "S"
              Eject
           Else
              @ Prow()+8,00 Say ""
              Setprc(0,0)
           Endif

           CAB = .T.
        Endif

        If CAB

           PAG++            
           CAB = .F.

           @ Prow(),00  Say "SERIAL SOFTWARE"
           CENTRO_PRN(132,Alltrim(CFG_EMPRES))
           @ Prow(),119 Say "Pag. "+Str(PAG,5)
           @ Prow()+1,0 Say ""

           If PAG == 1
              CENTRO_PRN(132,"RELACAO DE SERVICO DE REPRODUCAO - ["+Alltrim(CRI)+"]")
           Else
              CENTRO_PRN(132,"RELACAO DE SERVICO DE REPRODUCAO")
           Endif
           @ Prow(),119 Say "Data "+Dtoc(DATE())

           @ Prow()+1,0 Say Replicate(Chr(205),132)
           @ Prow()+1,0 Say "SEQ    DATA     NOME                           NASC     PAI/MAE                        REPRODUCAO"    
           @ Prow()+1,0 Say Replicate(Chr(205),132)

        Endif

        @ Prow()+1,0  Say Strzero(SEQ,6)
        @ Prow(),07   Say DATA

        WWCOD = ANIMAL
        WTXT  = ""

        If TIPO = "C"

           WTXT   = "Cobertura    > Touro: "
           WTOURO = TOURO

           Select ANIMAL
           ordsetfocus('SINC001')
           Seek WTOURO
 
           If Found()
              WTXT = WTXT+Alltrim(NOME)
           Else
              WTXT = WTXT+"N/C"
           Endif

           Select SERVICO

           WTXT = WTXT + " / Comunicacao: "+Dtoc(COMUNICA)+" / Numero: "+Alltrim(NUMERO)

        Elseif TIPO = "I"
           WTXT = "Inserminacao > Lote: "+Alltrim(LOTE)+" / Dose: "+Alltrim(Str(DOSE,2))+" / Metodo: "+Alltrim(METODO)

        Elseif TIPO = "T"
           WTXT = "Transplante  > Touro: "+Alltrim(TOURO_TE)+" / Mae: "+Alltrim(MAE_TE)+" / Receptora: "+Alltrim(RECEP_TE)
        Endif

        Select ANIMAL
        ordsetfocus('SINC001')
        Seek WWCOD

        @ Prow(),16   Say NOME    
        @ Prow(),47   Say DTNASC
        @ Prow(),56   Say PAI  
        @ Prow(),87   Say Left(WTXT,45)
        @ Prow()+1,16 Say Upper(Alltrim(RACA))+", "+Upper(Alltrim(COR))
        @ Prow(),56   Say MAE  
        @ Prow(),87   Say Substr(WTXT,46,100)
        @ Prow()+1,0  Say Replicate(Chr(196),132)

        Select SERVICO
        Skip 

    Enddo

    If PRN == "S"
       @ Prow(),Pcol() Say Chr(18)         
   
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

    SET CENTURY ON

Enddo

Return .T.


********************
Function rpr_PRENHEZ
********************

Setcolor(COR_CAB)
@ 03,42 Say "RELA"+Chr(128)+Chr(142)+"O DE PRENHEZ                 "
Setcolor(COR_TEXT)

While .T.

    @ 05,00 Clear To 22,79
    @ 24,00 Clear To 24,79

    WCODIGO = "999999" 
    WDATA1  = DATE()
    WDATA2  = DATE()        

    @ 07,01 Say "Touro (Pai)......................:"
    @ 09,01 Say "Data da prenhez inicial..........:"
    @ 11,01 Say "Data da prenhez final............:"

    Setcolor(COR_TEXT)
    @ 13,01 say "Saida para impressora.......(S/N)?"

    @ 07,36 Get WCODIGO Picture "999999" Valid view_ANIMAL(07,43,"M")
    @ 09,36 get WDATA1  Picture "@!"                         
    @ 11,36 get WDATA2  Picture "@!"     Valid WDATA2 >= WDATA1
    READ_F()

    If Lastkey() == 27
       @ 05,00 Clear To 22,79
       @ 24,00 Clear To 24,79
       Return .T.
    Endif

    PRN = REDUCAO(13,01,"Saida para impressora.......(S/N)?","N") 

    If CONFIRMA(24,"Confirma...(S/N)?","S") == "N"
       Loop
    Endif

    If PRN == "S"
       If ! chck_PRN()
          Loop
       Endif
    Endif

    If PRN == "N"
       WARQRPR = Alltrim(Str(RANDOM()%99999999,8))
       @ 13,44 Say "[ "+Right(Replicate(Chr(177),8)+WARQRPR,8)+" ]"
       WARQRPR = "RELAT\"+WARQRPR+".TXT"
    Endif
      
    Setcolor(COR_TEXT+"*")
    @ 24,70 Say "Aguarde..!"
    Setcolor(COR_TEXT)

    CAB = .T.
    PAG = 0
    TNA = 0

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

    WUNQ = .T.

    Select EXAME
    ordsetfocus('SINC004')
    Seek WDATA1

    While DATA <= WDATA2 .And. ! EOF()

        If WCODIGO <> "999999"
           If TOURO <> WCODIGO
              Skip
              Loop
           Endif
        Endif

        If Prow() > 58 .And. ! CAB .And. ! EOF()
  
           If PRN = "S"
              Eject
           Else
              @ Prow()+8,00 Say ""
              Setprc(0,0)
           Endif

           CAB = .T.
        Endif

        If CAB

           PAG++            
           CAB = .F.

           @ Prow(),00  Say "SERIAL SOFTWARE"
           CENTRO_PRN(85,Alltrim(CFG_EMPRES))
           @ Prow(),72  Say "Pag. "+Str(PAG,5)
           @ Prow()+1,0 Say ""

           If PAG == 1
              CENTRO_PRN(85,"RELACAO DE PRENHEZ - ["+Alltrim(CRI)+"]")
           Else
              CENTRO_PRN(85,"RELACAO DE PRENHEZ")
           Endif
           @ Prow(),72  Say "Data "+Dtoc(DATE())

           @ Prow()+1,0 Say Replicate(Chr(205),85)
           @ Prow()+1,0 Say "SEQ    DATA     NOME                           NASC     TOURO"  
           @ Prow()+1,0 Say Replicate(Chr(205),85)

        Endif

        @ Prow()+1,0  Say Strzero(SEQ,6)
        @ Prow(),07   Say DATA

        WWCOD   = ANIMAL
        WWTOURO = TOURO

        Select ANIMAL
        ordsetfocus('SINC001')
        Seek WWCOD

        @ Prow(),16   Say NOME    
        @ Prow(),47   Say DTNASC

        Seek WWTOURO

        If Found()
           @ Prow(),56 Say NOME
        Else
           @ Prow(),56 Say "N/C"
        Endif

        @ Prow()+1,16 Say Upper(Alltrim(RACA))+", "+Upper(Alltrim(COR))
        @ Prow()+1,0  Say Replicate(Chr(196),85)
 
        Select EXAME

        Skip 

    Enddo

    If PRN == "S"
       @ Prow(),Pcol() Say Chr(18)         
   
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

    SET CENTURY ON

Enddo

Return .T.


*******************
Function rpr_ABORTO
*******************

Setcolor(COR_CAB)
@ 03,42 Say "RELA"+Chr(128)+Chr(142)+"O DE ABORTO                  "
Setcolor(COR_TEXT)

While .T.

    @ 05,00 Clear To 22,79
    @ 24,00 Clear To 24,79

    WCODIGO = "999999" 
    WDATA1  = DATE()
    WDATA2  = DATE()        

    @ 07,01 Say "Aninal...........................:"
    @ 09,01 Say "Data do aborto inicial...........:"
    @ 11,01 Say "Data do aborto final.............:"

    Setcolor(COR_TEXT)
    @ 13,01 say "Saida para impressora.......(S/N)?"

    @ 07,36 Get WCODIGO Picture "999999" Valid view_ANIMAL(07,43,"F")
    @ 09,36 get WDATA1  Picture "@!"                         
    @ 11,36 get WDATA2  Picture "@!"     Valid WDATA2 >= WDATA1
    READ_F()

    If Lastkey() == 27
       @ 05,00 Clear To 22,79
       @ 24,00 Clear To 24,79
       Return .T.
    Endif

    PRN = REDUCAO(13,01,"Saida para impressora.......(S/N)?","N") 

    If CONFIRMA(24,"Confirma...(S/N)?","S") == "N"
       Loop
    Endif

    If PRN == "S"
       If ! chck_PRN()
          Loop
       Endif
    Endif

    If PRN == "N"
       WARQRPR = Alltrim(Str(RANDOM()%99999999,8))
       @ 13,44 Say "[ "+Right(Replicate(Chr(177),8)+WARQRPR,8)+" ]"
       WARQRPR = "RELAT\"+WARQRPR+".TXT"
    Endif
      
    Setcolor(COR_TEXT+"*")
    @ 24,70 Say "Aguarde..!"
    Setcolor(COR_TEXT)

    CAB = .T.
    PAG = 0
    TNA = 0

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

    WUNQ = .T.

    Select ABORTO
    ordsetfocus('SINC003')
    Seek WDATA1

    While DATA <= WDATA2 .And. ! EOF()

        If WCODIGO <> "999999"
           If ANIMAL <> WCODIGO
              Skip
              Loop
           Endif
        Endif

        If Prow() > 58 .And. ! CAB .And. ! EOF()
  
           If PRN = "S"
              Eject
           Else
              @ Prow()+8,00 Say ""
              Setprc(0,0)
           Endif

           CAB = .T.
        Endif

        If CAB

           PAG++            
           CAB = .F.

           @ Prow(),00  Say "SERIAL SOFTWARE"
           CENTRO_PRN(85,Alltrim(CFG_EMPRES))
           @ Prow(),72  Say "Pag. "+Str(PAG,5)
           @ Prow()+1,0 Say ""

           If PAG == 1
              CENTRO_PRN(85,"RELACAO DE ABORTO - ["+Alltrim(CRI)+"]")
           Else
              CENTRO_PRN(85,"RELACAO DE ABORTO")
           Endif
           @ Prow(),72  Say "Data "+Dtoc(DATE())

           @ Prow()+1,0 Say Replicate(Chr(205),85)
           @ Prow()+1,0 Say "SEQ    DATA     NOME                           NASC     OBSERVACAO"
           @ Prow()+1,0 Say Replicate(Chr(205),85)

        Endif

        @ Prow()+1,0  Say Strzero(SEQ,6)
        @ Prow(),07   Say DATA

        WWCOD = ANIMAL
        WWOBS = OBS

        Select ANIMAL
        ordsetfocus('SINC001')
        Seek WWCOD

        @ Prow(),16   Say NOME    
        @ Prow(),47   Say DTNASC
        @ Prow(),56   Say Alltrim(WWOBS)

        @ Prow()+1,16 Say Upper(Alltrim(RACA))+", "+Upper(Alltrim(COR))

        @ Prow()+1,0  Say Replicate(Chr(196),85)
 
        Select ABORTO

        Skip 

    Enddo

    If PRN == "S"
       @ Prow(),Pcol() Say Chr(18)         
   
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

    SET CENTURY ON

Enddo

Return .T.


*******************
Function rpr_PARTOS
*******************

Setcolor(COR_CAB)
@ 03,42 Say "RELA"+Chr(128)+Chr(142)+"O DE PARTOS/DESMAME          "
Setcolor(COR_TEXT)

While .T.

    @ 05,00 Clear To 22,79
    @ 24,00 Clear To 24,79

    WCOD_M  = "999999" 
    WCOD_P  = "999999" 
    WDATA1  = DATE()
    WDATA2  = DATE()        

    @ 07,01 Say "Mae..............................:"
    @ 09,01 Say "Pai..............................:"
    @ 11,01 Say "Data do parto inicial............:"
    @ 13,01 Say "Data do parto final..............:"

    Setcolor(COR_TEXT)
    @ 15,01 say "Saida para impressora.......(S/N)?"

    @ 07,36 Get WCOD_M  Picture "999999" Valid view_ANIMAL(07,43,"F")
    @ 09,36 Get WCOD_P  Picture "999999" Valid view_ANIMAL(09,43,"M")

    @ 11,36 get WDATA1  Picture "@!"                         
    @ 13,36 get WDATA2  Picture "@!"     Valid WDATA2 >= WDATA1
    READ_F()

    If Lastkey() == 27
       @ 05,00 Clear To 22,79
       @ 24,00 Clear To 24,79
       Return .T.
    Endif

    PRN = REDUCAO(15,01,"Saida para impressora.......(S/N)?","N") 

    If CONFIRMA(24,"Confirma...(S/N)?","S") == "N"
       Loop
    Endif

    If PRN == "S"
       If ! chck_PRN()
          Loop
       Endif
    Endif

    If PRN == "N"
       WARQRPR = Alltrim(Str(RANDOM()%99999999,8))
       @ 13,44 Say "[ "+Right(Replicate(Chr(177),8)+WARQRPR,8)+" ]"
       WARQRPR = "RELAT\"+WARQRPR+".TXT"
    Endif
      
    Setcolor(COR_TEXT+"*")
    @ 24,70 Say "Aguarde..!"
    Setcolor(COR_TEXT)

    CAB = .T.
    PAG = 0
    TNA = 0

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

    WUNQ = .T.

    Select PARTO
    ordsetfocus('SINC004')
    Seek WDATA1

    While DATA <= WDATA2 .And. ! EOF()

        If WCOD_M <> "999999"
           If MAE <> WCOD_M
              Skip
              Loop
           Endif
        Endif

        If WCOD_P <> "999999"
           If PAI <> WCOD_P
              Skip
              Loop
           Endif
        Endif

        If Prow() > 58 .And. ! CAB .And. ! EOF()
  
           If PRN = "S"
              Eject
           Else
              @ Prow()+8,00 Say ""
              Setprc(0,0)
           Endif

           CAB = .T.
        Endif

        If CAB

           PAG++            
           CAB = .F.

           @ Prow(),00  Say "SERIAL SOFTWARE"
           CENTRO_PRN(85,Alltrim(CFG_EMPRES))
           @ Prow(),72  Say "Pag. "+Str(PAG,5)
           @ Prow()+1,0 Say ""

           If PAG == 1
              CENTRO_PRN(85,"RELACAO DE PARTO/DESMAME - ["+Alltrim(CRI)+"]")
           Else
              CENTRO_PRN(85,"RELACAO DE PARTO/DESMAME")
           Endif
           @ Prow(),72  Say "Data "+Dtoc(DATE())

           @ Prow()+1,0 Say Replicate(Chr(205),85)
           @ Prow()+1,0 Say "SEQ    DATA     MAE/PAI                        BEZERRO                         SEXO"
           @ Prow()+1,0 Say Replicate(Chr(205),85)

        Endif

        @ Prow()+1,0 Say Strzero(SEQ,6)
        @ Prow(),07  Say DATA

        WWCOD = MAE     

        Select ANIMAL
        ordsetfocus('SINC001')
        Seek WWCOD

        @ Prow(),16 Say NOME    

        Select PARTO

        @ Prow(),47 Say BEZERRO 

        If SEXO = "M"
           @ Prow(),79 Say "Macho"
        Elseif SEXO = "F"
           @ Prow(),79 Say "Femea"
        Endif

        WWCOD = PAI

        Select ANIMAL
        ordsetfocus('SINC001')
        Seek WWCOD

        @ Prow()+1,16 Say NOME    

        Select PARTO
        @ Prow(),47   Say "Resenha: "+Alltrim(RESENHA)
        
        If ! Empty(DESMAME)
           @ Prow()+1,47 Say "Desmame: "+Dtoc(DESMAME)
        Else
           @ Prow()+1,0  Say ""
        Endif

        @ Prow()+1,0 Say If(Empty(OBS),"","Obs: "+Alltrim(OBS))

        @ Prow()+1,0 Say Replicate(Chr(196),85)
 
        Skip 

    Enddo

    If PRN == "S"
       @ Prow(),Pcol() Say Chr(18)         
   
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

    SET CENTURY ON

Enddo

Return .T.


**********************
Function rpr_SINT_TRAT
**********************

Setcolor(COR_CAB)
@ 03,42 Say "RELA"+Chr(128)+Chr(142)+"O SINTOMAS/TRATAMENTOS       "
Setcolor(COR_TEXT)

While .T.

    @ 05,00 Clear To 22,79
    @ 24,00 Clear To 24,79

    WCODIGO  = "999999" 
    WSINTOMA = "999"
    WDATA1   = DATE()
    WDATA2   = DATE()        

    @ 07,01 Say "Aninal............................:"
    @ 09,01 Say "Sintoma...........................:     -"
    @ 11,01 Say "Data do sintoma/tratamento inicial:"
    @ 13,01 Say "Data do sintoma/tratamento final..:"

    Setcolor(COR_TEXT)
    @ 15,01 say "Saida para impressora........(S/N)?"

    @ 07,37 Get WCODIGO Picture "999999" Valid view_ANIMAL(07,44,"")
    @ 09,37 Get WSINTOMA                 Valid view_SINTOMA(09,43)
    @ 11,37 Get WDATA1  Picture "@!"                         
    @ 13,37 Get WDATA2  Picture "@!"     Valid WDATA2 >= WDATA1
    READ_F()

    If Lastkey() == 27
       @ 05,00 Clear To 22,79
       @ 24,00 Clear To 24,79
       Return .T.
    Endif

    PRN = REDUCAO(15,01,"Saida para impressora........(S/N)?","N") 

    If CONFIRMA(24,"Confirma...(S/N)?","S") == "N"
       Loop
    Endif

    If PRN == "S"
       If ! chck_PRN()
          Loop
       Endif
    Endif

    If PRN == "N"
       WARQRPR = Alltrim(Str(RANDOM()%99999999,8))
       @ 13,44 Say "[ "+Right(Replicate(Chr(177),8)+WARQRPR,8)+" ]"
       WARQRPR = "RELAT\"+WARQRPR+".TXT"
    Endif
      
    Setcolor(COR_TEXT+"*")
    @ 24,70 Say "Aguarde..!"
    Setcolor(COR_TEXT)

    CAB = .T.
    PAG = 0
    TNA = 0

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

    WUNQ = .T.

    Select SINTOMA
    ordsetfocus('SINC003')
    Seek WDATA1

    While DATA <= WDATA2 .And. ! EOF()

        If WCODIGO <> "999999"
           If ANIMAL <> WCODIGO
              Skip
              Loop
           Endif
        Endif

        If WSINTOMA <> "999"
           If SINTOMA <> WSINTOMA
              Skip
              Loop
           Endif
        Endif

        If Prow() > 58 .And. ! CAB .And. ! EOF()
  
           If PRN = "S"
              Eject
           Else
              @ Prow()+8,00 Say ""
              Setprc(0,0)
           Endif

           CAB = .T.
        Endif

        If CAB

           PAG++            
           CAB = .F.

           @ Prow(),00  Say "SERIAL SOFTWARE"
           CENTRO_PRN(85,Alltrim(CFG_EMPRES))
           @ Prow(),72  Say "Pag. "+Str(PAG,5)
           @ Prow()+1,0 Say ""

           If PAG == 1
              CENTRO_PRN(85,"RELACAO DE SINTOMAS/TRATAMENTOS - ["+Alltrim(CRI)+"]")
           Else
              CENTRO_PRN(85,"RELACAO DE SINTOMAS/TRATAMENTOS")
           Endif
           @ Prow(),72  Say "Data "+Dtoc(DATE())

           @ Prow()+1,0 Say Replicate(Chr(205),85)
           @ Prow()+1,0 Say "SEQ    DATA     NOME                           NASC     RACA/COR"
           @ Prow()+1,0 Say Replicate(Chr(205),85)

        Endif

        @ Prow()+1,0 Say Strzero(SEQ,6)
        @ Prow(),07  Say DATA

        WWCOD = ANIMAL

        Select ANIMAL
        ordsetfocus('SINC001')
        Seek WWCOD

        @ Prow(),16 Say NOME    
        @ Prow(),47 Say DTNASC
        @ Prow(),56 Say Upper(Alltrim(RACA))+", "+Upper(Alltrim(COR))

        Select SINTOMA

        @ Prow()+2,0 Say "Sintoma...:"

        WWSINT = SINTOMA

        Select CSINTOMA
        ordsetfocus('SINC001')
        Seek WWSINT

        @ Prow(),12 Say SINTOMA

        Select SINTOMA

        @ Prow()+1,12 Say COMPLEM
        @ Prow()+1,0  Say "Tratamento:"
        @ Prow(),12   Say TRATA1  
        @ Prow()+1,12 Say TRATA2  
        @ Prow()+1,12 Say TRATA3  

        @ Prow()+1,0  Say Replicate(Chr(196),85)
 
        Skip 

    Enddo

    If PRN == "S"
       @ Prow(),Pcol() Say Chr(18)         
   
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

    SET CENTURY ON

Enddo

Return .T.


****************************
Function view_SINTOMA(LL,CC)
****************************

WVAR = READVAR()

If &WVAR = "999"
   @ LL,CC Say Space(30)
   @ LL,CC Say "GERAL"
   Return .T.
Endif

If Empty(&WVAR)
   help_SINTOMA()
   Return .F.
Endif

AREA = ALIAS()

Select CSINTOMA
ordsetfocus('SINC001')
Seek &WVAR

If ! Found()
   Setcursor(0)
   @ 24,00 Clear To 24,79
   CENTRO(24,"Sintoma n"+chr(132)+"o cadastrado...")
   Inkey(5)
   @ 24,00 Clear To 24,70
   Setcursor(1)
   @ LL,CC Say Space(25)
   Select &AREA
   Return .F.
Endif

@ LL,CC Say SINTOMA

Select &AREA

Return .T.


*****************
Function rpr_PESO
*****************

Setcolor(COR_CAB)
@ 03,42 Say "RELA"+Chr(128)+Chr(142)+"O PESO/CONDI"+Chr(128)+Chr(142)+"O CORPOREA     "
Setcolor(COR_TEXT)

While .T.

    @ 05,00 Clear To 22,79
    @ 24,00 Clear To 24,79

    WCODIGO = "999999" 
    WDATA1  = DATE()
    WDATA2  = DATE()        

    @ 07,01 Say "Aninal.......................:"
    @ 09,01 Say "Data da pesagem inicial......:"
    @ 11,01 Say "Data da pesagem final........:"
    @ 13,01 say "Saida para impressora...(S/N)?"

    @ 07,32 Get WCODIGO Picture "999999" Valid view_ANIMAL(07,39,"")
    @ 09,32 Get WDATA1  Picture "@!"                         
    @ 11,32 Get WDATA2  Picture "@!"     Valid WDATA2 >= WDATA1
    READ_F()

    If Lastkey() == 27
       @ 05,00 Clear To 22,79
       @ 24,00 Clear To 24,79
       Return .T.
    Endif

    PRN = REDUCAO(13,01,"Saida para impressora...(S/N)?","N") 

    If CONFIRMA(24,"Confirma...(S/N)?","S") == "N"
       Loop
    Endif

    If PRN == "S"
       If ! chck_PRN()
          Loop
       Endif
    Endif

    If PRN == "N"
       WARQRPR = Alltrim(Str(RANDOM()%99999999,8))
       @ 13,44 Say "[ "+Right(Replicate(Chr(177),8)+WARQRPR,8)+" ]"
       WARQRPR = "RELAT\"+WARQRPR+".TXT"
    Endif
      
    Setcolor(COR_TEXT+"*")
    @ 24,70 Say "Aguarde..!"
    Setcolor(COR_TEXT)

    Select 101 
    USE TEMP\PESAGEM.TMP Exclusive Alias TMP
    ZAP
    Index On Upper(NOME)+Dtos(DATA) To TEMP\PESAGEM.IDX

    Select PESO
    ordsetfocus('SINC004')
    Seek WDATA1

    While DATA <= WDATA2 .And. ! EOF()

        If WCODIGO <> "999999"
           If ANIMAL <> WCODIGO
              Skip
              Loop
           Endif
        Endif

        WWSEQ  = SEQ
        WWDATA = DATE
        WWCOD  = ANIMAL
        WWNOME = ""
        WWPESO = PESO
        WWCOND = CONDICAO

        Select ANIMAL
        ordsetfocus('SINC001')
        Seek WWCOD
        
        WWNOME = NOME

        Select TMP
        DBAPPEND()
        Replace SEQ      With WWSEQ ,;
                DATA     With WWDATA,; 
                ANIMAL   With WWCOD ,; 
                NOME     With WWNOME,; 
                PESO     With WWPESO,; 
                CONDICAO With WWCOND 
 
        Select PESO
        Skip 

    Enddo

    CAB = .T.
    PAG = 0

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

    UNQ       = .T.
    UNQ2      = .T.
    SLT       = 1
    WPESO_ANT = 0

    Select TMP
    ordsetfocus('SINC001')
    Go Top

    While ! EOF()

        If Prow() > 58 .And. ! CAB .And. ! EOF()
  
           If PRN = "S"
              Eject
           Else
              @ Prow()+8,00 Say ""
              Setprc(0,0)
           Endif

           CAB = .T.
        Endif

        If CAB

           PAG++            

           CAB = .F.
           UNQ = .T.

           @ Prow(),00  Say "SERIAL SOFTWARE"
           CENTRO_PRN(85,Alltrim(CFG_EMPRES))
           @ Prow(),72  Say "Pag. "+Str(PAG,5)
           @ Prow()+1,0 Say ""

           If PAG == 1
              CENTRO_PRN(85,"RELACAO DE PESO/COND.CORPOREA - ["+Alltrim(CRI)+"]")
           Else
              CENTRO_PRN(85,"RELACAO DE PESO/COND.CORPOREA")
           Endif
           @ Prow(),72  Say "Data "+Dtoc(DATE())

           @ Prow()+1,0 Say Replicate(Chr(205),85)
           @ Prow()+1,0 Say "NOME                           SEQ    DATA     CONDICAO                   PESO  GANHO"
           @ Prow()+1,0 Say Replicate(Chr(205),85)

        Endif

        If UNQ 
           @ Prow()+1,0 Say NOME
           UNQ = .F.
        Else
           @ Prow()+SLT,0 Say ""
        Endif

        @ Prow(),31 Say Strzero(SEQ,6)
        @ Prow(),38 Say DATA
        @ Prow(),48 Say Left(CONDICAO,20)
        @ Prow(),70 Say PESO Picture "@R@E 999.99Kg"

        WGANHO = 100 - ((WPESO_ANT / PESO) * 100)

        If WPESO_ANT <= 0
           WGANHO = 0
        Endif

        @ Prow(),79 Say WGANHO Picture "@R@E 999.9%"

        WPESO_ANT = PESO
        WWCOD     = ANIMAL

        Select TMP
        Skip

        If ANIMAL <> WWCOD .Or. EOF()
           @ Prow()+1,0  Say Replicate(Chr(196),85)
           WPESO_ANT = 0
           UNQ  = .T.
           SLT  = 1
        Endif

    Enddo

    If PRN == "S"
       @ Prow(),Pcol() Say Chr(18)         
   
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

    SET CENTURY ON

    Select 101
    DBCLOSEAREA()

    Select PESO

Enddo

Return .T.


********************
Function rpr_VACINAS
********************

Setcolor(COR_CAB)
@ 03,42 Say "RELA"+Chr(128)+Chr(142)+"O ANIMAIS VACINADOS          "
Setcolor(COR_TEXT)

While .T.

    @ 05,00 Clear To 22,79
    @ 24,00 Clear To 24,79

    WCODIGO = "999999" 
    WVACINA = "999"
    WDATA1  = DATE()
    WDATA2  = DATE()        

    @ 07,01 Say "Aninal.......................:"
    @ 09,01 Say "Vacina.......................:     -"
    @ 11,01 Say "Data da vacina inicial.......:"
    @ 13,01 Say "Data da vacina final.........:"

    Setcolor(COR_TEXT)
    @ 15,01 say "Saida para impressora...(S/N)?"

    @ 07,32 Get WCODIGO Picture "999999" Valid view_ANIMAL(07,39,"")
    @ 09,32 Get WVACINA                  Valid view_VACINA(09,38)
    @ 11,32 Get WDATA1  Picture "@!"                         
    @ 13,32 Get WDATA2  Picture "@!"     Valid WDATA2 >= WDATA1
    READ_F()

    If Lastkey() == 27
       @ 05,00 Clear To 22,79
       @ 24,00 Clear To 24,79
       Return .T.
    Endif

    PRN = REDUCAO(15,01,"Saida para impressora...(S/N)?","N") 

    If CONFIRMA(24,"Confirma...(S/N)?","S") == "N"
       Loop
    Endif

    If PRN == "S"
       If ! chck_PRN()
          Loop
       Endif
    Endif

    If PRN == "N"
       WARQRPR = Alltrim(Str(RANDOM()%99999999,8))
       @ 13,44 Say "[ "+Right(Replicate(Chr(177),8)+WARQRPR,8)+" ]"
       WARQRPR = "RELAT\"+WARQRPR+".TXT"
    Endif
      
    Setcolor(COR_TEXT+"*")
    @ 24,70 Say "Aguarde..!"
    Setcolor(COR_TEXT)

    CAB = .T.
    PAG = 0
    TNA = 0

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

    WUNQ = .T.

    Select VACINA
    ordsetfocus('SINC003')
    Seek WDATA1

    While DATA <= WDATA2 .And. ! EOF()

        If WCODIGO <> "999999"
           If ANIMAL <> WCODIGO
              Skip
              Loop
           Endif
        Endif

        If WVACINA <> "999"
           If VACINA <> WVACINA  
              Skip
              Loop
           Endif
        Endif

        If Prow() > 58 .And. ! CAB .And. ! EOF()
  
           If PRN = "S"
              Eject
           Else
              @ Prow()+8,00 Say ""
              Setprc(0,0)
           Endif

           CAB = .T.
        Endif

        If CAB

           PAG++            
           CAB = .F.

           @ Prow(),00  Say "SERIAL SOFTWARE"
           CENTRO_PRN(85,Alltrim(CFG_EMPRES))
           @ Prow(),72  Say "Pag. "+Str(PAG,5)
           @ Prow()+1,0 Say ""

           If PAG == 1
              CENTRO_PRN(85,"RELACAO DE ANIMAIS VACINADOS - ["+Alltrim(CRI)+"]")
           Else
              CENTRO_PRN(85,"RELACAO DE ANIMAIS VACINADOS")
           Endif
           @ Prow(),72  Say "Data "+Dtoc(DATE())

           @ Prow()+1,0 Say Replicate(Chr(205),85)
           @ Prow()+1,0 Say "SEQ    NOME                           DATA     VACINA                        PROXIMA"
           @ Prow()+1,0 Say Replicate(Chr(205),85)

        Endif

        @ Prow()+1,0 Say Strzero(SEQ,6)

        WWCOD  = ANIMAL
        WWDATA = DATA
        WWVAC  = VACINA

        Select ANIMAL
        ordsetfocus('SINC001')
        Seek WWCOD

        @ Prow(),07 Say NOME    
        @ Prow(),38 Say WWDATA

        Select CVACINA
        ordsetfocus('SINC001')
        Seek WWVAC
 
        If Found()
           @ Prow(),47 Say WWVAC+"-"+VACINA
           WWPROX = WWDATA + FREQUENCIA
           @ Prow(),77 Say WWPROX
        Else
           @ Prow(),47 Say "<Nao cadastrada>"
           @ Prow(),77 Say "-"    
        Endif
 
        Select VACINA
        Skip 

    Enddo

    If PRN == "S"
       @ Prow(),Pcol() Say Chr(18)         
   
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

    SET CENTURY ON

Enddo

Return .T.


***************************
Function view_VACINA(LL,CC)
***************************

WVAR = READVAR()

If &WVAR = "999"
   @ LL,CC Say Space(25)
   @ LL,CC Say "GERAL"
   Return .T.
Endif

If Empty(&WVAR)
   help_VACINA()
   Return .F.
Endif

AREA = ALIAS()

Select CVACINA
ordsetfocus('SINC001')
Seek &WVAR

If ! Found()
   Setcursor(0)
   @ 24,00 Clear To 24,79
   CENTRO(24,"Vacina n"+chr(132)+"o cadastrada...")
   Inkey(5)
   @ 24,00 Clear To 24,70
   Setcursor(1)
   @ LL,CC Say Space(25)
   Select &AREA
   Return .F.
Endif

@ LL,CC Say VACINA

Select &AREA

Return .T.

