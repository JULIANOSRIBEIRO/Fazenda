*********************************************************************
****  Sistema......: SINCON - Sistema Integrado de Controle      ****
****  Versao.......: 3.0 - RACOES PILOTO                         ****
****  Programa.....: SINCON01.prg                                ****
****  Finalidade...: Manutencao dos Dados da Empresa             ****
****  Programador..: Sergio Alexandre de Carvalho                ****
****                                                             ****
****        Copyright (C) SERIAL SOFTWARE                        ****
****                                                             ****
****  Compilador...: CLIPPER (R) versao  5.01                    ****
****  Linkeditor...: RTLink (R)  versao  3.13                    ****
*********************************************************************

****************
function EMPRESA
****************

@ 05,00 clear to 22,79
@ 24,00 clear to 24,79

setcolor(COR_CAB)
@ 03,42 say "DADOS DA FAZENDA               "
setcolor(COR_TEXT)

@ 08,02 say "Fazenda.....:"
@ 09,02 say "Proprietario:"             
@ 11,02 say "Endere"+chr(135)+"o....:"
@ 12,02 say "Bairro......:" 
@ 13,02 say "Cidade......:" 
@ 14,02 say "Estado......:"
@ 14,42 say "Cep.........:"
@ 15,02 say "Telefone....:"
@ 15,42 say "Fax.........:"

@ 17,02 say "Resp.Tecnico:"

do while .t.
   setcolor(COR_CONS)
   @ 24,72 say "Esc-FIM"
   setcolor(COR_TEXT)

   @ 08,16 get CFG_EMPRES  
   @ 09,16 get CFG_PROPRI 

   @ 11,16 get CFG_ENDERE   
   @ 12,16 get CFG_BAIRRO
   @ 13,16 get CFG_CIDADE
   @ 14,16 get CFG_ESTADO
   @ 14,56 get CFG_CEP       picture "@R 99999-999"
   @ 15,16 get CFG_TEL       
   @ 15,56 get CFG_FAX       

   @ 17,16 get CFG_TECNIC  
   read_f()

   if lastkey() = 27
      @ 24,00 clear to 24,79
      DIR = WDIRE+"\EMPRESA.CFG"
      restore from &DIR additive      
     
      setcolor(COR_CAB)
      @ 01,02 say CFG_EMPRES  
      setcolor(COR_TEXT)

      return .t.
   endif

   if confirma(24,"Dados Corretos...(S/N)?","S") == "S"
      DIR = WDIRE+"\EMPRESA.CFG"
      save all like CFG_* to &DIR 
       
      @ 24,00 clear to 24,79
      
      setcolor(COR_CAB)
      @ 01,02 say CFG_EMPRES  
      setcolor(COR_TEXT)

      return .t.
   endif
enddo

return .t.
