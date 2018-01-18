*********************************************************************
****  Sistema......: SINCON - Sistema Integrado de Controle      ****
****  Versao.......: 1.0                                         ****
****  Programa.....: SINCON06.PRG                                ****
****  Finalidade...: Abertura das tabelas/banco de dados         ****
****                                                             ****
****          NTI - NÚCLEO DE TECNOLOGIA DA INFORMAÇÃO           ****
*********************************************************************

*******************
function OPEN_ARQ()
*******************
public AOF, ACDX, TCDX 

AGUARDE()

TCDX = 73
AOF  = 28     
ACDX = 0

CDX_AUXILIAR()
CDX_PRINCIPAL()

DBCLOSEALL()  

AGUARDE()

AOF = 000       

DBF_AUXILIAR()
DBF_PRINCIPAL()

@ 24,00 clear to 24,79

return .t.

*********************
function DBF_AUXILIAR
*********************

abre(AOF,"AUXILIAR\SENHA")
abre(AOF,"AUXILIAR\EXTENSO")
abre(AOF,"AUXILIAR\CSINTOMA")
abre(AOF,"AUXILIAR\CVACINA")
abre(AOF,"AUXILIAR\RACA")
abre(AOF,"AUXILIAR\CATEG")
abre(AOF,"AUXILIAR\PASTO")
abre(AOF,"AUXILIAR\PENDENC")
abre(AOF,"AUXILIAR\GRUPO")
abre(AOF,"AUXILIAR\LOTE")

return .t.


**********************
function DBF_PRINCIPAL
**********************

abre(AOF,"&WDIRE\ANIMAL")
abre(AOF,"&WDIRE\SINTOMA")
abre(AOF,"&WDIRE\PESO")
abre(AOF,"&WDIRE\VACINA")
abre(AOF,"&WDIRE\LEITE")
abre(AOF,"&WDIRE\SECA")
abre(AOF,"&WDIRE\PARTO")
abre(AOF,"&WDIRE\CIO")
abre(AOF,"&WDIRE\SERVICO")
abre(AOF,"&WDIRE\ABORTO")
abre(AOF,"&WDIRE\EXAME")
abre(AOF,"&WDIRE\BANCO")
abre(AOF,"&WDIRE\GRUPOS")
abre(AOF,"&WDIRE\PRODUTOS")
abre(AOF,"&WDIRE\OUTRAS_E")
abre(AOF,"&WDIRE\ITENS_E")
abre(AOF,"&WDIRE\OUTRAS_S")
abre(AOF,"&WDIRE\ITENS_S")

return .T.

*********************
function CDX_AUXILIAR
*********************

*** LOGIN
ACDX++
if ! file("AUXILIAR\SENHA.CDX")
   barraCDX(ACDX,TCDX,"AUXILIAR\SENHA")
   abre(AOF,"AUXILIAR\SENHA","E")
   index on uncrp(USUARIO) tag SINC001
   DBCLOSEAREA()
endif

*** EXTENSO
ACDX++
if ! file("AUXILIAR\EXTENSO.CDX")
   barraCDX(ACDX,TCDX,"AUXILIAR\EXTENSO")
   abre(AOF,"AUXILIAR\EXTENSO","E")
   index on CODIGO tag SINC001
   DBCLOSEAREA()
endif

*** CSINTOMA
ACDX++
if ! file("AUXILIAR\CSINTOMA.CDX")
   barraCDX(ACDX,TCDX,"AUXILIAR\CSINTOMA")
   abre(AOF,"AUXILIAR\CSINTOMA","E")
   index on upper(CODIGO)  tag SINC001
   index on upper(SINTOMA) tag SINC002
   DBCLOSEAREA()    
endif

*** CVACINA
ACDX++
if ! file("AUXILIAR\CVACINA.CDX")
   barraCDX(ACDX,TCDX,"AUXILIAR\CVACINA")
   abre(AOF,"AUXILIAR\CVACINA","E")
   index on upper(CODIGO) tag SINC001
   index on upper(VACINA) tag SINC002
   DBCLOSEAREA()
endif

*** RACA
ACDX++
if ! file("AUXILIAR\RACA.CDX")
   barraCDX(ACDX,TCDX,"AUXILIAR\RACA")
   abre(AOF,"AUXILIAR\RACA","E")
   index on CODIGO      tag SINC001
   index on upper(RACA) tag SINC002
   DBCLOSEAREA()
endif

*** CATEG   
ACDX++
if ! file("AUXILIAR\CATEG.CDX")
   barraCDX(ACDX,TCDX,"AUXILIAR\CATEG")
   abre(AOF,"AUXILIAR\CATEG","E")
   index on upper(CODIGO)    tag SINC001
   index on upper(CATEGORIA) tag SINC002
   DBCLOSEAREA()
endif

*** PASTO
ACDX++
if ! file("AUXILIAR\PASTO.CDX")
   barraCDX(ACDX,TCDX,"AUXILIAR\PASTO")
   abre(AOF,"AUXILIAR\PASTO","E")
   index on upper(PASTO) tag SINC001
   DBCLOSEAREA()
endif

*** PENDENC 
ACDX++
if ! file("AUXILIAR\PENDENC.CDX")
   barraCDX(ACDX,TCDX,"AUXILIAR\PENDENC")
   abre(AOF,"AUXILIAR\PENDENC","E")
   index on CODIGO            tag SINC001
   index on upper(DESCRICAO)  tag SINC002
   DBCLOSEAREA()
endif

*** GRUPO 
ACDX++
If ! File("AUXILIAR\GRUPO.CDX")
   barraCDX(ACDX,TCDX,"AUXILIAR\GRUPO")
   abre(AOF,"AUXILIAR\GRUPO","E")
   index On CODIGO           tag SINC001
   index on upper(DESCRICAO) tag SINC002
   DBCLOSEAREA()
Endif

*** LOTE
ACDX++
if ! file("AUXILIAR\LOTE.CDX")
   barraCDX(ACDX,TCDX,"AUXILIAR\LOTE")
   abre(AOF,"AUXILIAR\LOTE","E")
   index on CODIGO           tag SINC001
   index on upper(DESCRICAO) tag SINC002
   DBCLOSEAREA()
endif

return .t.


**********************
function CDX_PRINCIPAL
**********************

*** ANIMAL      
ACDX++
if ! file("&WDIRE\ANIMAL.CDX")
   barraCDX(ACDX,TCDX,"&WDIRE\ANIMAL")
   abre(AOF,"&WDIRE\ANIMAL","E")
   index on CODIGO                  tag SINC001
   index on upper(NOME)             tag SINC002
   index on upper(LOTE)+CODIGO      tag SINC003
   index on upper(LOTE)+upper(NOME) tag SINC004 
   index on NREG                    tag SINC005
   DBCLOSEAREA()  
endif

*** SINTOMA  
ACDX++
if ! file("&WDIRE\SINTOMA.CDX")
   barraCDX(ACDX,TCDX,"&WDIRE\SINTOMA")
   abre(AOF,"&WDIRE\SINTOMA","E")
   index on SEQ                         tag SINC001
   index on ANIMAL+descend(dtos(DATA))  tag SINC002
   index on DATA                        tag SINC003
   DBCLOSEAREA()
endif

*** PESO   
ACDX++
if ! file("&WDIRE\PESO.CDX")
   barraCDX(ACDX,TCDX,"&WDIRE\PESO")
   abre(AOF,"&WDIRE\PESO","E")
   index on SEQ                         tag SINC001
   index on ANIMAL+descend(dtos(DATA))  tag SINC002
   index on ANIMAL+dtos(DATA)           tag SINC003
   index on DATA                        tag SINC004
   DBCLOSEAREA()
endif

*** VACINA  
ACDX++
if ! file("&WDIRE\VACINA.CDX")
   barraCDX(ACDX,TCDX,"&WDIRE\VACINA")
   abre(AOF,"&WDIRE\VACINA","E")
   index on SEQ                        tag SINC001
   index on ANIMAL+descend(dtos(DATA)) tag SINC002
   index on DATA                       tag SINC003
   DBCLOSEAREA()
endif

*** LEITE
ACDX++
if ! file("&WDIRE\LEITE.CDX")
   barraCDX(ACDX,TCDX,"&WDIRE\LEITE")
   abre(AOF,"&WDIRE\LEITE","E")
   index on SEQ                        tag SINC001
   index on ANIMAL+descend(dtos(DATA)) tag SINC002
   DBCLOSEAREA()
endif

*** SECA 
ACDX++
if ! file("&WDIRE\SECA.CDX")
   barraCDX(ACDX,TCDX,"&WDIRE\SECA")
   abre(AOF,"&WDIRE\SECA","E")
   index on SEQ                        tag SINC001
   index on ANIMAL+descend(dtos(DATA)) tag SINC002
   DBCLOSEAREA()
endif

*** PARTO  
ACDX++
if ! file("&WDIRE\PARTO.CDX")
   barraCDX(ACDX,TCDX,"&WDIRE\PARTO")
   abre(AOF,"&WDIRE\PARTO","E")
   index on SEQ                     tag SINC001
   index on MAE+descend(dtos(DATA)) tag SINC002
   index on PAI+descend(dtos(DATA)) tag SINC003
   index on DATA                    tag SINC004
   DBCLOSEAREA()
endif

*** CIO 
ACDX++
if ! file("&WDIRE\CIO.CDX")
   barraCDX(ACDX,TCDX,"&WDIRE\CIO")
   abre(AOF,"&WDIRE\CIO","E")
   index on SEQ                        tag SINC001
   index on ANIMAL+descend(dtos(DATA)) tag SINC002
   index on DATA                       tag SINC003
   DBCLOSEAREA()
endif

*** SERVICO
ACDX++
if ! file("&WDIRE\SERVICO.CDX")
   barraCDX(ACDX,TCDX,"&WDIRE\SERVICO")
   abre(AOF,"&WDIRE\SERVICO","E")
   index on SEQ                        tag SINC001
   index on ANIMAL+descend(dtos(DATA)) tag SINC002
   index on TOURO+Descend(dtos(DATA))  tag SINC003
   index on LOTE+Descend(dtos(DATA))   tag SINC004
   index on upper(TOURO_TE)            tag SINC005
   index on DATA                       tag SINC006
   DBCLOSEAREA()
endif

*** ABORTO
ACDX++
if ! file("&WDIRE\ABORTO.CDX")
   barraCDX(ACDX,TCDX,"&WDIRE\ABORTO")
   abre(AOF,"&WDIRE\ABORTO","E")
   index on SEQ                        tag SINC001
   index on ANIMAL+descend(dtos(DATA)) tag SINC002
   index on DATA                       tag SINC003
   DBCLOSEAREA()
endif

*** EXAME  
ACDX++
if ! file("&WDIRE\EXAME.CDX")
   barraCDX(ACDX,TCDX,"&WDIRE\EXAME")
   abre(AOF,"&WDIRE\EXAME","E")
   index on SEQ                        tag SINC001
   index on ANIMAL+descend(dtos(DATA)) tag SINC002
   index on TOURO+Descend(dtos(DATA))  tag SINC003
   index on DATA                       tag SINC004
   DBCLOSEAREA()
endif

*** BANCO  
ACDX++
if ! file("&WDIRE\BANCO.CDX")
   barraCDX(ACDX,TCDX,"&WDIRE\BANCO")
   abre(AOF,"&WDIRE\BANCO","E")
   index on LOTE                              tag SINC001
   index on DATA                              tag SINC002
   index on upper(DOADOR)+descend(dtos(DATA)) tag SINC003 
   DBCLOSEAREA()
endif

*** GRUPOS   
ACDX++
if ! file("&WDIRE\GRUPOS.CDX")
   barraCDX(ACDX,TCDX,"&WDIRE\GRUPOS")
   abre(AOF,"&WDIRE\GRUPOS","E")
   index on CODIGO        tag SINC001
   index on upper(GRUPO)  tag SINC002
   DBCLOSEAREA()
endif

*** PRODUTOS 
ACDX++
if ! file("&WDIRE\PRODUTOS.CDX")
   barraCDX(ACDX,TCDX,"&WDIRE\PRODUTOS")
   abre(AOF,"&WDIRE\PRODUTOS","E")
   index on CODIGO                 tag SINC001
   index on upper(PRODUTO)         tag SINC002
   index on COD_GRP+Upper(PRODUTO) tag SINC003
   index on val(CODIGO)            tag SINC004
   DBCLOSEAREA()
endif

*** OUTRAS_E 
ACDX++
if ! file("&WDIRE\OUTRAS_E.CDX")
   barraCDX(ACDX,TCDX,"&WDIRE\OUTRAS_E")
   abre(AOF,"&WDIRE\OUTRAS_E","E")
   index on SEQ     tag SINC001
   index on DOCNO   tag SINC002
   index on ENTRADA tag SINC003
   DBCLOSEAREA()
endif

*** ITENS_E
ACDX++
if ! File("&WDIRE\ITENS_E.CDX")
   BARRACDX(ACDX,TCDX,"&WDIRE\ITENS_E")
   ABRE(AOF,"&WDIRE\ITENS_E","E")
   index On SEQ tag SINC001
   DBCLOSEAREA()
endif

*** OUTRAS_S
ACDX++
if ! file("&WDIRE\OUTRAS_S.CDX")
   barraCDX(ACDX,TCDX,"&WDIRE\OUTRAS_S")
   abre(AOF,"&WDIRE\OUTRAS_S","E")
   index on SEQ     tag SINC001
   index on DOCNO   tag SINC002
   index on SAIDA   tag SINC003
   DBCLOSEAREA()
endif

*** ITENS_S 
ACDX++
if ! file("&WDIRE\ITENS_S.CDX")
   barraCDX(ACDX,TCDX,"&WDIRE\ITENS_S")
   abre(AOF,"&WDIRE\ITENS_S","E")
   index On SEQ tag SINC001
   DBCLOSEAREA()
endif

return .t.

*************************************
function barraCDX(ACDX, TCDX, ARQCDX)
*************************************
ARQCDX = "Indexando arquivo "+ARQCDX+".dbf"
ARQCDX = left(space(40-(len(ARQCDX)/2))+ARQCDX+space(80),80)

setcolor(COR_DEST)
for I = 1 to ((80/TCDX)*ACDX)
   @ 24,(I-1) say substr(ARQCDX,I,1)
next

setcolor(COR_TEXT)
for X = I to 80
   @ 24,(X-1) say substr(ARQCDX,X,1)
next

return .t.

