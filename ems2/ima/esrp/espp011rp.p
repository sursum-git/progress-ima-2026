/* Programa de controle de versao e seguran»a do Datasul EMS */
{include/i-prgvrs.i espp011rp 2.04.00.001}

DEF BUFFER empresa FOR mgadm.empresa.
DEF BUFFER moeda FOR mgadm.moeda.

/* Definicao de variaveis globais comuns a todos os programas do EMS */
{utp/ut-glob.i}

DEFINE TEMP-TABLE tt-param NO-UNDO
    FIELD destino          AS INTEGER
    FIELD arquivo          AS CHAR FORMAT "x(35)"
    FIELD usuario          AS CHAR FORMAT "x(12)"
    FIELD data-exec        AS DATE
    FIELD hora-exec        AS INTEGER
    FIELD modelo-rtf       AS CHAR FORMAT "x(35)"
    FIELD l-habilitaRtf    AS LOG
    FIELD cod-estabel      AS CHAR
    FIELD nr-container-ini AS INT
    FIELD nr-container-fim AS INT
    FIELD it-codigo-ini    AS CHAR
    FIELD it-codigo-fim    AS CHAR
    FIELD cod-refer-ini    AS CHAR
    FIELD cod-refer-fim    AS CHAR
    FIELD no-ab-reppri-ini AS CHAR
    FIELD no-ab-reppri-fim AS CHAR
    FIELD cod-sit-ped      AS INTEGER
    FIELD cod-sit-ctner    AS INTEGER
    FIELD us-dolar         AS DEC
    FIELD resumido         AS LOG.

DEFINE TEMP-TABLE tt-digita NO-UNDO
    FIELD ordem            AS INTEGER   FORMAT ">>>>9"
    FIELD exemplo          AS CHARACTER FORMAT "x(30)"
    INDEX id ordem.
    
/* Parametros de entrada logica obrigatoria */
DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita                   AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.

DEFINE TEMP-TABLE tt-pedidos
       FIELD nr-container LIKE pp-ped-venda.nr-container   
       FIELD nr-pedcli    LIKE pp-ped-venda.nr-pedcli   
       FIELD dt-implant   LIKE pp-ped-venda.dt-implant
       FIELD nome-abrev   LIKE pp-ped-venda.nome-abrev  
       FIELD no-ab-reppri LIKE pp-ped-venda.no-ab-reppri
       FIELD it-codigo    LIKE pp-ped-item.it-codigo
       FIELD desc-item    LIKE ITEM.desc-item
       FIELD cod-refer    LIKE pp-ped-item.cod-refer
       FIELD qt-pedida    LIKE pp-ped-item.qt-pedida    
       FIELD moeda        LIKE moeda.descricao
       FIELD vl-item      AS DEC
       FIELD tot-item     AS DEC.

DEFINE TEMP-TABLE tt-resumo
       FIELD nr-container LIKE pp-ped-venda.nr-container   
       FIELD it-codigo    LIKE pp-ped-item.it-codigo
       FIELD desc-item    LIKE ITEM.desc-item
       FIELD qt-comprada  LIKE pp-ped-item.qt-pedida    
       FIELD qt-vendida   LIKE pp-ped-item.qt-pedida    
       FIELD vl-tot-ven   AS DEC
       FIELD qt-disp      LIKE pp-ped-item.qt-pedida.

DEF VAR h-acomp      AS HANDLE  NO-UNDO.
DEF VAR c-sit-ped    AS CHAR.
DEF VAR c-tp-pedido  AS CHAR.
DEF VAR c-desc-item  AS CHAR.
DEF VAR de-tot-item  AS DEC.
DEF VAR de-tot-rep   AS DEC.
DEF VAR de-vl-item   AS DEC.
DEF VAR c-moeda      AS CHAR.

{include/i-rpvar.i}   

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

FOR EACH tt-raw-digita:
    CREATE tt-digita.
    RAW-TRANSFER tt-raw-digita.raw-digita TO tt-digita.
END.

FORM
   tt-pedidos.nr-container  COLUMN-LABEL "Container"
   tt-pedidos.nr-pedcli     COLUMN-LABEL "Pedido"     FORMAT "x(7)"
   tt-pedidos.nome-abrev    COLUMN-LABEL "Cliente"       
   tt-pedidos.no-ab-reppri  COLUMN-LABEL "Repres"     FORMAT "x(12)" 
   tt-pedidos.dt-implant    COLUMN-LABEL "Data Ped"   
   tt-pedidos.it-codigo     COLUMN-LABEL "Item"       FORMAT "x(8)"
   tt-pedidos.desc-item     COLUMN-LABEL "Descri‡Æo"  FORMAT "x(32)"
   tt-pedidos.cod-refer     COLUMN-LABEL "Ref"        FORMAT "x(5)"
   tt-pedidos.qt-pedida     COLUMN-LABEL "Quant"      FORMAT ">>>,>>9.99" 
   tt-pedidos.moeda         COLUMN-LABEL "Mo"         FORMAT "x(3)"
   tt-pedidos.vl-item       COLUMN-LABEL "Valor"      FORMAT ">>,>>9.99" 
   tt-pedidos.tot-item      COLUMN-LABEL "TotItem"    FORMAT ">>,>>>,>>9.99" 
   WITH WIDTH 152 55 DOWN STREAM-IO FRAME f-detalhe.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Processando..").

/* ABERTURA DO ARQUIVO DE SAÖDA (ARQUIVO/IMPRESSORA) CORREPONDE A INCLUDE CDP/CD9520.I (MAGNUS) */
{include/i-rpout.i}
{include/i-rpcab.i}

/* LOCALIZACAO DO NOME DA EMPRESA PARA COMPOR O CABECALHO PADRAO */
FIND empresa NO-LOCK WHERE empresa.ep-codigo = i-ep-codigo-usuario NO-ERROR.
IF NOT AVAIL empresa THEN
   RETURN "ADM-ERROR":U.

/* VALORIZACAO DAS OUTRAS VARIAVEIS QUE COMPOEM O CABECALHO PADRAO */
ASSIGN c-programa     = "ESPP011.W":U
       c-versao       = "2.04":U
       c-revisao      = ".00.001":U
       c-empresa      = empresa.razao-social
       c-sistema      = "EMS":U.

ASSIGN c-titulo-relat = "RELATORIO DE PEDIDOS":U. 

VIEW FRAME f-cabec.
VIEW FRAME f-rodape.

IF tt-param.us-dolar = 0 THEN
   ASSIGN tt-param.us-dolar = 1.

CASE tt-param.cod-sit-ped.
    WHEN 1 THEN ASSIGN c-sit-ped = '1,4'
                       c-tp-pedido = 'PI'.
    WHEN 3 THEN ASSIGN c-sit-ped = '1,4'
                       c-tp-pedido = 'PE'.
    WHEN 6 THEN ASSIGN c-sit-ped = '6'
                       c-tp-pedido = 'PI'.
END CASE.


FOR EACH ped-venda WHERE
         LOOKUP(STRING(ped-venda.cod-sit-ped),c-sit-ped) > 0 AND
         ped-venda.tp-pedido = c-tp-pedido AND
         ped-venda.cod-estabel = tt-param.cod-estabel AND 
         ped-venda.no-ab-reppri  >= tt-param.no-ab-reppri-ini AND
         ped-venda.no-ab-reppri  <= tt-param.no-ab-reppri-fim NO-LOCK,
    FIRST ped-venda-ext WHERE
          ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND
          ped-venda-ext.nr-pedido = ped-venda.nr-pedido  AND
          ped-venda-ext.nr-container >= tt-param.nr-container-ini AND
          ped-venda-ext.nr-container <= tt-param.nr-container-fim NO-LOCK,
    EACH ped-item OF ped-venda WHERE
         ped-item.it-codigo >= tt-param.it-codigo-ini AND
         ped-item.it-codigo <= tt-param.it-codigo-fim AND 
         ped-item.cod-refer >= tt-param.cod-refer-ini AND 
         ped-item.cod-refer <= tt-param.cod-refer-fim NO-LOCK
         BREAK BY ped-venda.nr-pedcli
               BY ped-item.it-codigo
               BY ped-item.cod-refer.


    FIND pp-container WHERE
         pp-container.nr-container = ped-venda-ext.nr-container NO-LOCK NO-ERROR.

    IF pp-container.situacao <> tt-param.cod-sit-ctner THEN NEXT.

    FIND ITEM WHERE
         ITEM.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.

    ASSIGN c-desc-item = ''.
    IF AVAIL ITEM THEN
       ASSIGN c-desc-item = ITEM.desc-item.

    ASSIGN c-moeda = IF ped-venda.mo-codigo = 0
                     THEN 'R$' ELSE 'US$'
           de-vl-item = IF ped-venda.mo-codigo = 0
                        THEN ped-item.vl-preuni
                        ELSE ped-item.vl-preuni * tt-param.us-dolar
           de-tot-item = ped-item.qt-pedida * ped-item.vl-preuni
           de-tot-rep = de-tot-rep + de-tot-item.

    CREATE tt-pedidos.
    ASSIGN tt-pedidos.nr-container = ped-venda-ext.nr-container   
           tt-pedidos.nr-pedcli = ped-venda.nr-pedcli   
           tt-pedidos.nome-abrev = ped-venda.nome-abrev  
           tt-pedidos.dt-implant = ped-venda.dt-implant
           tt-pedidos.no-ab-reppri = ped-venda.no-ab-reppri
           tt-pedidos.it-codigo = ped-item.it-codigo
           tt-pedidos.desc-item = c-desc-item
           tt-pedidos.cod-refer = ped-item.cod-refer
           tt-pedidos.qt-pedida = ped-item.qt-pedida    
           tt-pedidos.moeda = c-moeda
           tt-pedidos.vl-item = de-vl-item    
           tt-pedidos.tot-item = de-tot-item.

    FIND pp-it-container WHERE
         pp-it-container.nr-container   = ped-venda-ext.nr-container AND
         pp-it-container.it-comprado    = ped-item.it-codigo AND
         pp-it-container.ref-comprada   = ped-item.cod-refer 
         NO-LOCK NO-ERROR.

    FIND tt-resumo WHERE
         tt-resumo.nr-container = ped-venda-ext.nr-container AND
         tt-resumo.it-codigo = ped-item.it-codigo NO-ERROR.

    IF NOT AVAIL tt-resumo THEN DO.
       CREATE tt-resumo.
       ASSIGN tt-resumo.nr-container = ped-venda-ext.nr-container
              tt-resumo.it-codigo = ped-item.it-codigo
              tt-resumo.desc-item  = c-desc-item.
    END.
    ASSIGN tt-resumo.qt-vendida  = tt-resumo.qt-vendida + ped-item.qt-pedida
           tt-resumo.vl-tot-ven = tt-resumo.vl-tot-ven + de-tot-item.    .

    ASSIGN tt-resumo.qt-comprada = tt-resumo.qt-comprada + pp-it-container.qt-pedida.
END.

IF tt-param.destino = 4 THEN
   RUN pi-excel.
ELSE
   RUN pi-imprime.

RUN pi-finalizar in h-acomp.


/************************** PROCEDURES ******************************/

PROCEDURE pi-imprime.
    IF tt-param.resumido THEN DO.

    END.
    ELSE DO.
        FOR EACH tt-pedidos BREAK BY tt-pedidos.nr-pedcli.
            ACCUMULATE tt-pedidos.qt-pedida (TOTAL BY tt-pedidos.nr-pedcli).
            ACCUMULATE tt-pedidos.tot-item (TOTAL BY tt-pedidos.nr-pedcli).
    
            ACCUMULATE tt-pedidos.qt-pedida.
            ACCUMULATE tt-pedidos.tot-item.
    
    
            IF FIRST-OF(tt-pedidos.nr-pedcli) THEN
               DISP tt-pedidos.nr-container
                    tt-pedidos.nr-pedcli   
                    tt-pedidos.dt-implant
                    tt-pedidos.nome-abrev
                    tt-pedidos.no-ab-reppri
                    WITH FRAME f-detalhe.
    
            DISP tt-pedidos.it-codigo
                 tt-pedidos.desc-item
                 tt-pedidos.cod-refer
                 tt-pedidos.qt-pedida    
                 tt-pedidos.moeda
                 tt-pedidos.vl-item    
                 tt-pedidos.tot-item
                 WITH FRAME f-detalhe.
            DOWN WITH FRAME f-detalhe.
    
            IF LAST-OF(tt-pedidos.nr-pedcli) THEN DO.
               DISP FILL("-",10) FORMAT "x(10)" @ tt-pedidos.qt-pedida
                    FILL("-",13) FORMAT "x(13)" @ tt-pedidos.tot-item
                    WITH FRAME f-detalhe.
               DOWN WITH FRAME f-detalhe.
    
               DISP (ACCUM TOTAL BY tt-pedidos.nr-pedcli tt-pedidos.qt-pedida) @ tt-pedidos.qt-pedida
                    (ACCUM TOTAL BY tt-pedidos.nr-pedcli tt-pedidos.tot-item) @ tt-pedidos.tot-item
                    WITH FRAME f-detalhe.
               DOWN 2 WITH FRAME f-detalhe.
            END.
        END.
    
        DOWN 1 WITH FRAME f-detalhe.
    
        DISP FILL("-",10) FORMAT "x(10)" @ tt-pedidos.qt-pedida
             FILL("-",13) FORMAT "x(13)" @ tt-pedidos.tot-item
             WITH FRAME f-detalhe.
        DOWN WITH FRAME f-detalhe.
    
        DISP "T O T A L    G E R A L" @ tt-pedidos.desc-item
             (ACCUM TOTAL tt-pedidos.qt-pedida) @ tt-pedidos.qt-pedida
             (ACCUM TOTAL tt-pedidos.tot-item) @ tt-pedidos.tot-item
             WITH FRAME f-detalhe.
        DOWN 2 WITH FRAME f-detalhe.
    END.
END PROCEDURE.


PROCEDURE pi-excel.
    DEF VAR chExcel  AS COM-HANDLE NO-UNDO.
    DEF VAR chbook  AS COM-HANDLE NO-UNDO.
    DEF VAR chsheet AS COM-HANDLE NO-UNDO.
    DEF VAR i-lin AS INT.
    DEF VAR c-arq-excel AS CHAR.

    ASSIGN c-arq-excel = SESSION:TEMP-DIRECTORY + "PI.XLS".

    /*** Cria a Inst³ncia do Excel ***/
    CREATE 'excel.application' chexcel.
    
    ASSIGN chexcel:VISIBLE = FALSE /* FALSE = N’o Mostra - TRUE = Mostra Excel */
           chexcel:displayalerts  = FALSE /* FALSE = N’o Mostra Mensagens de Alerta   */
           chbook = chexcel:workbooks:ADD()
           chsheet = chbook:worksheets:ITEM(1).

    IF tt-param.resumido THEN DO.
        ASSIGN i-lin = 1. 
    
        ASSIGN chsheet:cells(1,1):VALUE = "Container".
        ASSIGN chsheet:cells(1,2):VALUE = "Item".     
        ASSIGN chsheet:cells(1,3):VALUE = "Descri‡Æo".
        ASSIGN chsheet:cells(1,4):VALUE = "Qtde Total Comprada".    
        ASSIGN chsheet:cells(1,5):VALUE = "Qtde Total Vendida".    
        ASSIGN chsheet:cells(1,6):VALUE = "Vlr Total Vendido".
        ASSIGN chsheet:cells(1,7):VALUE = "Qtde Total Disponivel".    


        ASSIGN chsheet:range("1:" + STRING(i-lin)):FONT:ColorIndex = 5
               chsheet:range("1:" + STRING(i-lin)):FONT:NAME = "Courrier New"
               chsheet:range("1:" + STRING(i-lin)):FONT:bold = TRUE
               chsheet:range("1:" + STRING(i-lin)):FONT:SIZE = 10.

        FOR EACH tt-resumo BREAK BY tt-resumo.it-codigo.
            ASSIGN tt-resumo.qt-disp = tt-resumo.qt-comprada - tt-resumo.qt-vendida.

            RUN pi-acompanhar IN h-acomp (INPUT "Item: " + tt-resumo.it-codigo).

            ASSIGN i-lin = i-lin + 1.
            
            ASSIGN chsheet:cells(i-lin,1):VALUE = tt-resumo.nr-container
                   chsheet:cells(i-lin,2):VALUE = tt-resumo.it-codigo
                   chsheet:cells(i-lin,3):VALUE = tt-resumo.desc-item
                   chsheet:cells(i-lin,4):VALUE = tt-resumo.qt-comprada
                   chsheet:cells(i-lin,5):VALUE = tt-resumo.qt-vendida
                   chsheet:cells(i-lin,6):VALUE = tt-resumo.vl-tot-ven
                   chsheet:cells(i-lin,7):VALUE = tt-resumo.qt-disp.
        END.
    END.
    ELSE DO.
        chexcel:range("G:G"):SELECT.
        ASSIGN chexcel:SELECTION:NumberFormat   = "@".    /* Formato da c²lula - @ = Texto */
        chsheet:Cells:SELECT. 
        
        ASSIGN i-lin = 1. 
    
        ASSIGN chsheet:cells(1,1):VALUE = "Container".
        ASSIGN chsheet:cells(1,2):VALUE = "Pedido".   
        ASSIGN chsheet:cells(1,3):VALUE = "Cliente".  
        ASSIGN chsheet:cells(1,4):VALUE = "Repres".   
        ASSIGN chsheet:cells(1,5):VALUE = "Item".     
        ASSIGN chsheet:cells(1,6):VALUE = "Descri‡Æo".
        ASSIGN chsheet:cells(1,7):VALUE = "Ref".      
        ASSIGN chsheet:cells(1,8):VALUE = "Quant".    
        ASSIGN chsheet:cells(1,9):VALUE = "Moeda". 
        ASSIGN chsheet:cells(1,10):VALUE = "Valor".    
        ASSIGN chsheet:cells(1,11):VALUE = "Total Item". 
    
        ASSIGN chsheet:range("1:" + STRING(i-lin)):FONT:ColorIndex = 5
               chsheet:range("1:" + STRING(i-lin)):FONT:NAME = "Courrier New"
               chsheet:range("1:" + STRING(i-lin)):FONT:bold = TRUE
               chsheet:range("1:" + STRING(i-lin)):FONT:SIZE = 10.
    
        FOR EACH tt-pedidos BREAK BY tt-pedidos.nr-pedcli.
    
            RUN pi-acompanhar IN h-acomp (INPUT "Pedido: " + tt-pedidos.nr-pedcli).
    
            ACCUMULATE tt-pedidos.qt-pedida (TOTAL BY tt-pedidos.nr-pedcli).
            ACCUMULATE tt-pedidos.tot-item (TOTAL BY tt-pedidos.nr-pedcli).
    
            ACCUMULATE tt-pedidos.qt-pedida.
            ACCUMULATE tt-pedidos.tot-item.
    
            ASSIGN i-lin = i-lin + 1.
    
            IF FIRST-OF(tt-pedidos.nr-pedcli) THEN
               ASSIGN chsheet:cells(i-lin,1):VALUE = tt-pedidos.nr-container
                      chsheet:cells(i-lin,2):VALUE = tt-pedidos.nr-pedcli
                      chsheet:cells(i-lin,3):VALUE = tt-pedidos.nome-abrev
                      chsheet:cells(i-lin,4):VALUE = tt-pedidos.no-ab-reppri.
    
            ASSIGN chsheet:cells(i-lin,5):VALUE = tt-pedidos.it-codigo
                   chsheet:cells(i-lin,6):VALUE = tt-pedidos.desc-item
                   chsheet:cells(i-lin,7):VALUE = tt-pedidos.cod-refer
                   chsheet:cells(i-lin,8):VALUE = tt-pedidos.qt-pedida
                   chsheet:cells(i-lin,9):VALUE = tt-pedidos.moeda
                   chsheet:cells(i-lin,10):VALUE = tt-pedidos.vl-item
                   chsheet:cells(i-lin,11):VALUE = tt-pedidos.tot-item.
    
    
            IF LAST-OF(tt-pedidos.nr-pedcli) THEN DO.
               ASSIGN i-lin = i-lin + 1.
    
               ASSIGN chsheet:cells(i-lin,6):VALUE = "Total do Pedido"
                      chsheet:cells(i-lin,8):VALUE = (ACCUM TOTAL BY tt-pedidos.nr-pedcli tt-pedidos.qt-pedida)
                      chsheet:cells(i-lin,11):VALUE = (ACCUM TOTAL BY tt-pedidos.nr-pedcli tt-pedidos.tot-item).
    
               ASSIGN chsheet:range(STRING(i-lin) + ":" + STRING(i-lin)):FONT:ColorIndex = 3
                      chsheet:range(STRING(i-lin) + ":" + STRING(i-lin)):FONT:NAME = "Courrier New"
                      chsheet:range(STRING(i-lin) + ":" + STRING(i-lin)):FONT:bold = TRUE
                      chsheet:range(STRING(i-lin) + ":" + STRING(i-lin)):FONT:SIZE = 10.
               ASSIGN i-lin = i-lin + 1.
            END.
        END.
    
        ASSIGN i-lin = i-lin + 2.
                                        
        ASSIGN chsheet:cells(i-lin,6):VALUE = "T O T A L    G E R A L"
               chsheet:cells(i-lin,8):VALUE = (ACCUM TOTAL tt-pedidos.qt-pedida)
               chsheet:cells(i-lin,11):VALUE = (ACCUM TOTAL tt-pedidos.tot-item).
    
    
        ASSIGN chsheet:range(STRING(i-lin) + ":" + STRING(i-lin)):FONT:ColorIndex = 3
               chsheet:range(STRING(i-lin) + ":" + STRING(i-lin)):FONT:NAME = "Courrier New"
               chsheet:range(STRING(i-lin) + ":" + STRING(i-lin)):FONT:bold = TRUE
               chsheet:range(STRING(i-lin) + ":" + STRING(i-lin)):FONT:SIZE = 10.
    
    END.
    ASSIGN chexcel:VISIBLE = TRUE /* FALSE = N’o Mostra - TRUE = Mostra Excel*/ .
    ChBook:Saveas(c-arq-excel, 1,'','',FALSE,FALSE,FALSE). /* Salvar o arquivo no format XLS */
    
    RELEASE OBJECT chsheet NO-ERROR.
    RELEASE OBJECT chexcel NO-ERROR.
    RELEASE OBJECT chbook  NO-ERROR.
    
END PROCEDURE.



