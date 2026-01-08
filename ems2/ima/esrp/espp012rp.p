/* Programa de controle de versao e seguranªa do Datasul EMS */
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
    FIELD dt-chegada-ini   AS DATE
    FIELD dt-chegada-fim   AS DATE
    FIELD it-codigo-ini    AS CHAR
    FIELD it-codigo-fim    AS CHAR
    FIELD cod-refer-ini    AS CHAR
    FIELD cod-refer-fim    AS CHAR
    FIELD no-ab-reppri-ini AS CHAR
    FIELD no-ab-reppri-fim AS CHAR
    FIELD qt-dias          AS INT
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

DEFINE TEMP-TABLE tt-notas
       FIELD nr-container LIKE pp-container.nr-container
       FIELD nr-nota-fis  LIKE nota-fiscal.nr-nota-fis
       FIELD dt-emissao   LIKE nota-fiscal.dt-emis
       FIELD nome-abrev   LIKE nota-fiscal.nome-abrev  
       FIELD no-ab-reppri LIKE nota-fiscal.no-ab-reppri
       FIELD it-codigo    LIKE it-nota-fisc.it-codigo
       FIELD desc-item    LIKE ITEM.desc-item
       FIELD classif      LIKE fam-comerc.descricao
       FIELD cod-refer    LIKE it-nota-fisc.cod-refer
       FIELD qt-faturada  LIKE it-nota-fisc.qt-faturada[1]    
       FIELD vl-tot-item  LIKE it-nota-fisc.vl-tot-item.

DEFINE TEMP-TABLE tt-resumo
       FIELD nr-container LIKE pp-container.nr-container
       FIELD it-codigo    LIKE it-nota-fisc.it-codigo
       FIELD desc-item    LIKE ITEM.desc-item
       FIELD classif      LIKE fam-comerc.descricao
       FIELD qt-comprada  AS DEC 
       FIELD qt-faturada  AS DEC EXTENT 4
       FIELD vl-faturado  AS DEC EXTENT 4
       FIELD perc-fat     AS DEC EXTENT 4.

DEF VAR h-acomp      AS HANDLE  NO-UNDO.
DEF VAR c-desc-item  AS CHAR.
DEF VAR de-tot-item  AS DEC.
DEF VAR de-tot-rep   AS DEC.
DEF VAR de-vl-item   AS DEC.
DEF VAR c-moeda      AS CHAR.
DEF VAR i-dias-fat   AS INT.

{include/i-rpvar.i}   

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

FOR EACH tt-raw-digita:
    CREATE tt-digita.
    RAW-TRANSFER tt-raw-digita.raw-digita TO tt-digita.
END.

FORM
   tt-notas.nr-container  COLUMN-LABEL "Container"
   tt-notas.nr-nota-fis   COLUMN-LABEL "Nota Fiscal"     FORMAT "x(7)"
   tt-notas.nome-abrev    COLUMN-LABEL "Cliente"       
   tt-notas.no-ab-reppri  COLUMN-LABEL "Repres"          FORMAT "x(12)" 
   tt-notas.dt-emis       COLUMN-LABEL "Data EmissÉo"   
   tt-notas.it-codigo     COLUMN-LABEL "Item"            FORMAT "x(8)"
   tt-notas.desc-item     COLUMN-LABEL "Descriá∆o"       FORMAT "x(32)"
   tt-notas.classif       COLUMN-LABEL "Classificaá∆o"   FORMAT "x(20)"
   tt-notas.cod-refer     COLUMN-LABEL "Ref"             FORMAT "x(5)"
   tt-notas.qt-faturada   COLUMN-LABEL "Quant"           FORMAT ">>>,>>9.99" 
   tt-notas.vl-tot-item   COLUMN-LABEL "Valor"           FORMAT ">>>,>>>,>>9.99" 
   WITH WIDTH 152 55 DOWN STREAM-IO FRAME f-detalhe.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Processando..").

/* ABERTURA DO ARQUIVO DE SA÷DA (ARQUIVO/IMPRESSORA) CORREPONDE A INCLUDE CDP/CD9520.I (MAGNUS) */
{include/i-rpout.i}
{include/i-rpcab.i}

/* LOCALIZACAO DO NOME DA EMPRESA PARA COMPOR O CABECALHO PADRAO */
FIND empresa NO-LOCK WHERE empresa.ep-codigo = i-ep-codigo-usuario NO-ERROR.
IF NOT AVAIL empresa THEN
   RETURN "ADM-ERROR":U.

/* VALORIZACAO DAS OUTRAS VARIAVEIS QUE COMPOEM O CABECALHO PADRAO */
ASSIGN c-programa     = "ESPP012.W":U
       c-versao       = "2.04":U
       c-revisao      = ".00.001":U
       c-empresa      = empresa.razao-social
       c-sistema      = "EMS":U.

ASSIGN c-titulo-relat = "RELATORIO DE FATURAMENTO":U. 

VIEW FRAME f-cabec.
VIEW FRAME f-rodape.

FOR EACH pp-container WHERE
         pp-container.nr-container    >= tt-param.nr-container-ini AND
         pp-container.nr-container    <= tt-param.nr-container-fim AND 
         pp-container.dt-prev-chegada >= tt-param.dt-chegada-ini AND 
         pp-container.dt-prev-chegada <= tt-param.dt-chegada-fim NO-LOCK
         BY pp-container.dt-prev-chegada.

    FOR EACH pp-it-container OF pp-container WHERE
             pp-it-container.it-codigo >= tt-param.it-codigo-ini AND
             pp-it-container.it-codigo <= tt-param.it-codigo-fim AND 
             pp-it-container.cod-refer >= tt-param.cod-refer-ini AND 
             pp-it-container.cod-refer <= tt-param.cod-refer-fim NO-LOCK
             BREAK BY pp-it-container.it-codigo.
    
        IF FIRST-OF(pp-it-container.it-codigo) THEN DO.
            RUN pi-acompanhar IN h-acomp (INPUT "Container:" + STRING(pp-container.nr-container) + " Chegada " + STRING(pp-container.dt-prev-chegada) + " Item: " + pp-it-container.it-codigo).
        
            FIND item WHERE
                 item.it-codigo = pp-it-container.it-codigo NO-LOCK NO-ERROR.
        
            FIND fam-comerc WHERE
                 fam-comerc.fm-cod-com = item.fm-cod-com NO-LOCK NO-ERROR.

            FIND tt-resumo WHERE
                 tt-resumo.nr-container = pp-container.nr-container AND
                 tt-resumo.it-codigo = pp-it-container.it-codigo NO-ERROR.
        
            IF NOT AVAIL tt-resumo THEN DO.
               CREATE tt-resumo.
               ASSIGN tt-resumo.nr-container = pp-container.nr-container
                      tt-resumo.it-codigo = pp-it-container.it-codigo
                      tt-resumo.desc-item = ITEM.desc-item
                      tt-resumo.classif =  IF AVAIL fam-comerc
                                           THEN fam-comerc.descricao
                                           ELSE "Indefinida".
            END.
        
            FOR EACH nota-fiscal WHERE
                     nota-fiscal.cod-estabel   = tt-param.cod-estabel AND
                     nota-fiscal.dt-emis      >= pp-container.dt-prev-chegada AND
                     nota-fiscal.dt-emis      <= pp-container.dt-prev-chegada + tt-param.qt-dias AND
                     nota-fiscal.no-ab-reppri >= tt-param.no-ab-reppri-ini AND
                     nota-fiscal.no-ab-reppri <= tt-param.no-ab-reppri-fim NO-LOCK,
                EACH it-nota-fisc OF nota-fiscal WHERE
                     it-nota-fisc.it-codigo = pp-it-container.it-codigo  NO-LOCK.
        
                IF nota-fiscal.dt-cancela <> ? THEN NEXT.
        
                FIND natur-oper WHERE
                     natur-oper.nat-operacao = nota-fiscal.nat-operacao NO-LOCK NO-ERROR.
                IF NOT AVAIL natur-oper THEN NEXT.
                IF natur-oper.tipo = 1 THEN NEXT. /* Movto de Entr */

                /* Venda entre Estabelecimentos */
                FIND estabelec WHERE
                     estabelec.cgc = nota-fiscal.cgc NO-LOCK NO-ERROR.
                IF AVAIL estabelec THEN NEXT.

                FIND ped-venda WHERE
                     ped-venda.nome-abrev = nota-fiscal.nome-ab-cli AND
                     ped-venda.nr-pedcli = nota-fiscal.nr-pedcli
                     NO-LOCK NO-ERROR.
                IF NOT AVAIL ped-venda THEN NEXT.

                FIND tt-notas WHERE
                     tt-notas.nr-container = pp-container.nr-container AND
                     tt-notas.nr-nota-fis = nota-fiscal.nr-nota-fis AND
                     tt-notas.it-codigo = it-nota-fisc.it-codigo NO-ERROR.
                IF NOT AVAIL tt-notas THEN DO.
                   CREATE tt-notas.
                   ASSIGN tt-notas.nr-container = pp-container.nr-container
                          tt-notas.nr-nota-fis = nota-fiscal.nr-nota-fis
                          tt-notas.nome-abrev = nota-fiscal.nome-ab-cli
                          tt-notas.dt-emis = nota-fiscal.dt-emis
                          tt-notas.no-ab-reppri = nota-fiscal.no-ab-reppri
                          tt-notas.it-codigo = it-nota-fisc.it-codigo
                          tt-notas.desc-item = c-desc-item.
                          tt-notas.classif = IF AVAIL fam-comerc
                                             THEN fam-comerc.descricao
                                             ELSE "Indefinida".
                END.
                ASSIGN tt-notas.qt-faturada = tt-notas.qt-faturada + it-nota-fisc.qt-faturada[1]
                       tt-notas.vl-tot-item = tt-notas.vl-tot-item + It-nota-fisc.vl-tot-item.
        
                ASSIGN i-dias-fat = nota-fiscal.dt-emis - pp-container.dt-prev-chegada.
        
                IF i-dias-fat < 30 THEN
                   ASSIGN tt-resumo.qt-faturada[1] = tt-resumo.qt-faturada[1] + it-nota-fisc.qt-faturada[1]
                          tt-resumo.vl-faturado[1] = tt-resumo.vl-faturado[1] + it-nota-fisc.vl-tot-item.
                ELSE IF i-dias-fat < 60 THEN
                    ASSIGN tt-resumo.qt-faturada[2] = tt-resumo.qt-faturada[2] + it-nota-fisc.qt-faturada[1]
                           tt-resumo.vl-faturado[2] = tt-resumo.vl-faturado[2] + it-nota-fisc.vl-tot-item.
                ELSE IF i-dias-fat < 90 THEN
                    ASSIGN tt-resumo.qt-faturada[3] = tt-resumo.qt-faturada[3] + it-nota-fisc.qt-faturada[1]
                           tt-resumo.vl-faturado[3] = tt-resumo.vl-faturado[3] + it-nota-fisc.vl-tot-item.
                ELSE 
                    ASSIGN tt-resumo.qt-faturada[4] = tt-resumo.qt-faturada[4] + it-nota-fisc.qt-faturada[1]
                           tt-resumo.vl-faturado[4] = tt-resumo.vl-faturado[4] + it-nota-fisc.vl-tot-item.
            END.
        
            ASSIGN tt-resumo.perc-fat[1] = tt-resumo.qt-faturada[1] / tt-resumo.qt-comprada * 100
                   tt-resumo.perc-fat[2] = tt-resumo.qt-faturada[2] / tt-resumo.qt-comprada * 100
                   tt-resumo.perc-fat[3] = tt-resumo.qt-faturada[3] / tt-resumo.qt-comprada * 100
                   tt-resumo.perc-fat[4] = tt-resumo.qt-faturada[4] / tt-resumo.qt-comprada * 100.
        END.

        ASSIGN tt-resumo.qt-comprada = tt-resumo.qt-comprada + pp-it-container.qt-pedida.
    END.
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
        FOR EACH tt-notas BREAK BY tt-notas.nr-nota-fis.
            ACCUMULATE tt-notas.qt-faturada (TOTAL BY tt-notas.nr-nota-fis).
            ACCUMULATE tt-notas.vl-tot-item (TOTAL BY tt-notas.nr-nota-fis).
    
            ACCUMULATE tt-notas.qt-faturada.
            ACCUMULATE tt-notas.vl-tot-item.
    
            IF FIRST-OF(tt-notas.nr-nota-fis) THEN
               DISP tt-notas.nr-container
                    tt-notas.nr-nota-fis   
                    tt-notas.dt-emis
                    tt-notas.nome-abrev
                    tt-notas.no-ab-reppri
                    WITH FRAME f-detalhe.
    
            DISP tt-notas.it-codigo
                 tt-notas.desc-item
                 tt-notas.classif
                 tt-notas.cod-refer
                 tt-notas.qt-faturada    
                 tt-notas.vl-tot-item
                 WITH FRAME f-detalhe.
            DOWN WITH FRAME f-detalhe.
    
            IF LAST-OF(tt-notas.nr-nota-fis) THEN DO.
               DISP FILL("-",10) FORMAT "x(10)" @ tt-notas.qt-faturada
                    FILL("-",13) FORMAT "x(13)" @ tt-notas.vl-tot-item
                    WITH FRAME f-detalhe.
               DOWN WITH FRAME f-detalhe.
    
               DISP (ACCUM TOTAL BY tt-notas.nr-nota-fis tt-notas.qt-faturada) @ tt-notas.qt-faturada
                    (ACCUM TOTAL BY tt-notas.nr-nota-fis tt-notas.vl-tot-item) @ tt-notas.vl-tot-item
                    WITH FRAME f-detalhe.
               DOWN 2 WITH FRAME f-detalhe.
            END.
        END.
    
        DOWN 1 WITH FRAME f-detalhe.
    
        DISP FILL("-",10) FORMAT "x(10)" @ tt-notas.qt-faturada
             FILL("-",13) FORMAT "x(13)" @ tt-notas.vl-tot-item
             WITH FRAME f-detalhe.
        DOWN WITH FRAME f-detalhe.
    
        DISP "T O T A L    G E R A L" @ tt-notas.desc-item
             (ACCUM TOTAL tt-notas.qt-faturada) @ tt-notas.qt-faturada
             (ACCUM TOTAL tt-notas.vl-tot-item) @ tt-notas.vl-tot-item
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

    ASSIGN c-arq-excel = SESSION:TEMP-DIRECTORY + "MEDIA-FAT-CONTAINER.XLS".

    /*** Cria a Inst≥ncia do Excel ***/
    CREATE 'excel.application' chexcel.
    
    ASSIGN chexcel:VISIBLE = FALSE /* FALSE = Nío Mostra - TRUE = Mostra Excel */
           chexcel:displayalerts  = FALSE /* FALSE = Nío Mostra Mensagens de Alerta   */
           chbook = chexcel:workbooks:ADD()
           chsheet = chbook:worksheets:ITEM(1).

    IF tt-param.resumido THEN DO.
        ASSIGN i-lin = 1. 
    
        ASSIGN chsheet:cells(1,1):VALUE = "Container".
        ASSIGN chsheet:cells(1,2):VALUE = "Item".     
        ASSIGN chsheet:cells(1,3):VALUE = "Descriá∆o".
        ASSIGN chsheet:cells(1,4):VALUE = "Classificaá∆o".
        ASSIGN chsheet:cells(1,5):VALUE = "Qtde Total Comprada".    
        
        ASSIGN chsheet:cells(1,6):VALUE = "Qtde Faturada < 30d".    
        ASSIGN chsheet:cells(1,7):VALUE = "Valor Faturado < 30d".    
        ASSIGN chsheet:cells(1,8):VALUE = "%Fat < 30d".    

        ASSIGN chsheet:cells(1,9):VALUE = "Qtde Faturada < 60d".    
        ASSIGN chsheet:cells(1,10):VALUE = "Valor Faturado < 60d".    
        ASSIGN chsheet:cells(1,11):VALUE = "%Fat < 60d".    
        
        ASSIGN chsheet:cells(1,12):VALUE = "Qtde Faturada < 90d".    
        ASSIGN chsheet:cells(1,13):VALUE = "Valor Faturado < 90d".    
        ASSIGN chsheet:cells(1,14):VALUE = "%Fat < 90d".    
        
        ASSIGN chsheet:cells(1,15):VALUE = "Qtde Faturada > 90d".    
        ASSIGN chsheet:cells(1,16):VALUE = "Valor Faturado > 90d".    
        ASSIGN chsheet:cells(1,17):VALUE = "%Fat > 90d".    

        ASSIGN chsheet:range("1:" + STRING(i-lin)):FONT:ColorIndex = 5
               chsheet:range("1:" + STRING(i-lin)):FONT:NAME = "Courrier New"
               chsheet:range("1:" + STRING(i-lin)):FONT:bold = TRUE
               chsheet:range("1:" + STRING(i-lin)):FONT:SIZE = 10.

        FOR EACH tt-resumo BREAK BY tt-resumo.it-codigo.
            ASSIGN i-lin = i-lin + 1.
            
            ASSIGN chsheet:cells(i-lin,1):VALUE = tt-resumo.nr-container
                   chsheet:cells(i-lin,2):VALUE = tt-resumo.it-codigo
                   chsheet:cells(i-lin,3):VALUE = tt-resumo.desc-item
                   chsheet:cells(i-lin,4):VALUE = tt-resumo.classif
                   chsheet:cells(i-lin,5):VALUE = tt-resumo.qt-comprada

                   chsheet:cells(i-lin,6):VALUE = tt-resumo.qt-faturada[1]
                   chsheet:cells(i-lin,7):VALUE = tt-resumo.vl-faturado[1]
                   chsheet:cells(i-lin,8):VALUE = tt-resumo.perc-fat[1]

                   chsheet:cells(i-lin,9):VALUE = tt-resumo.qt-faturada[2]
                   chsheet:cells(i-lin,10):VALUE = tt-resumo.vl-faturado[2]
                   chsheet:cells(i-lin,11):VALUE = tt-resumo.perc-fat[2]
        
                   chsheet:cells(i-lin,12):VALUE = tt-resumo.qt-faturada[3]
                   chsheet:cells(i-lin,13):VALUE = tt-resumo.vl-faturado[3]
                   chsheet:cells(i-lin,14):VALUE = tt-resumo.perc-fat[3]
        
                   chsheet:cells(i-lin,15):VALUE = tt-resumo.qt-faturada[4]
                   chsheet:cells(i-lin,16):VALUE = tt-resumo.vl-faturado[4]
                   chsheet:cells(i-lin,17):VALUE = tt-resumo.perc-fat[4].

            chSheet:range("E" + STRING(i-lin) + ":G" + STRING(i-lin)):NumberFormat = "#.###.##0,00".
            chSheet:range("H" + STRING(i-lin) + ":H" + STRING(i-lin)):NumberFormat = "##0,00".
            chSheet:range("I" + STRING(i-lin) + ":J" + STRING(i-lin)):NumberFormat = "#.###.##0,00".
            chSheet:range("K" + STRING(i-lin) + ":K" + STRING(i-lin)):NumberFormat = "##0,00".
            chSheet:range("L" + STRING(i-lin) + ":M" + STRING(i-lin)):NumberFormat = "#.###.##0,00".
            chSheet:range("N" + STRING(i-lin) + ":N" + STRING(i-lin)):NumberFormat = "##0,00".
        END.
    END.
    ELSE DO.
        chexcel:range("G:G"):SELECT.
        ASSIGN chexcel:SELECTION:NumberFormat   = "@".    /* Formato da c≤lula - @ = Texto */
        chsheet:Cells:SELECT. 
        
        ASSIGN i-lin = 1. 
    
        ASSIGN chsheet:cells(1,1):VALUE = "Container".
        ASSIGN chsheet:cells(1,2):VALUE = "Nota Fiscal".   
        ASSIGN chsheet:cells(1,3):VALUE = "Emiss∆o".   
        ASSIGN chsheet:cells(1,4):VALUE = "Cliente".  
        ASSIGN chsheet:cells(1,5):VALUE = "Repres".   
        ASSIGN chsheet:cells(1,6):VALUE = "Item".     
        ASSIGN chsheet:cells(1,7):VALUE = "Descriá∆o".
        ASSIGN chsheet:cells(1,8):VALUE = "Classif".
        ASSIGN chsheet:cells(1,9):VALUE = "Qt Faturada".    
        ASSIGN chsheet:cells(1,10):VALUE = "Valor Total Item".    
    
        ASSIGN chsheet:range("1:" + STRING(i-lin)):FONT:ColorIndex = 5
               chsheet:range("1:" + STRING(i-lin)):FONT:NAME = "Courrier New"
               chsheet:range("1:" + STRING(i-lin)):FONT:bold = TRUE
               chsheet:range("1:" + STRING(i-lin)):FONT:SIZE = 10.
    
        FOR EACH tt-notas BREAK BY tt-notas.nr-nota-fis.
    
            ACCUMULATE tt-notas.qt-faturada (TOTAL BY tt-notas.nr-nota-fis).
            ACCUMULATE tt-notas.vl-tot-item (TOTAL BY tt-notas.nr-nota-fis).
    
            ACCUMULATE tt-notas.qt-faturada.
            ACCUMULATE tt-notas.vl-tot-item.
    
            ASSIGN i-lin = i-lin + 1.
    
            IF FIRST-OF(tt-notas.nr-nota-fis) THEN
               ASSIGN chsheet:cells(i-lin,1):VALUE = tt-notas.nr-container
                      chsheet:cells(i-lin,2):VALUE = tt-notas.nr-nota-fis
                      chsheet:cells(i-lin,3):VALUE = tt-notas.dt-emis
                      chsheet:cells(i-lin,4):VALUE = tt-notas.nome-abrev
                      chsheet:cells(i-lin,5):VALUE = tt-notas.no-ab-reppri.
    
            ASSIGN chsheet:cells(i-lin,6):VALUE = tt-notas.it-codigo
                   chsheet:cells(i-lin,7):VALUE = tt-notas.desc-item
                   chsheet:cells(i-lin,8):VALUE = tt-notas.classif
                   chsheet:cells(i-lin,9):VALUE = tt-notas.qt-faturada
                   chsheet:cells(i-lin,10):VALUE = tt-notas.vl-tot-item.
    
            chSheet:range("I" + STRING(i-lin) + ":J" + STRING(i-lin)):NumberFormat = "#.###.##0,00".

            IF LAST-OF(tt-notas.nr-nota-fis) THEN DO.
               ASSIGN i-lin = i-lin + 1.
    
               ASSIGN chsheet:cells(i-lin,8):VALUE = "Total da Nota"
                      chsheet:cells(i-lin,9):VALUE = (ACCUM TOTAL BY tt-notas.nr-nota-fis tt-notas.qt-faturada)
                      chsheet:cells(i-lin,10):VALUE = (ACCUM TOTAL BY tt-notas.nr-nota-fis tt-notas.vl-tot-item).
    
               ASSIGN chsheet:range(STRING(i-lin) + ":" + STRING(i-lin)):FONT:ColorIndex = 3
                      chsheet:range(STRING(i-lin) + ":" + STRING(i-lin)):FONT:NAME = "Courrier New"
                      chsheet:range(STRING(i-lin) + ":" + STRING(i-lin)):FONT:bold = TRUE
                      chsheet:range(STRING(i-lin) + ":" + STRING(i-lin)):FONT:SIZE = 10.
               ASSIGN i-lin = i-lin + 1.
            END.
        END.
    
        ASSIGN i-lin = i-lin + 2.
                                        
        ASSIGN chsheet:cells(i-lin,8):VALUE = "T O T A L    G E R A L"
               chsheet:cells(i-lin,9):VALUE = (ACCUM TOTAL tt-notas.qt-faturada)
               chsheet:cells(i-lin,10):VALUE = (ACCUM TOTAL tt-notas.vl-tot-item).
    
        ASSIGN chsheet:range(STRING(i-lin) + ":" + STRING(i-lin)):FONT:ColorIndex = 3
               chsheet:range(STRING(i-lin) + ":" + STRING(i-lin)):FONT:NAME = "Courrier New"
               chsheet:range(STRING(i-lin) + ":" + STRING(i-lin)):FONT:bold = TRUE
               chsheet:range(STRING(i-lin) + ":" + STRING(i-lin)):FONT:SIZE = 10.
    END.
    ASSIGN chexcel:VISIBLE = TRUE /* FALSE = Nío Mostra - TRUE = Mostra Excel*/ .
    ChBook:Saveas(c-arq-excel, 1,'','',FALSE,FALSE,FALSE). /* Salvar o arquivo no format XLS */
    
    RELEASE OBJECT chsheet NO-ERROR.
    RELEASE OBJECT chexcel NO-ERROR.
    RELEASE OBJECT chbook  NO-ERROR.
    
END PROCEDURE.



