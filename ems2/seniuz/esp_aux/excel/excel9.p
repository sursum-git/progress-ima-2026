DEFINE VARIABLE excelAppl   AS COM-HANDLE. 
DEFINE VARIABLE excelSheet  AS COM-HANDLE.

CREATE "Excel.Application" excelAppl. 
ExcelAppl:SheetsInNewWorkbook = 1. /* N§ Planilhas a Serem Criadas */



excelAppl:VISIBLE = TRUE. 
excelAppl:WindowState = 1.  /* 1-normal   2-minimizado */ 

excelAppl:Workbooks:ADD(""). 

excelSheet=excelAppl:sheets:ITEM(1). 

excelAppl:DisplayFormulaBar = TRUE. 
excelAppl:CommandBars("Standard"):Visible = TRUE. 
excelAppl:CommandBars("Formatting"):Visible = TRUE. 

excelAppl:worksheets:ITEM(1):SELECT. 
excelAppl:range("A1"):VALUE = "TESTE". 

excelAppl:range("A1:b1"):SELECT().
excelAppl:range("A1:b1"):Merge.

excelAppl:range("A1:b1"):HorizontalAlignment = 3.

 /*   Chworksheet:range("A1:N1"):HorizontalAlignment = 4. */



/*excelSheet:NAME="EXEMPLO".*/

/*
excelAppl:worksheets:ITEM(2):SELECT. 
excelAppl:range("B1"):VALUE = STRING(TODAY + 10). 

excelAppl:worksheets:ITEM(3):SELECT. 
excelAppl:range("C1"):VALUE = STRING(TODAY + 20). 

excelAppl:worksheets:ITEM(1):SELECT. 
*/

RELEASE OBJECT excelAppl. 
RELEASE OBJECT excelSheet.

/*
chExcelApplication:Range("A1:K11"):Select.
chExcelApplication:Selection:Copy.

chExcelApplication:Range("A" + string(m-linha)):Select.
chExcelApplication:ActiveSheet:Paste.
chExcelApplication:Application:CutCopyMode = False.
*/


/*

 PROGTEG - PROGRESS TO EXCEL  

/* This program extracts data from a Progress database 
This program leaves Excel open. You should close it manually when the program completes. 
You must create a folder that named Excel under `C:` and copy the Excel.exe and *.dll files 

Program name : excel.p
Date : 21 May 2000 
Author : Cem DAGLI cem@progteg.com
*/

/* *********************** Control Definitions ********************** */
DEFINE VARIABLE rowi AS INTEGER.
DEFINE VARIABLE sys AS INTEGER.
DEFINE VARIABLE sheet1 AS INTEGER.
DEFINE VARIABLE kl AS INTEGER.
DEFINE VARIABLE itemn AS CHARACTER.
DEFINE VARIABLE sty AS CHARACTER.
DEFINE VARIABLE cl AS CHARACTER.
DEFINE VARIABLE excelon AS LOGICAL INITIAL FALSE.
DEFINE VARIABLE custno1 LIKE customer.cust-num LABEL "Cust-No>=".
DEFINE VARIABLE custno2 LIKE customer.cust-num LABEL "Cust-No<=".

DEFINE BUTTON bt_excel 
LABEL "Excel" 
SIZE 15 BY 1.05.

/* Query definitions */
DEFINE QUERY MainFrame FOR sports.customer SCROLLING.

/* ************************ Frame Definitions *********************** */

DEFINE FRAME MainFrame
custno1 AT ROW 4.79 COL 20.5 COLON-ALIGNED
custno2 AT ROW 4.79 COL 45.5 COLON-ALIGNED
Bt_Excel AT ROW 8.11 COL 37.5
"CUSTOMER INFORMATION" VIEW-AS TEXT
SIZE 20 BY .84 AT ROW 2.26 COL 25.5
FGCOLOR 4 
WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
SIDE-LABELS NO-UNDERLINE THREE-D
AT COL 1 ROW 1
SIZE 85.25 BY 15.42. 

ENABLE ALL WITH FRAME MainFrame.

PROCEDURE WinExec EXTERNAL "C:\windows\system\krnl386.exe": 
DEFINE INPUT PARAMETER prog_name AS CHARACTER. 
DEFINE INPUT PARAMETER prog_style AS SHORT. 
END PROCEDURE.

ON CHOOSE OF bt_excel IN FRAME MainFrame
DO: 

RUN WinExec (INPUT "C:\EXCEL\Excel /e", INPUT 1). /* 1=normal 2=minimized */
excelon = TRUE.

DDE INITIATE sys FRAME FRAME MainFrame:HANDLE 
APPLICATION "Excel" TOPIC "System". 
IF sys = 0 THEN
DO:
MESSAGE "Couldn't find Excel" VIEW-AS ALERT-BOX.
RETURN.
END.

DDE EXECUTE sys COMMAND "[new(1)]". 
DDE INITIATE sheet1 FRAME FRAME MainFrame:HANDLE 
APPLICATION "Excel" TOPIC "Sheet1" . 
DDE SEND sheet1 SOURCE "Cust-Num" ITEM "r1c1". 
DDE SEND sheet1 SOURCE "Name" ITEM "r1c2". 
DDE SEND sheet1 SOURCE "Country" ITEM "r1c3".

rowi = 3.
FOR EACH customer WHERE ( customer.cust-num>= INT(custno1:SCREEN-VALUE) ) AND 
            ( customer.cust-num <=INT(custno2:SCREEN-VALUE) ) 
NO-LOCK BY cust-num:
            
itemn = "R" + STRING(rowi) + "C1".
DDE SEND sheet1 SOURCE STRING(Customer.cust-num) ITEM itemn. 
itemn = "R" + STRING(rowi) + "C2".
DDE SEND sheet1 SOURCE Customer.Name ITEM itemn.
itemn = "R" + STRING(rowi) + "C3".
DDE SEND sheet1 SOURCE Customer.Country ITEM itemn. 

rowi = rowi + 1.
END.

DDE EXECUTE sheet1 COMMAND "[select(~"C2~")]". 
DDE EXECUTE sys COMMAND "[column.width(20)]". 
DDE EXECUTE sheet1 COMMAND "[select(~"C3~")]". 
DDE EXECUTE sys COMMAND "[column.width(20)]". 
DDE EXECUTE sheet1 COMMAND "[select(~"C4~")]". 
DDE EXECUTE sys COMMAND "[column.width(12)]". 
DDE EXECUTE sheet1 COMMAND "[select(~"C1~")]". 
DDE EXECUTE sys COMMAND "[column.width(3)]". 

DDE EXECUTE sys COMMAND "[app.restore()]". 
DDE EXECUTE sys COMMAND "[arrange.all()]". 

DDE TERMINATE sys.
DDE TERMINATE sheet1.

END. 

APPLY "ENTRY" TO custno1 IN FRAME MainFrame.
WAIT-FOR CHOOSE OF bt_excel.
 
 KAPAT 
 





*/



/*
define temp-table tt-param no-undo
    field destino                   as integer
    field arquivo                   as char format "x(35)"
    field usuario                   as char format "x(12)"
    field data-exec               as date
    field hora-exec               as integer
    FIELD i-encomenda-ini    AS INTEGER
    FIELD i-encomenda-fim   AS INTEGER
    FIELD d-abertura-ini        AS DATE
    FIELD d-abertura-fim      AS DATE
    FIELD l-poslin                AS LOG.

DEFINE TEMP-TABLE tt-af NO-UNDO
    FIELD ttm-abertura             LIKE  es-af.dt-abertura
    FIELD ttm-encomenda         LIKE  es-af.nr-af       
    FIELD ttm-modelo               LIKE  es-cad-tipo.modelo
    FIELD ttm-modelochass       LIKE  es-cad-modelo.modelo  
    FIELD ttm-af                       LIKE  es-af.nr-af
    FIELD ttm-ciclolinha             LIKE  es-af.ciclo_linha
    FIELD ttm-seq                     LIKE  es-ne.seq-pedido          
    FIELD ttm-ne                      LIKE  es-ne.nr-ne
    FIELD ttm-linha                   LIKE  es-cad-posicao.descricao
    FIELD ttm-ent-chassi           LIKE  es-ne.dt-ent-chassi
    FIELD ttm-ent-linha             LIKE  es-ne.dt-ent-linha
    FIELD ttm-prev-libe             LIKE  es-ne.prev-libe      
    FIELD ttm-poltrona              LIKE  es-ne.tipo-poltrona 
    FIELD ttm-lib-efet                LIKE  es-ne.liber-efet   
    FIELD ttm-saida                   LIKE  es-ne.data-saida  
    FIELD ttm-cliente                 LIKE  emitente.nome-emit
    FIELD ttm-liberacao             LIKE  ITEM.data-liberac.


define temp-table tt-digita no-undo
    field encomenda            LIKE es-af.nr-af.

define buffer b-tt-digita for tt-digita.

def temp-table tt-raw-digita
        field raw-digita      as raw.

/* defini‡Æo de vari veis  */
DEFINE VARIABLE h-acomp         AS HANDLE        NO-UNDO.
def var chExcelApplication           as com-handle    no-undo.
def var chWorkbook                    as com-handle    no-undo.
def var chworksheet                   as com-handle    no-undo.
def var m-linha                          as int           no-undo.

/* recebimento de parƒmetros */
DEFINE INPUT PARAMETER raw-param AS RAW NO-UNDO. /*vari vel com conte£do tt-param*/
DEFINE INPUT PARAMETER TABLE FOR tt-raw-digita.  

/*carrega parametros*/
CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

/*carrega temp-table*/
FOR EACH tt-raw-digita:
    CREATE tt-digita.
    RAW-TRANSFER tt-raw-digita.raw-digita TO tt-digita.
END.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* executando de forma persistente o utilit rio de acompanhamento */
RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Imprimindo *}
RUN pi-inicializar IN h-acomp ('Listagem de AFs').


find first tt-digita no-lock no-error. /*se temp-table de digitacao possuir registros, filtra pela tt-digita*/
IF AVAIL tt-digita THEN DO:
    FOR EACH tt-digita,
        EACH es-af WHERE es-af.nr-af = tt-digita.encomenda
        AND es-af.dt-abertura >= tt-param.d-abertura-ini
        AND es-af.dt-abertura <= tt-param.d-abertura-fim:

        RUN relatorio.

    END. /*for each tt-digita*/
END. /*if avail tt-digita*/
ELSE DO: /*senao filtra por faixa - frame de selecao */
    FOR EACH es-af WHERE es-af.nr-af >= tt-param.i-encomenda-ini
                     AND es-af.nr-af <= tt-param.i-encomenda-fim
                     AND es-af.dt-abertura >= tt-param.d-abertura-ini
                     AND es-af.dt-abertura <= tt-param.d-abertura-fim:

        RUN relatorio.
    END. /*for each es-af*/
END. /*else do*/

IF TEMP-TABLE tt-af:HAS-RECORDS THEN
   RUN pi-excel.

run pi-finalizar in h-acomp.

PROCEDURE relatorio:

            CREATE tt-af.
            ASSIGN tt-af.ttm-abertura       = es-af.dt-abertura
                   tt-af.ttm-encomenda        = es-af.nr-af
                   tt-af.ttm-af                      = es-af.nr-af
                   tt-af.ttm-ciclolinha            = es-af.ciclo_linha.

            FIND FIRST ITEM WHERE ITEM.it-codigo = string(es-af.nr-af) NO-ERROR.
            IF AVAIL ITEM THEN DO:
                ASSIGN tt-af.ttm-liberacao = ITEM.data-liberac.
            END.
            FIND FIRST es-cad-modelo WHERE STRING (es-cad-modelo.cod-modelo) = es-af.modelo NO-ERROR.
            IF AVAIL es-cad-modelo THEN DO:
                ASSIGN tt-af.ttm-modelochass       = caps(es-cad-modelo.modelo).
            END.

            FIND FIRST es-cad-tipo WHERE es-cad-tipo.cod-tipo = es-af.tipo NO-ERROR.
            IF AVAIL es-cad-tipo THEN DO:
                ASSIGN tt-af.ttm-modelo            = caps(es-cad-tipo.modelo). 
            END.

            FIND FIRST emitente WHERE emitente.cod-emitente = es-af.cod-emitente NO-ERROR.
            IF AVAIL emitente THEN DO:
                ASSIGN tt-af.ttm-cliente    = caps(emitente.nome-abrev).
            END.

            IF tt-param.l-poslin = YES THEN DO:
                FOR EACH es-ne WHERE es-ne.nr-af-original = es-af.nr-af:
                     
                      RUN achou_NE.
                      run pi-acompanhar in h-acomp ('AFs: ' + STRING(tt-af.ttm-af)).
                END.
            END.
            ELSE DO:
                FOR EACH es-ne WHERE es-ne.nr-af-original = es-af.nr-af
                               AND es-ne.posi-linha <> 29:
                      RUN achou_NE.
                      run pi-acompanhar in h-acomp ('AFs: ' + STRING(tt-af.ttm-af)).
                END.
            END.

END PROCEDURE. /*procedure relatorio*/

PROCEDURE achou_NE:
     ASSIGN tt-af.ttm-seq        = es-ne.seq-ped-orig
            tt-af.ttm-ne               = es-ne.nr-ne
            tt-af.ttm-ent-chassi    = es-ne.dt-ent-chassi
            tt-af.ttm-ent-linha      = es-ne.dt-ent-linha
            tt-af.ttm-prev-libe     = es-ne.prev-libe
            tt-af.ttm-poltrona      = caps(es-ne.tipo-poltrona)
            tt-af.ttm-lib-efet       = es-ne.liber-efet
            tt-af.ttm-saida          = es-ne.data-saida.
            FIND FIRST es-cad-posicao WHERE es-cad-posicao.cod-posi-linha = es-ne.posi-linha NO-ERROR.
            IF AVAIL es-cad-posicao THEN DO:
                 ASSIGN tt-af.ttm-linha  = caps(es-cad-posicao.descricao).
            END.
            

END PROCEDURE.

/******
** procedure pi-excel.
******/
PROCEDURE pi-excel:
    create "Excel.Application" chExcelApplication.
    assign chWorkbook = chExcelApplication:Workbooks:add()
           chworksheet=chexcelapplicAtion:sheets:item(1)
           chworksheet:name="Relat¢rio de AF's"
           m-linha = 1.

    run pi-acompanhar in h-acomp (input "Aguarde, Gerando Excel...").

    assign chworksheet:range("A" + string(m-linha) + ":Q" + string(m-linha)):font:bold = yes
           chworksheet:range("A1"):value = "Dt.Abertura"
           chworksheet:range("B1"):value = "Encomenda"
           chworksheet:range("C1"):value = "Modelo"
           chworksheet:range("D1"):value = "Mod.Chassis"
           chworksheet:range("E1"):value = "AF"
           chworksheet:range("F1"):value = "Seq."
           chworksheet:range("G1"):value = "NE"
           chworksheet:range("H1"):value = "Linha"
           chworksheet:range("I1"):value = "Ciclo Linha"
           chworksheet:range("J1"):value = "Ent.Chassis"
           chworksheet:range("K1"):value = "Ent.Linha"
           chworksheet:range("L1"):value = "Prev.Lib."
           chworksheet:range("M1"):value = "Poltrona"
           chworksheet:range("N1"):VALUE = "Lib.Efet."
           chworksheet:range("O1"):VALUE = "Sa¡da"
           chworksheet:range("P1"):VALUE = "Libera‡Æo"
           chworksheet:range("Q1"):VALUE = "Cliente"
           m-linha = m-linha + 1.

    FOR EACH tt-af:
        assign chworksheet:range("A" + string(m-linha)):value = tt-af.ttm-abertura
               chworksheet:range("B" + string(m-linha)):value = tt-af.ttm-encomenda
               chworksheet:range("C" + string(m-linha)):value = tt-af.ttm-modelo
               chworksheet:range("D" + string(m-linha)):value = tt-af.ttm-modelochass
               chworksheet:range("E" + string(m-linha)):value = tt-af.ttm-af
               chworksheet:range("F" + string(m-linha)):value = tt-af.ttm-seq
               chworksheet:range("G" + string(m-linha)):value = tt-af.ttm-ne
               chworksheet:range("H" + string(m-linha)):value = tt-af.ttm-linha
               chworksheet:range("I" + string(m-linha)):value = tt-af.ttm-ciclolinha
               chworksheet:range("J" + string(m-linha)):value = tt-af.ttm-ent-chassi
               chworksheet:range("K" + string(m-linha)):value = tt-af.ttm-ent-linha
               chworksheet:range("L" + string(m-linha)):value = tt-af.ttm-prev-libe
               chworksheet:range("M" + string(m-linha)):value = tt-af.ttm-poltrona
               chworksheet:range("N" + STRING(m-linha)):VALUE = tt-af.ttm-lib-efet
               chworksheet:range("O" + STRING(m-linha)):VALUE = tt-af.ttm-saida
               chworksheet:range("P" + STRING(m-linha)):VALUE = tt-af.ttm-liberacao
               chworksheet:range("Q" + STRING(m-linha)):VALUE = tt-af.ttm-cliente
               m-linha = m-linha + 1.
    END.

    chExcelApplication:Range("A1"):select.
    chExcelApplication:Range("A:A"):EntireColumn:AutoFit.
    chExcelApplication:Visible = yes.
    release object chExcelApplication.



*/  
  
  
