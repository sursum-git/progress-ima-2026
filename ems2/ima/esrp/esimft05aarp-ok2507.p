/* PROGRAMA: RELAT-ALGODAORP.P                                               **
** DATA    : 09/ABRIL/2010                                                   **
** AUTOR   : Anderson Fagner                                                  **
** OBJETIVO: RELATàRIO DE ITENS 100% ALGODÇO                                 **
******************************************************************************/

/* Programa de controle de versao e seguran»a do Datasul EMS */
{include/i-prgvrs.i relat-algodaorp 2.06.00.001}

/* Definicao de variaveis globais comuns a todos os programas do EMS */
{utp/ut-glob.i}
DEF VAR h-acomp     AS HANDLE  NO-UNDO.
DEF VAR da-dt-calc  AS DATE    FORMAT "99/99/9999".

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Processando..").


DEFINE TEMP-TABLE tt-param NO-UNDO
    FIELD destino          AS INTEGER
    FIELD arquivo          AS CHAR FORMAT "x(35)"
    FIELD usuario          AS CHAR FORMAT "x(12)"
    FIELD data-exec        AS DATE
    FIELD hora-exec        AS INTEGER
    FIELD classifica       AS INTEGER
    FIELD desc-classifica  AS CHAR FORMAT "x(40)"
    FIELD modelo-rtf       AS CHAR FORMAT "x(35)"
    FIELD l-habilitaRtf    AS LOG
    FIELD fi-it-codigo-ini AS CHAR
    FIELD fi-it-codigo-fim AS CHAR
    FIELD fi-ge-codigo-ini AS INT
    FIELD fi-ge-codigo-fim AS INT
    FIELD fi-dt-trans-ini  AS DATE
    FIELD fi-dt-trans-fim  AS DATE
    FIELD tg-deposito-arm  AS log
    FIELD tg-deposito-alm  AS log
    FIELD tg-deposito-emb  AS log
    FIELD tg-deposito-sal  AS log
    FIELD tg-deposito-aim  AS LOG
    FIELD tg-deposito-log  AS log
    FIELD tg-salva-excel   AS LOG
    FIELD rs-origem        AS INT.
    
DEF TEMP-TABLE tt-item
    FIELD it-codigo   AS CHAR
    FIELD desc-item   AS CHAR
    FIELD un          AS CHAR
    FIELD c-orig      AS CHAR
    FIELD qt-ini      LIKE saldo-estoq.qtidade-atu
    FIELD qt-nota-ent LIKE saldo-estoq.qtidade-atu
    FIELD qt-nota-sai LIKE saldo-estoq.qtidade-atu
    FIELD qt-tran-ent LIKE saldo-estoq.qtidade-atu
    FIELD qt-tran-sai LIKE saldo-estoq.qtidade-atu
    FIELD qt-ajus-ent LIKE saldo-estoq.qtidade-atu
    FIELD qt-ajus-sai LIKE saldo-estoq.qtidade-atu
    FIELD qt-fim      LIKE saldo-estoq.qtidade-atu.

DEF TEMP-TABLE tt-total
    FIELD un          AS CHAR
    FIELD qt-ini      like saldo-estoq.qtidade-atu
    FIELD qt-nota-ent like saldo-estoq.qtidade-atu
    FIELD qt-nota-sai like saldo-estoq.qtidade-atu
    FIELD qt-tran-ent like saldo-estoq.qtidade-atu
    FIELD qt-tran-sai like saldo-estoq.qtidade-atu
    FIELD qt-ajus-ent like saldo-estoq.qtidade-atu
    FIELD qt-ajus-sai like saldo-estoq.qtidade-atu
    FIELD qt-fim      like saldo-estoq.qtidade-atu.

DEFINE VARIABLE var-codigo-orig-1 AS INTEGER     NO-UNDO.
DEFINE VARIABLE var-codigo-orig-2 AS INTEGER     NO-UNDO.

/*------------------- Excel-------------------------*/
DEFINE VARIABLE chExcelApp  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkBook  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkSheet AS COM-HANDLE NO-UNDO.

DEF VAR i-linha AS INTEGER NO-UNDO.
DEF VAR c-arquivo-xls AS CHARACTER  NO-UNDO.


/*---------------------------------------------------*/
DEFINE NEW SHARED TEMP-TABLE tt-digita NO-UNDO
    FIELD ordem              AS INTEGER   FORMAT ">>>>9"
    FIELD nr-pedcli          AS CHAR
    FIELD nome-abrev         AS CHAR
    FIELD l-status           AS LOG.

/* Parametros de entrada logica obrigatoria */
DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita                   AS RAW.

DEF BUFFER empresa FOR mgcad.empresa.
DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.   


CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

FOR EACH tt-raw-digita :
    CREATE tt-digita.
    RAW-TRANSFER tt-raw-digita.raw-digita TO tt-digita.
END.

{include/i-rpvar.i}   

/* ABERTURA DO ARQUIVO DE SAÖDA (ARQUIVO/IMPRESSORA) CORREPONDE A INCLUDE CDP/CD9520.I (MAGNUS) */
{include/i-rpout.i}
{include/i-rpcab.i}

FORM HEADER "Per¡odo entre"          AT 01
            tt-param.fi-dt-trans-ini AT 15 FORMAT "99/99/9999"
            "e"                      AT 26
            tt-param.fi-dt-trans-fim AT 28 FORMAT "99/99/9999"
            SKIP
            "Transferencia"         AT 86
            "Ajuste"                AT 112
            SKIP
            "Item"                  AT 02   
            "Descri‡Æo"             AT 08
            "UN"                    AT 37
            "Ori"                   AT 40
            "Qt Inicial"            AT 45   
            "Qt Entrada"            AT 56   
            "Qt Saida"              AT 69  
            "Entrada"               AT 81   
            "Saida"                 AT 94   
            "Entrada"               AT 104   
            "Saida"                 AT 116     
            "Qt Final"              AT 125
            SKIP
            FILL("-",132)           AT 01 FORMAT "x(132)" 
            WITH FRAME f-cabec. 

/* LOCALIZACAO DO NOME DA EMPRESA PARA COMPOR O CABECALHO PADRAO */
FIND empresa NO-LOCK WHERE empresa.ep-codigo = i-ep-codigo-usuario NO-ERROR.
IF NOT AVAIL empresa THEN
   RETURN "ADM-ERROR":U.

/* VALORIZACAO DAS OUTRAS VARIAVEIS QUE COMPOEM O CABECALHO PADRAO */
ASSIGN c-programa     = "relat-algodao.W":U
       c-versao       = "2.06":U
       c-revisao      = ".00.001":U
       c-empresa      = empresa.razao-social
       c-sistema      = "EMS":U.

ASSIGN c-titulo-relat = "RELATàRIO DE ENTRADAS E SAIDAS":U.

VIEW FRAME f-cabec.
VIEW FRAME f-rodape.

/******************************* Calcula Movimentos *******************************/
CASE tt-param.rs-origem:
    
    WHEN 0 THEN
        ASSIGN var-codigo-orig-1 = 1
               var-codigo-orig-2 = 3.
    WHEN 1 THEN
        ASSIGN var-codigo-orig-1 = 0
               var-codigo-orig-2 = 0.
    OTHERWISE
        ASSIGN var-codigo-orig-1 = 3
               var-codigo-orig-2 = 3.

END CASE.

FIND FIRST para-ped NO-LOCK NO-ERROR.

FOR EACH item WHERE 
         item.it-codigo   >= tt-param.fi-it-codigo-ini  AND 
         item.it-codigo   <= tt-param.fi-it-codigo-fim  AND 
         item.ge-codigo   >= tt-param.fi-ge-codigo-ini AND
         item.ge-codigo  <= tt-param.fi-ge-codigo-fim NO-LOCK
         BY item.it-codigo.

    RUN pi-acompanhar IN  h-acomp (INPUT "Item: " + item.it-codigo).

    FIND FIRST tt-item WHERE 
               tt-item.it-codigo = item.it-codigo NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-item THEN DO:
       CREATE tt-item.
       ASSIGN tt-item.it-codigo = item.it-codigo
              tt-item.desc-item = item.desc-item
              tt-item.un        = item.un
              tt-item.c-orig    = IF ITEM.codigo-orig = 0  THEN "Nac" ELSE "Imp".  

       RUN esapi/calc-sldini-item-data.p (INPUT para-ped.estab-padrao,
                                          INPUT item.it-codigo,
                                          INPUT tt-param.fi-dt-trans-ini,
                                          OUTPUT tt-item.qt-ini).

       /*
       ASSIGN da-dt-calc = tt-param.fi-dt-trans-ini - 1.
       IF MONTH(da-dt-calc) = MONTH(tt-param.fi-dt-trans-ini) THEN
          ASSIGN da-dt-calc = DATE("01" + STRING(MONTH(tt-param.fi-dt-trans-ini),"99") + STRING(YEAR(tt-param.fi-dt-trans-ini),"9999") ) - 1.
       
       FOR EACH sl-it-per WHERE
                sl-it-per.it-codigo = item.it-codigo AND
                sl-it-per.periodo = da-dt-calc NO-LOCK.
           ASSIGN tt-item.qt-ini = tt-item.qt-ini + sl-it-per.quantidade.
       END.
       */

       ASSIGN tt-item.qt-fim = tt-item.qt-ini.
    END.

    FOR EACH movto-estoq WHERE 
             movto-estoq.dt-trans    >= tt-param.fi-dt-trans-ini   AND
             movto-estoq.dt-trans    <= tt-param.fi-dt-trans-fim AND
             movto-estoq.it-codigo    = item.it-codigo AND
             movto-estoq.cod-estabel  = para-ped.estab-padrao
             NO-LOCK USE-INDEX data-item.
    
        RUN pi-acompanhar IN  h-acomp (INPUT "Movimento: " + STRING(movto-estoq.dt-trans,"99/99/9999") + ' - ' + movto-estoq.it-codigo + " " + movto-estoq.cod-refer).
    
    
        IF movto-estoq.esp-docto = 21 /* NFE */ AND
           movto-estoq.ct-codigo = "110511"     AND /* Mercadorias em Poder de Terceiros */
           movto-estoq.sc-codigo = "00001"      THEN 
           NEXT.
        
        IF (movto-estoq.esp-docto = 21 OR movto-estoq.esp-docto = 23) AND
           movto-estoq.ct-codigo = "190000" AND /* Recebimento Fisico */  
           movto-estoq.sc-codigo = "00013" THEN
           NEXT.
        
        IF movto-estoq.esp-docto = 28 /* REQ */ AND
           movto-estoq.ct-codigo = "110503" THEN   /* Produtos em Elabora‡Æo */
           NEXT.
    
        IF movto-estoq.esp-docto = 33 AND movto-estoq.cod-depos <> "log" THEN /* Transferencia de Referencia */
           NEXT.
        
        /*Transferencia entre Empresas*/
        IF movto-estoq.esp-docto = 23 THEN DO.
           IF movto-estoq.tipo-trans = 1 THEN /*Entrada*/
              ASSIGN tt-item.qt-tran-ent = tt-item.qt-tran-ent + movto-estoq.quantidade
                      tt-item.qt-fim = tt-item.qt-fim + movto-estoq.quantidade.
           ELSE /*Saida*/
              ASSIGN tt-item.qt-tran-sai = tt-item.qt-tran-sai + movto-estoq.quantidade
                     tt-item.qt-fim = tt-item.qt-fim - movto-estoq.quantidade.

           NEXT.
        END.

        IF movto-estoq.esp-docto = 21 OR
           movto-estoq.esp-docto = 22 THEN DO.

           IF movto-estoq.referencia BEGINS 'RF' THEN NEXT. /* Recebimento F¡sico */

           IF movto-estoq.cod-emitente    = 1      OR /* Ima Tecidos */
              movto-estoq.cod-emitente    = 11212  OR /* Ima Tecidos */
              movto-estoq.cod-emitente    = 13516  OR /* Ima Tecidos */
              movto-estoq.cod-emitente    = 17232  OR /* Ima Tecidos */
              movto-estoq.cod-emitente    = 10535     /* MedTextil   */ THEN DO:
              IF movto-estoq.tipo-trans = 1 THEN /*Entrada*/
                 ASSIGN tt-item.qt-tran-ent = tt-item.qt-tran-ent + movto-estoq.quantidade
                        tt-item.qt-fim = tt-item.qt-fim + movto-estoq.quantidade.
              ELSE 
                 ASSIGN tt-item.qt-tran-sai = tt-item.qt-tran-sai + movto-estoq.quantidade
                        tt-item.qt-fim = tt-item.qt-fim - movto-estoq.quantidade.

              NEXT.
           END.
        END.

        /* Notas Fiscal */
        IF movto-estoq.esp-docto >= 19 AND 
           movto-estoq.esp-docto <= 22 OR
           (movto-estoq.esp-docto = 06 /* DIV */ 
             AND movto-estoq.ct-codigo = "110503")  THEN DO:
            
           IF movto-estoq.tipo-trans = 1 THEN /*Entrada*/
              ASSIGN tt-item.qt-nota-ent = tt-item.qt-nota-ent + movto-estoq.quantidade
                     tt-item.qt-fim = tt-item.qt-fim + movto-estoq.quantidade.
           ELSE /*Saida*/
              ASSIGN tt-item.qt-nota-sai = tt-item.qt-nota-sai + movto-estoq.quantidade
                     tt-item.qt-fim = tt-item.qt-fim - movto-estoq.quantidade.

           NEXT.  
        END.
        ELSE DO:
           IF movto-estoq.tipo-trans = 1 THEN /*Entrada*/
              ASSIGN tt-item.qt-ajus-ent = tt-item.qt-ajus-ent + movto-estoq.quantidade
                     tt-item.qt-fim = tt-item.qt-fim + movto-estoq.quantidade.
           ELSE /*Saida*/
              ASSIGN tt-item.qt-ajus-sai = tt-item.qt-ajus-sai + movto-estoq.quantidade
                     tt-item.qt-fim = tt-item.qt-fim - movto-estoq.quantidade.

           NEXT.
        END.
    END.
END.

IF tg-salva-excel THEN DO:
    CREATE "Excel.Application" chExcelApp NO-ERROR.
    chWorkBook = chExcelApp:Workbooks:Add(SEARCH("modelo-xlt\esimft05aa.xlt")).
    /*chWorkBook = chExcelApp:Workbooks:Add().*/
    chWorkSheet = chExcelApp:Sheets:Item(1).
    chexcelapp:VISIBLE = FALSE.
    
    ASSIGN i-linha = 4.
END.

FOR EACH tt-item WHERE tt-item.qt-ini      <> 0
                    or tt-item.qt-nota-ent <> 0
                    or tt-item.qt-nota-sai <> 0
                    or tt-item.qt-tran-ent <> 0
                    or tt-item.qt-tran-sai <> 0
                    or tt-item.qt-ajus-ent <> 0
                    or tt-item.qt-ajus-sai <> 0
                    OR tt-item.qt-fim      <> 0.

    IF tg-salva-excel THEN DO:
       RUN pi-acompanhar IN  h-acomp (INPUT STRING("Imprimindo-xls") + '-' + tt-item.it-codigo  ).
       /*------------------------- Excel ----------------------------------*/
       chWorkSheet:Range("A" + STRING(i-linha)):VALUE = tt-item.it-codigo.
       chWorkSheet:Range("B" + STRING(i-linha)):VALUE = tt-item.desc-item.
       chWorkSheet:Range("C" + STRING(i-linha)):VALUE = tt-item.un.
       chWorkSheet:Range("D" + STRING(i-linha)):VALUE = tt-item.c-orig.
       chWorkSheet:Range("E" + STRING(i-linha)):VALUE = tt-item.qt-ini.
       chWorkSheet:Range("F" + STRING(i-linha)):VALUE = tt-item.qt-nota-ent.
       chWorkSheet:Range("G" + STRING(i-linha)):VALUE = tt-item.qt-nota-sai.
       chWorkSheet:Range("H" + STRING(i-linha)):VALUE = tt-item.qt-tran-ent.
       chWorkSheet:Range("I" + STRING(i-linha)):VALUE = tt-item.qt-tran-sai.
       chWorkSheet:Range("J" + STRING(i-linha)):VALUE = tt-item.qt-ajus-ent.
       chWorkSheet:Range("K" + STRING(i-linha)):VALUE = tt-item.qt-ajus-sai.
       chWorkSheet:Range("L" + STRING(i-linha)):VALUE = tt-item.qt-fim.
       ASSIGN i-linha = i-linha + 1.
       /*--------------------------------------------------------------------*/
    END.
    ELSE DO.
        RUN pi-acompanhar IN  h-acomp (INPUT STRING("Imprimindo-txt") + '-' + tt-item.it-codigo  ).
    
        PUT tt-item.it-codigo      AT 01  FORMAT "x(6)"
            tt-item.desc-item      AT 08  FORMAT "x(28)"
            tt-item.un             AT 37  FORMAT "x(2)"
            tt-item.c-orig         AT 40  FORMAT "x(3)"
            tt-item.qt-ini         AT 45  FORMAT ">>>,>>9.99"
            tt-item.qt-nota-ent    AT 56  FORMAT ">>>,>>9.99"
            tt-item.qt-nota-sai    AT 67  FORMAT ">>>,>>9.99"
            tt-item.qt-tran-ent    AT 78  FORMAT ">>,>>9.99"
            tt-item.qt-tran-sai    AT 89  FORMAT ">>,>>9.99"
            tt-item.qt-ajus-ent    AT 100 FORMAT ">>,>>9.99"
            tt-item.qt-ajus-sai    AT 111 FORMAT ">>,>>9.99"
            tt-item.qt-fim         AT 122 FORMAT "->>>,>>9.99"
            SKIP.
    END.
    
    FIND FIRST tt-total WHERE tt-total.un = tt-item.un NO-ERROR.
    IF NOT AVAIL tt-total THEN DO:
       CREATE tt-total.
       ASSIGN tt-total.un = tt-item.un.
    END.
    ASSIGN tt-total.qt-ini       = tt-total.qt-ini      + tt-item.qt-ini     
           tt-total.qt-nota-ent  = tt-total.qt-nota-ent + tt-item.qt-nota-ent
           tt-total.qt-nota-sai  = tt-total.qt-nota-sai + tt-item.qt-nota-sai
           tt-total.qt-tran-ent  = tt-total.qt-tran-ent + tt-item.qt-tran-ent
           tt-total.qt-tran-sai  = tt-total.qt-tran-sai + tt-item.qt-tran-sai
           tt-total.qt-ajus-ent  = tt-total.qt-ajus-ent + tt-item.qt-ajus-ent
           tt-total.qt-ajus-sai  = tt-total.qt-ajus-sai + tt-item.qt-ajus-sai
           tt-total.qt-fim       = tt-total.qt-fim      + tt-item.qt-fim.
END.

IF tg-salva-excel THEN DO:
    ASSIGN i-linha = i-linha + 1.
    chWorkSheet:Range("B" + STRING(i-linha)):VALUE = "Total".         
    ASSIGN i-linha = i-linha + 1.
END.
ELSE DO.
    PUT UNFORMAT SKIP(1)
        FILL("=",132)            AT 01 FORMAT "x(132)"
        "Total no per¡odo entre" AT 01
        tt-param.fi-dt-trans-ini AT 25 FORMAT "99/99/9999"
        "e"                      AT 36
        tt-param.fi-dt-trans-fim AT 38 FORMAT "99/99/9999"
        SKIP
        "Transferencia"         AT 72
        "Ajuste"                AT 104
        SKIP
        "UN"                    AT 07 
        "Qt Inicial"            AT 23   
        "Qt Entrada"            AT 38   
        "Qt Saida"              AT 54  
        "Entrada"               AT 69   
        "Saida"                 AT 85   
        "Entrada"               AT 97    
        "Saida"                 AT 113     
        "Qt Final"              AT 125
        SKIP
        FILL("=",132)            AT 01 FORMAT "x(132)" .
END.

FOR EACH tt-total BREAK BY tt-total.un.
    IF tg-salva-excel THEN DO:
        chWorkSheet:Range("C" + STRING(i-linha)):VALUE = tt-total.un.         
        chWorkSheet:Range("E" + STRING(i-linha)):VALUE = tt-total.qt-ini.     
        chWorkSheet:Range("F" + STRING(i-linha)):VALUE = tt-total.qt-nota-ent.
        chWorkSheet:Range("G" + STRING(i-linha)):VALUE = tt-total.qt-nota-sai.
        chWorkSheet:Range("H" + STRING(i-linha)):VALUE = tt-total.qt-tran-ent.
        chWorkSheet:Range("I" + STRING(i-linha)):VALUE = tt-total.qt-tran-sai.
        chWorkSheet:Range("J" + STRING(i-linha)):VALUE = tt-total.qt-ajus-ent.
        chWorkSheet:Range("K" + STRING(i-linha)):VALUE = tt-total.qt-ajus-sai.
        chWorkSheet:Range("L" + STRING(i-linha)):VALUE = tt-total.qt-fim .    
        ASSIGN i-linha = i-linha + 1.
    END.
    ELSE DO.
        PUT "TOTAL"                  AT 01
            tt-total.un              AT 07
            tt-total.qt-ini          AT 21  FORMAT ">>>,>>>,>>9.99"
            tt-total.qt-nota-ent     AT 35  FORMAT ">>>,>>>,>>9.99"
            tt-total.qt-nota-sai     AT 49  FORMAT ">>>,>>>,>>9.99"
            tt-total.qt-tran-ent     AT 63  FORMAT ">>>,>>>,>>9.99" 
            tt-total.qt-tran-sai     AT 77  FORMAT ">>>,>>>,>>9.99" 
            tt-total.qt-ajus-ent     AT 91  FORMAT ">>,>>>,>>9.99" 
            tt-total.qt-ajus-sai     AT 105 FORMAT ">>,>>>,>>9.99" 
            tt-total.qt-fim          AT 118 FORMAT "->>>,>>>,>>9.99".
    END.
END.

IF tg-salva-excel THEN DO:
   /*------------------------- Excel ----------------------------------*/
   RUN pi-acompanhar IN  h-acomp (INPUT STRING("Salvando Arquivo EXCEL")).
   
   ASSIGN c-arquivo-xls = SESSION:TEMP-DIRECTORY + "esimft05aa" + "_" + string(day(NOW)) + "-" + string(MONTH(NOW)) + "-" + string(YEAR(NOW)) + "_" + SUBSTRING(STRING(TIME,"HH:MM:SS"),1,2) + "h" + SUBSTRING(STRING(TIME,"HH:MM:SS"),4,2) + "m" + SUBSTRING(STRING(TIME,"HH:MM:SS"),7,2) + "s" + ".xls".
   RELEASE OBJECT chWorkSheet.
   chWorkBook:SaveAs(c-arquivo-xls,-4143,,,,,).
   chWorkBook:Save().  
   chexcelapp:VISIBLE = TRUE.
   /*chWorkBook:Close().  */
   /*chExcelApp:Quit().  */

   /*chWorkBook:Close().  
   chExcelApp:Quit().*/
   
   RELEASE OBJECT chWorkBook.
   RELEASE OBJECT chExcelApp.
   /*------------------------------------------------------------------*/
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
RUN pi-finalizar in h-acomp.
RETURN "OK":U.
