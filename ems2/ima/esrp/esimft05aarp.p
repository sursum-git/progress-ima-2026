/* PROGRAMA: RELAT-ALGODAORP.P                                               **
** DATA    : 09/ABRIL/2010                                                   **
** AUTOR   : Anderson Fagner                                                  **
** OBJETIVO: RELATàRIO DE ITENS 100% ALGODÇO                                 **
******************************************************************************/

/* Programa de controle de versao e seguran»a do Datasul EMS */
{include/i-prgvrs.i relat-algodaorp 2.06.00.001}
{esp/util.i}

/* Definicao de variaveis globais comuns a todos os programas do EMS */
{utp/ut-glob.i}
DEF VAR h-acomp     AS HANDLE  NO-UNDO.
DEF VAR da-dt-calc  AS DATE    FORMAT "99/99/9999".

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Processando..").


DEFINE TEMP-TABLE tt-param NO-UNDO
    FIELD destino           AS INTEGER
    FIELD arquivo           AS CHAR FORMAT "x(35)"
    FIELD usuario           AS CHAR FORMAT "x(12)"
    FIELD data-exec         AS DATE
    FIELD hora-exec         AS INTEGER
    FIELD classifica        AS INTEGER
    FIELD desc-classifica   AS CHAR FORMAT "x(40)"
    FIELD modelo-rtf        AS CHAR FORMAT "x(35)"
    FIELD l-habilitaRtf     AS LOG
    FIELD cod-estabel-ini   AS CHAR
    FIELD cod-estabel-fim   AS CHAR
    FIELD fi-it-codigo-ini  AS CHAR
    FIELD fi-it-codigo-fim  AS CHAR
    FIELD fi-cod-refer-ini  AS CHAR
    FIELD fi-cod-refer-fim  AS CHAR
    FIELD fi-ge-codigo-ini  AS INT
    FIELD fi-ge-codigo-fim  AS INT
    FIELD fi-dt-trans-ini   AS DATE
    FIELD fi-dt-trans-fim   AS DATE
    FIELD tg-deposito-arm   AS log
    FIELD tg-deposito-alm   AS log
    FIELD tg-deposito-emb   AS log
    FIELD tg-deposito-sal   AS log
    FIELD tg-deposito-aim   AS LOG
    FIELD tg-deposito-log   AS log
    FIELD tg-deposito-fech  AS log
    FIELD tg-salva-excel    AS LOG
    FIELD rs-origem         AS INT
    FIELD tg-detalhado      AS LOG.
    
DEF TEMP-TABLE tt-resumido
    FIELD it-codigo   AS CHAR
    FIELD desc-item   AS CHAR
    FIELD classif     LIKE fam-comerc.descricao
    FIELD un          AS CHAR
    FIELD c-orig      AS CHAR
    FIELD qt-ini      LIKE saldo-estoq.qtidade-atu
    FIELD qt-nota-ent LIKE saldo-estoq.qtidade-atu
    FIELD qt-nota-sai LIKE saldo-estoq.qtidade-atu
    FIELD qt-tran-ent LIKE saldo-estoq.qtidade-atu
    FIELD qt-tran-sai LIKE saldo-estoq.qtidade-atu
    FIELD qt-ajus-ent LIKE saldo-estoq.qtidade-atu
    FIELD qt-ajus-sai LIKE saldo-estoq.qtidade-atu
    FIELD qt-fim      LIKE saldo-estoq.qtidade-atu
    FIELD qt-ent      LIKE saldo-estoq.qtidade-atu
    FIELD qt-sai      LIKE saldo-estoq.qtidade-atu
    INDEX indice1 it-codigo.

DEF TEMP-TABLE tt-detalhado
    FIELD it-codigo   AS CHAR
    FIELD cod-refer   AS CHAR
    FIELD desc-item   AS CHAR
    FIELD classif     LIKE fam-comerc.descricao
    FIELD un          AS CHAR
    FIELD c-orig      AS CHAR
    FIELD qt-ini      LIKE saldo-estoq.qtidade-atu
    FIELD qt-nota-ent LIKE saldo-estoq.qtidade-atu
    FIELD qt-nota-sai LIKE saldo-estoq.qtidade-atu
    FIELD qt-tran-ent LIKE saldo-estoq.qtidade-atu
    FIELD qt-tran-sai LIKE saldo-estoq.qtidade-atu
    FIELD qt-ajus-ent LIKE saldo-estoq.qtidade-atu
    FIELD qt-ajus-sai LIKE saldo-estoq.qtidade-atu
    FIELD qt-fim      LIKE saldo-estoq.qtidade-atu
    FIELD qt-ent      LIKE saldo-estoq.qtidade-atu
    FIELD qt-sai      LIKE saldo-estoq.qtidade-atu
    INDEX indice1 it-codigo.

DEFINE TEMP-TABLE tt-detalhado-2 LIKE tt-detalhado
    FIELD ano   AS INT
    FIELD mes   AS INT
    INDEX princ IS PRIMARY it-codigo ano mes .

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
DEFINE VARIABLE l-resumido AS LOGICAL.

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

FIND FIRST ped-venda NO-LOCK NO-ERROR.

FOR EACH item WHERE
         item.it-codigo  >= tt-param.fi-it-codigo-ini AND
         item.it-codigo  <= tt-param.fi-it-codigo-fim AND
         item.ge-codigo  >= tt-param.fi-ge-codigo-ini AND
         item.ge-codigo  <= tt-param.fi-ge-codigo-fim NO-LOCK.

    FIND fam-comerc WHERE
         fam-comerc.fm-cod-com = item.fm-cod-com NO-LOCK NO-ERROR.

    FIND FIRST tt-resumido WHERE 
               tt-resumido.it-codigo = item.it-codigo NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-resumido THEN DO:
       CREATE tt-resumido.
       ASSIGN tt-resumido.it-codigo = item.it-codigo
              tt-resumido.desc-item = item.desc-item
              tt-resumido.classif   = IF AVAIL fam-comerc 
                                      THEN fam-comerc.descricao
                                      ELSE "SEM CLASSIF"
              tt-resumido.un        = item.un
              tt-resumido.c-orig    = IF item.codigo-orig = 0  THEN "Nac" ELSE "Imp".  
    END.

    FOR EACH saldo-estoq WHERE
             saldo-estoq.cod-estabel >= tt-param.cod-estabel-ini AND
             saldo-estoq.cod-estabel <= tt-param.cod-estabel-fim AND
             saldo-estoq.it-codigo   = item.it-codigo  AND 
             saldo-estoq.cod-refer   >= tt-param.fi-cod-refer-ini AND
             saldo-estoq.cod-refer   <= tt-param.fi-cod-refer-fim NO-LOCK.

        IF saldo-estoq.cod-estabel = '505' AND 
           saldo-estoq.cod-depos <> 'ITA'  THEN NEXT.
        
        IF tg-deposito-fech = YES AND
           saldo-estoq.cod-depos <> 'DEF' THEN NEXT.

        IF tg-deposito-fech = NO AND
           saldo-estoq.cod-depos = 'DEF' THEN NEXT.

        ASSIGN tt-resumido.qt-ini = tt-resumido.qt-ini + saldo-estoq.qtidade-atu.

        FIND FIRST tt-detalhado WHERE 
                   tt-detalhado.it-codigo = item.it-codigo AND 
                   tt-detalhado.cod-refer = saldo-estoq.cod-refer NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-detalhado THEN DO:
           CREATE tt-detalhado.
           ASSIGN tt-detalhado.it-codigo = item.it-codigo
                  tt-detalhado.cod-refer = saldo-estoq.cod-refer
                  tt-detalhado.desc-item = item.desc-item
                  tt-detalhado.classif   = tt-resumido.classif
                  tt-detalhado.un        = item.un
                  tt-detalhado.c-orig    = IF ITEM.codigo-orig = 0  THEN "Nac" ELSE "Imp".  
        END.
        ASSIGN tt-detalhado.qt-ini = tt-detalhado.qt-ini + saldo-estoq.qtidade-atu.
    END.
END.

IF tt-param.tg-detalhado THEN DO.
   FOR EACH tt-detalhado NO-LOCK,
       EACH movto-estoq WHERE 
            movto-estoq.cod-estabel >= tt-param.cod-estabel-ini AND
            movto-estoq.cod-estabel <= tt-param.cod-estabel-fim AND
            movto-estoq.it-codigo = tt-detalhado.it-codigo AND
            movto-estoq.dt-trans  >= tt-param.fi-dt-trans-ini NO-LOCK.

       IF movto-estoq.cod-estabel = '505' AND
          movto-estoq.cod-depos <> 'ITA'  THEN DO: 
          //IF movto-estoq.esp-docto <> 22 THEN // diferente de faturamento
             NEXT.

       END.
       
       IF tg-deposito-fech = YES AND
          movto-estoq.cod-depos <> 'DEF' THEN NEXT.

       IF tg-deposito-fech = NO AND
          movto-estoq.cod-depos = 'DEF' THEN NEXT.

       /*
       FIND natur-oper WHERE
            natur-oper.nat-oper = movto-estoq.nat-oper NO-LOCK NO-ERROR.
       IF AVAIL natur-oper THEN DO.
          IF tt-param.tg-deposito-fech = NO AND
             natur-oper.terceiros = YES THEN NEXT.

          IF tt-param.tg-deposito-fech = YES AND
             natur-oper.terceiros = NO THEN NEXT.
       END.
       */
       
       /*
       IF AVAIL natur-oper AND
          natur-oper.tipo = 1 AND
          natur-oper.tp-oper-terc = 2 THEN NEXT. */

        IF movto-estoq.cod-refer <> tt-detalhado.cod-refer THEN NEXT.

        RUN pi-acompanhar IN  h-acomp (INPUT "Movimento: " + movto-estoq.it-codigo + "-" + movto-estoq.cod-refer + " " + STRING(movto-estoq.dt-trans,"99/99/9999")).

        /*
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
        */

        IF movto-estoq.tipo-trans = 1 THEN /*Entrada*/
           ASSIGN tt-detalhado.qt-ent = tt-detalhado.qt-ent + movto-estoq.quantidade.
        ELSE
           ASSIGN tt-detalhado.qt-sai = tt-detalhado.qt-sai + movto-estoq.quantidade.

        IF movto-estoq.dt-trans > tt-param.fi-dt-trans-fim THEN NEXT.

        /*Transferencia entre Empresas*/
        IF movto-estoq.esp-docto = 23 THEN DO.
           IF movto-estoq.tipo-trans = 1 THEN /*Entrada*/
              ASSIGN tt-detalhado.qt-tran-ent = tt-detalhado.qt-tran-ent + movto-estoq.quantidade.
           ELSE /*Saida*/
              ASSIGN tt-detalhado.qt-tran-sai = tt-detalhado.qt-tran-sai + movto-estoq.quantidade.
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
                 ASSIGN tt-detalhado.qt-tran-ent = tt-detalhado.qt-tran-ent + movto-estoq.quantidade.
              ELSE 
                 ASSIGN tt-detalhado.qt-tran-sai = tt-detalhado.qt-tran-sai + movto-estoq.quantidade.

              NEXT.
           END.
        END.

        /* Notas Fiscal */
        IF movto-estoq.esp-docto >= 19 AND 
           movto-estoq.esp-docto <= 22 OR
           (movto-estoq.esp-docto = 06 /* DIV */ 
             AND movto-estoq.ct-codigo = "110503")  THEN DO:

           IF movto-estoq.tipo-trans = 1 THEN /*Entrada*/
              ASSIGN tt-detalhado.qt-nota-ent = tt-detalhado.qt-nota-ent + movto-estoq.quantidade.
           ELSE /*Saida*/
              ASSIGN tt-detalhado.qt-nota-sai = tt-detalhado.qt-nota-sai + movto-estoq.quantidade.

           NEXT.  
        END.
        ELSE DO:
           IF movto-estoq.tipo-trans = 1 THEN /*Entrada*/
              ASSIGN tt-detalhado.qt-ajus-ent = tt-detalhado.qt-ajus-ent + movto-estoq.quantidade.
           ELSE /*Saida*/
              ASSIGN tt-detalhado.qt-ajus-sai = tt-detalhado.qt-ajus-sai + movto-estoq.quantidade.

           NEXT.
        END.

        FIND tt-detalhado-2 
            WHERE tt-detalhado-2.it-codigo = tt-detalhado.it-codigo
            AND   tt-detalhado-2.ano       = YEAR(movto-estoq.dt-trans)
            AND   tt-detalhado-2.mes       = MONTH(movto-estoq.dt-trans)
            NO-ERROR.
        IF NOT AVAIL tt-detalhado-2 THEN DO:
            CREATE tt-detalhado-2.
            ASSIGN tt-detalhado-2.it-codigo = tt-detalhado.it-codigo        
                   tt-detalhado-2.ano       = YEAR(movto-estoq.dt-trans)    
                   tt-detalhado-2.mes       = MONTH(movto-estoq.dt-trans) 
                   .
        END.

        ASSIGN tt-detalhado-2.qt-ini       = tt-detalhado-2.qt-ini       + tt-detalhado.qt-ini     
               tt-detalhado-2.qt-nota-ent  = tt-detalhado-2.qt-nota-ent  + tt-detalhado.qt-nota-ent
               tt-detalhado-2.qt-nota-sai  = tt-detalhado-2.qt-nota-sai  + tt-detalhado.qt-nota-sai
               tt-detalhado-2.qt-tran-ent  = tt-detalhado-2.qt-tran-ent  + tt-detalhado.qt-tran-ent
               tt-detalhado-2.qt-tran-sai  = tt-detalhado-2.qt-tran-sai  + tt-detalhado.qt-tran-sai
               tt-detalhado-2.qt-ajus-ent  = tt-detalhado-2.qt-ajus-ent  + tt-detalhado.qt-ajus-ent
               tt-detalhado-2.qt-ajus-sai  = tt-detalhado-2.qt-ajus-sai  + tt-detalhado.qt-ajus-sai
               tt-detalhado-2.qt-fim       = tt-detalhado-2.qt-fim       + tt-detalhado.qt-fim     
               tt-detalhado-2.qt-ent       = tt-detalhado-2.qt-ent       + tt-detalhado.qt-ent     
               tt-detalhado-2.qt-sai       = tt-detalhado-2.qt-sai       + tt-detalhado.qt-sai 
               .
   END.

   FOR EACH tt-detalhado NO-LOCK.
       ASSIGN tt-detalhado.qt-ini = tt-detalhado.qt-ini - tt-detalhado.qt-ent + tt-detalhado.qt-sai.
       ASSIGN tt-detalhado.qt-fim = tt-detalhado.qt-ini + 
                               tt-detalhado.qt-nota-ent - tt-detalhado.qt-nota-sai + 
                               tt-detalhado.qt-tran-ent - tt-detalhado.qt-tran-sai + 
                               tt-detalhado.qt-ajus-ent - tt-detalhado.qt-ajus-sai.
   END.

   FOR EACH tt-detalhado-2 NO-LOCK.
       ASSIGN tt-detalhado-2.qt-ini = tt-detalhado-2.qt-ini - tt-detalhado-2.qt-ent + tt-detalhado-2.qt-sai.
       ASSIGN tt-detalhado-2.qt-fim = tt-detalhado-2.qt-ini + 
                               tt-detalhado-2.qt-nota-ent - tt-detalhado-2.qt-nota-sai + 
                               tt-detalhado-2.qt-tran-ent - tt-detalhado-2.qt-tran-sai + 
                               tt-detalhado-2.qt-ajus-ent - tt-detalhado-2.qt-ajus-sai.
   END.

   


END.
ELSE DO.
   FOR EACH tt-resumido NO-LOCK,
       EACH movto-estoq WHERE 
            movto-estoq.cod-estabel >= tt-param.cod-estabel-ini AND
            movto-estoq.cod-estabel <= tt-param.cod-estabel-fim AND
            movto-estoq.it-codigo = tt-resumido.it-codigo AND
            movto-estoq.dt-trans  >= tt-param.fi-dt-trans-ini NO-LOCK.
    
        RUN pi-acompanhar IN  h-acomp (INPUT "Movimento: " + movto-estoq.it-codigo + "-" + movto-estoq.cod-refer + " " + STRING(movto-estoq.dt-trans,"99/99/9999")).

        IF movto-estoq.cod-estabel = '505' AND
           movto-estoq.cod-depos <> 'ITA'  THEN DO: 
           //IF movto-estoq.esp-docto <> 22 THEN // diferente de faturamento
              NEXT.
        END.
        
        IF tg-deposito-fech = YES AND
           movto-estoq.cod-depos <> 'DEF' THEN NEXT.

        IF tg-deposito-fech = NO AND
           movto-estoq.cod-depos = 'DEF' THEN NEXT.

        /*
        IF AVAIL natur-oper AND
           natur-oper.tipo = 1 AND
           natur-oper.tp-oper-terc = 2 THEN NEXT.
        */   

        /*
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
        */

        IF movto-estoq.tipo-trans = 1 THEN /*Entrada*/
           ASSIGN tt-resumido.qt-ent = tt-resumido.qt-ent + movto-estoq.quantidade.
        ELSE
           ASSIGN tt-resumido.qt-sai = tt-resumido.qt-sai + movto-estoq.quantidade.
    
        IF movto-estoq.dt-trans > tt-param.fi-dt-trans-fim THEN NEXT.

        /*Transferencia entre Empresas*/
        IF movto-estoq.esp-docto = 23 THEN DO.
           IF movto-estoq.tipo-trans = 1 THEN /*Entrada*/
              ASSIGN tt-resumido.qt-tran-ent = tt-resumido.qt-tran-ent + movto-estoq.quantidade.
           ELSE /*Saida*/
              ASSIGN tt-resumido.qt-tran-sai = tt-resumido.qt-tran-sai + movto-estoq.quantidade.
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
                 ASSIGN tt-resumido.qt-tran-ent = tt-resumido.qt-tran-ent + movto-estoq.quantidade.
              ELSE 
                 ASSIGN tt-resumido.qt-tran-sai = tt-resumido.qt-tran-sai + movto-estoq.quantidade.
    
              NEXT.
           END.
        END.
    
        /* Notas Fiscal */
        IF movto-estoq.esp-docto >= 19 AND 
           movto-estoq.esp-docto <= 22 OR
           (movto-estoq.esp-docto = 06 /* DIV */ 
             AND movto-estoq.ct-codigo = "110503")  THEN DO:
            
           IF movto-estoq.tipo-trans = 1 THEN /*Entrada*/
              ASSIGN tt-resumido.qt-nota-ent = tt-resumido.qt-nota-ent + movto-estoq.quantidade.
           ELSE /*Saida*/
              ASSIGN tt-resumido.qt-nota-sai = tt-resumido.qt-nota-sai + movto-estoq.quantidade.
    
           NEXT.  
        END.
        ELSE DO:
           IF movto-estoq.tipo-trans = 1 THEN /*Entrada*/
              ASSIGN tt-resumido.qt-ajus-ent = tt-resumido.qt-ajus-ent + movto-estoq.quantidade.
           ELSE /*Saida*/
              ASSIGN tt-resumido.qt-ajus-sai = tt-resumido.qt-ajus-sai + movto-estoq.quantidade.
    
           NEXT.
        END.
   END.

   FOR EACH tt-resumido NO-LOCK.
       ASSIGN tt-resumido.qt-ini = tt-resumido.qt-ini - tt-resumido.qt-ent + tt-resumido.qt-sai.
       ASSIGN tt-resumido.qt-fim = tt-resumido.qt-ini + 
                               tt-resumido.qt-nota-ent - tt-resumido.qt-nota-sai + 
                               tt-resumido.qt-tran-ent - tt-resumido.qt-tran-sai + 
                               tt-resumido.qt-ajus-ent - tt-resumido.qt-ajus-sai.
   END.
END.

IF tg-salva-excel THEN DO:
    CREATE "Excel.Application" chExcelApp NO-ERROR.
    chWorkBook = chExcelApp:Workbooks:Add(SEARCH("modelo-xlt\esimft05aa.xlt")).
    chWorkSheet = chExcelApp:Sheets:Item(1).
    chexcelapp:VISIBLE = FALSE.
    
    ASSIGN i-linha = 4.
END.

IF tt-param.tg-detalhado THEN DO.
    FOR EACH tt-detalhado WHERE 
             tt-detalhado.qt-ini      <> 0 OR 
             tt-detalhado.qt-nota-ent <> 0 OR 
             tt-detalhado.qt-nota-sai <> 0 OR 
             tt-detalhado.qt-tran-ent <> 0 OR 
             tt-detalhado.qt-tran-sai <> 0 OR 
             tt-detalhado.qt-ajus-ent <> 0 OR 
             tt-detalhado.qt-ajus-sai <> 0 OR 
             tt-detalhado.qt-fim      <> 0 NO-LOCK.

         IF tg-salva-excel THEN DO:
            RUN pi-acompanhar IN h-acomp (INPUT STRING("Imprimindo-xls") + '-' + tt-detalhado.it-codigo  ).
            /*------------------------- Excel ----------------------------------*/
            chWorkSheet:Range("A" + STRING(i-linha)):VALUE = tt-detalhado.it-codigo.
            chWorkSheet:Range("B" + STRING(i-linha)):VALUE = tt-detalhado.desc-item.
            chWorkSheet:Range("C" + STRING(i-linha)):VALUE = tt-detalhado.cod-refer.
            chWorkSheet:Range("D" + STRING(i-linha)):VALUE = tt-detalhado.classif.
            chWorkSheet:Range("E" + STRING(i-linha)):VALUE = tt-detalhado.un.
            chWorkSheet:Range("F" + STRING(i-linha)):VALUE = tt-detalhado.c-orig.
            chWorkSheet:Range("G" + STRING(i-linha)):VALUE = tt-detalhado.qt-ini.
            chWorkSheet:Range("H" + STRING(i-linha)):VALUE = tt-detalhado.qt-nota-ent.
            chWorkSheet:Range("I" + STRING(i-linha)):VALUE = tt-detalhado.qt-nota-sai.
            chWorkSheet:Range("J" + STRING(i-linha)):VALUE = tt-detalhado.qt-tran-ent.
            chWorkSheet:Range("K" + STRING(i-linha)):VALUE = tt-detalhado.qt-tran-sai.
            chWorkSheet:Range("L" + STRING(i-linha)):VALUE = tt-detalhado.qt-ajus-ent.
            chWorkSheet:Range("M" + STRING(i-linha)):VALUE = tt-detalhado.qt-ajus-sai.
            chWorkSheet:Range("N" + STRING(i-linha)):VALUE = tt-detalhado.qt-fim.
            ASSIGN i-linha = i-linha + 1.
            /*--------------------------------------------------------------------*/
         END.
         ELSE DO.
            RUN pi-acompanhar IN  h-acomp (INPUT STRING("Imprimindo-txt") + '-' + tt-detalhado.it-codigo  ).

            PUT tt-detalhado.it-codigo      AT 01  FORMAT "x(6)"
                tt-detalhado.desc-item      AT 08  FORMAT "x(28)"
                tt-detalhado.classif        AT 30  FORMAT "x(20)"
                tt-detalhado.un             AT 59  FORMAT "x(2)"
                tt-detalhado.c-orig         AT 62  FORMAT "x(3)"
                tt-detalhado.qt-ini         AT 67  FORMAT ">>>,>>9.99"
                tt-detalhado.qt-nota-ent    AT 78  FORMAT ">>>,>>9.99"
                tt-detalhado.qt-nota-sai    AT 89  FORMAT ">>>,>>9.99"
                tt-detalhado.qt-tran-ent    AT 100 FORMAT ">>,>>9.99"
                tt-detalhado.qt-tran-sai    AT 111 FORMAT ">>,>>9.99"
                tt-detalhado.qt-ajus-ent    AT 122 FORMAT ">>,>>9.99"
                tt-detalhado.qt-ajus-sai    AT 133 FORMAT ">>,>>9.99"
                tt-detalhado.qt-fim         AT 144 FORMAT "->>>,>>9.99"
                SKIP.
         END.

         FIND FIRST tt-total WHERE 
                    tt-total.un = tt-detalhado.un NO-ERROR.
         IF NOT AVAIL tt-total THEN DO:
            CREATE tt-total.
            ASSIGN tt-total.un = tt-detalhado.un.
         END.
         ASSIGN tt-total.qt-ini       = tt-total.qt-ini      + tt-detalhado.qt-ini     
                tt-total.qt-nota-ent  = tt-total.qt-nota-ent + tt-detalhado.qt-nota-ent
                tt-total.qt-nota-sai  = tt-total.qt-nota-sai + tt-detalhado.qt-nota-sai
                tt-total.qt-tran-ent  = tt-total.qt-tran-ent + tt-detalhado.qt-tran-ent
                tt-total.qt-tran-sai  = tt-total.qt-tran-sai + tt-detalhado.qt-tran-sai
                tt-total.qt-ajus-ent  = tt-total.qt-ajus-ent + tt-detalhado.qt-ajus-ent
                tt-total.qt-ajus-sai  = tt-total.qt-ajus-sai + tt-detalhado.qt-ajus-sai
                tt-total.qt-fim       = tt-total.qt-fim      + tt-detalhado.qt-fim.
    END.
END.
ELSE DO.
   FOR EACH tt-resumido WHERE 
            tt-resumido.qt-ini      <> 0 OR 
            tt-resumido.qt-nota-ent <> 0 OR 
            tt-resumido.qt-nota-sai <> 0 OR 
            tt-resumido.qt-tran-ent <> 0 OR 
            tt-resumido.qt-tran-sai <> 0 OR 
            tt-resumido.qt-ajus-ent <> 0 OR 
            tt-resumido.qt-ajus-sai <> 0 OR 
            tt-resumido.qt-fim      <> 0 NO-LOCK.
    
        IF tg-salva-excel THEN DO:
           RUN pi-acompanhar IN  h-acomp (INPUT STRING("Imprimindo-xls") + '-' + tt-resumido.it-codigo  ).
           /*------------------------- Excel ----------------------------------*/
           chWorkSheet:Range("A" + STRING(i-linha)):VALUE = tt-resumido.it-codigo.
           chWorkSheet:Range("B" + STRING(i-linha)):VALUE = tt-resumido.desc-item.
           chWorkSheet:Range("D" + STRING(i-linha)):VALUE = tt-resumido.classif.
           chWorkSheet:Range("E" + STRING(i-linha)):VALUE = tt-resumido.un.
           chWorkSheet:Range("F" + STRING(i-linha)):VALUE = tt-resumido.c-orig.
           chWorkSheet:Range("G" + STRING(i-linha)):VALUE = tt-resumido.qt-ini.
           chWorkSheet:Range("H" + STRING(i-linha)):VALUE = tt-resumido.qt-nota-ent.
           chWorkSheet:Range("I" + STRING(i-linha)):VALUE = tt-resumido.qt-nota-sai.
           chWorkSheet:Range("J" + STRING(i-linha)):VALUE = tt-resumido.qt-tran-ent.
           chWorkSheet:Range("K" + STRING(i-linha)):VALUE = tt-resumido.qt-tran-sai.
           chWorkSheet:Range("L" + STRING(i-linha)):VALUE = tt-resumido.qt-ajus-ent.
           chWorkSheet:Range("M" + STRING(i-linha)):VALUE = tt-resumido.qt-ajus-sai.
           chWorkSheet:Range("N" + STRING(i-linha)):VALUE = tt-resumido.qt-fim.
           ASSIGN i-linha = i-linha + 1.
           /*--------------------------------------------------------------------*/
        END.
        ELSE DO.
            RUN pi-acompanhar IN  h-acomp (INPUT STRING("Imprimindo-txt") + '-' + tt-resumido.it-codigo  ).
        
            PUT tt-resumido.it-codigo      AT 01  FORMAT "x(6)"
                tt-resumido.desc-item      AT 08  FORMAT "x(28)"
                tt-resumido.classif        AT 37  FORMAT "x(20)"
                tt-resumido.un             AT 59  FORMAT "x(2)"
                tt-resumido.c-orig         AT 62  FORMAT "x(3)"
                tt-resumido.qt-ini         AT 67  FORMAT ">>>,>>9.99"
                tt-resumido.qt-nota-ent    AT 78  FORMAT ">>>,>>9.99"
                tt-resumido.qt-nota-sai    AT 89  FORMAT ">>>,>>9.99"
                tt-resumido.qt-tran-ent    AT 100 FORMAT ">>,>>9.99"
                tt-resumido.qt-tran-sai    AT 111 FORMAT ">>,>>9.99"
                tt-resumido.qt-ajus-ent    AT 122 FORMAT ">>,>>9.99"
                tt-resumido.qt-ajus-sai    AT 133 FORMAT ">>,>>9.99"
                tt-resumido.qt-fim         AT 144 FORMAT "->>>,>>9.99"
                SKIP.
        END.
        
        FIND FIRST tt-total WHERE 
                   tt-total.un = tt-resumido.un NO-ERROR.
        IF NOT AVAIL tt-total THEN DO:
           CREATE tt-total.
           ASSIGN tt-total.un = tt-resumido.un.
        END.
        ASSIGN tt-total.qt-ini       = tt-total.qt-ini      + tt-resumido.qt-ini     
               tt-total.qt-nota-ent  = tt-total.qt-nota-ent + tt-resumido.qt-nota-ent
               tt-total.qt-nota-sai  = tt-total.qt-nota-sai + tt-resumido.qt-nota-sai
               tt-total.qt-tran-ent  = tt-total.qt-tran-ent + tt-resumido.qt-tran-ent
               tt-total.qt-tran-sai  = tt-total.qt-tran-sai + tt-resumido.qt-tran-sai
               tt-total.qt-ajus-ent  = tt-total.qt-ajus-ent + tt-resumido.qt-ajus-ent
               tt-total.qt-ajus-sai  = tt-total.qt-ajus-sai + tt-resumido.qt-ajus-sai
               tt-total.qt-fim       = tt-total.qt-fim      + tt-resumido.qt-fim.
   END.
END.

IF tg-salva-excel THEN DO:
    ASSIGN i-linha = i-linha + 1.
    chWorkSheet:Range("C" + STRING(i-linha)):VALUE = "Total".         
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
        chWorkSheet:Range("E" + STRING(i-linha)):VALUE = tt-total.un.         
        chWorkSheet:Range("G" + STRING(i-linha)):VALUE = tt-total.qt-ini.     
        chWorkSheet:Range("H" + STRING(i-linha)):VALUE = tt-total.qt-nota-ent.
        chWorkSheet:Range("I" + STRING(i-linha)):VALUE = tt-total.qt-nota-sai.
        chWorkSheet:Range("J" + STRING(i-linha)):VALUE = tt-total.qt-tran-ent.
        chWorkSheet:Range("K" + STRING(i-linha)):VALUE = tt-total.qt-tran-sai.
        chWorkSheet:Range("L" + STRING(i-linha)):VALUE = tt-total.qt-ajus-ent.
        chWorkSheet:Range("M" + STRING(i-linha)):VALUE = tt-total.qt-ajus-sai.
        chWorkSheet:Range("N" + STRING(i-linha)):VALUE = tt-total.qt-fim .    
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
{esp/exportarTabelacsv3.i tt-detalhado-2 " " " " " " "tt-detalhado-ano-mes"}
/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
RUN pi-finalizar in h-acomp.
RETURN "OK":U.
