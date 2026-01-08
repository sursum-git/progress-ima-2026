/****************************************************************************
** Programa: upc-boin163.p - EPC de BO da tabela preco-item
** Objetivo: Replicar preco do Item para TODAS as Referencias
*****************************************************************************/
{include/i-epc200.i} /*Definicao tt-EPC*/

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-preco-item NO-UNDO LIKE preco-item
       FIELD r-rowid as rowid.

DEF BUFFER b-preco-item FOR preco-item.

/* Parameters Definitions */
DEF INPUT        PARAM p-ind-event AS CHAR   NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-epc.

/* Global Definitions */
DEF NEW GLOBAL SHARED VAR wh-preco-rp AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-preco-rd AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-all-ref  AS HANDLE NO-UNDO.

/* Variable definitions */
DEF VAR h-bo AS HANDLE.

IF p-ind-event = "AfterCreateRecord" THEN DO.
   FIND FIRST tt-epc WHERE 
              tt-epc.cod-event     = p-ind-event AND
              tt-epc.cod-parameter = "Object-Handle" NO-LOCK NO-ERROR.

   IF AVAILABLE tt-epc THEN DO.
      ASSIGN h-bo = WIDGET-HANDLE(tt-epc.val-parameter).
      RUN getRecord IN h-bo (OUTPUT TABLE tt-preco-item).

      FIND FIRST tt-preco-item NO-LOCK NO-ERROR.
      IF AVAIL tt-preco-item THEN DO.
         RUN pi-cria-ext-preco (INPUT tt-preco-item.nr-tabpre,
                                INPUT tt-preco-item.it-codigo,
                                INPUT tt-preco-item.cod-refer).  
   
         IF LOGICAL(wh-all-ref:SCREEN-VALUE) THEN DO.
            FOR EACH ref-item WHERE
                     ref-item.it-codigo = tt-preco-item.it-codigo AND
                     ref-item.cod-refer <> tt-preco-item.cod-refer NO-LOCK.
            
                FIND FIRST b-preco-item WHERE 
                           b-preco-item.nr-tabpre = tt-preco-item.nr-tabpre AND
                           b-preco-item.it-codigo = tt-preco-item.it-codigo AND
                           b-preco-item.cod-refer = ref-item.cod-refer 
                           SHARE-LOCK NO-ERROR.
                IF NOT AVAIL b-preco-item THEN DO.
                   CREATE b-preco-item.
                   BUFFER-COPY tt-preco-item TO b-preco-item
                        ASSIGN b-preco-item.cod-refer = ref-item.cod-refer.
    
                   ASSIGN b-preco-item.preco-venda   = tt-preco-item.preco-venda
                          b-preco-item.preco-fob     = tt-preco-item.preco-fob
                          b-preco-item.dec-1         = tt-preco-item.dec-1
                          b-preco-item.cod-unid-med  = tt-preco-item.cod-unid-med
                          b-preco-item.preco-min-cif = tt-preco-item.preco-venda
                          b-preco-item.preco-min-fob = tt-preco-item.preco-fob.
            
                   RUN pi-cria-ext-preco (INPUT b-preco-item.nr-tabpre,
                                          INPUT b-preco-item.it-codigo,
                                          INPUT b-preco-item.cod-refer).  
                END.
            END.
         END.
      END.
   END.
END.

PROCEDURE pi-cria-ext-preco.
    DEF INPUT PARAMETER p-nr-tabpre LIKE preco-item.nr-tabpre.
    DEF INPUT PARAMETER p-it-codigo LIKE preco-item.it-codigo.
    DEF INPUT PARAMETER p-cod-refer LIKE preco-item.cod-refer.

    FIND preco-item-ext WHERE
         preco-item-ext.nr-tabpre = p-nr-tabpre AND
         preco-item-ext.it-codigo = p-it-codigo AND
         preco-item-ext.cod-refer = p-cod-refer
         SHARE-LOCK NO-ERROR.

    IF NOT AVAIL preco-item-ext THEN DO.
       CREATE preco-item-ext.
       ASSIGN preco-item-ext.nr-tabpre = p-nr-tabpre
              preco-item-ext.it-codigo = p-it-codigo
              preco-item-ext.cod-refer = p-cod-refer.
    END.
    ASSIGN preco-item-ext.preco-rp = DECIMAL(wh-preco-rp:SCREEN-VALUE)
           preco-item-ext.preco-rd = DECIMAL(wh-preco-rd:SCREEN-VALUE).

END PROCEDURE.




