/****************************************************************************
** Programa: upc-boin163.p - EPC de BO da tabela preco-item
** Objetivo: Replicar preco do Item para TODAS as Referencias
** Autor...: SeniuZ - Toninho  Setembro/2012
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
DEF NEW GLOBAL SHARED VAR wh-all-ref  AS HANDLE NO-UNDO.

/* Variable definitions */
DEF VAR h-bo AS HANDLE.

IF p-ind-event = "AfterCreateRecord" or
   p-ind-event = "AfterUpdateRecord" THEN DO.
   FIND FIRST tt-epc WHERE 
              tt-epc.cod-event     = p-ind-event AND
              tt-epc.cod-parameter = "Object-Handle" NO-LOCK NO-ERROR.

   IF AVAILABLE tt-epc THEN DO.
      ASSIGN h-bo = WIDGET-HANDLE(tt-epc.val-parameter).
      RUN getRecord IN h-bo (OUTPUT TABLE tt-preco-item).

      FIND FIRST tt-preco-item SHARE-LOCK NO-ERROR.
      IF AVAIL tt-preco-item THEN DO.
         FIND preco-item WHERE 
              preco-item.nr-tabpre = tt-preco-item.nr-tabpre AND
              preco-item.it-codigo = tt-preco-item.it-codigo AND
              preco-item.cod-refer = tt-preco-item.cod-refer AND
              preco-item.cod-unid-med = tt-preco-item.cod-unid-med AND
              preco-item.dt-inival = tt-preco-item.dt-inival AND
              preco-item.quant-min = tt-preco-item.quant-min
              SHARE-LOCK NO-ERROR.

         FIND item WHERE
              item.it-codigo = preco-item.it-codigo NO-LOCK NO-ERROR.

         FIND tb-preco WHERE
              tb-preco.nr-tabpre = preco-item.nr-tabpre NO-LOCK NO-ERROR.

         ASSIGN preco-item.dt-inival = tb-preco.dt-inival
                preco-item.situacao = tb-preco.situacao
                preco-item.char-2 = item.desc-item.

         IF LOGICAL(wh-all-ref:SCREEN-VALUE) THEN DO.
            FOR EACH ref-item WHERE
                     ref-item.it-codigo = preco-item.it-codigo AND
                     ref-item.cod-refer <> preco-item.cod-refer NO-LOCK.
            
                FIND b-preco-item WHERE 
                     b-preco-item.nr-tabpre = preco-item.nr-tabpre AND
                     b-preco-item.it-codigo = preco-item.it-codigo AND
                     b-preco-item.cod-refer = ref-item.cod-refer AND
                     b-preco-item.cod-unid-med = preco-item.cod-unid-med AND
                     b-preco-item.dt-inival = preco-item.dt-inival AND
                     b-preco-item.quant-min = preco-item.quant-min
                     SHARE-LOCK NO-ERROR.
                IF NOT AVAIL b-preco-item THEN DO.
                   CREATE b-preco-item.
                   BUFFER-COPY preco-item TO b-preco-item
                        ASSIGN b-preco-item.cod-refer = ref-item.cod-refer.
                END.
                ASSIGN b-preco-item.preco-venda   = preco-item.preco-venda
                       b-preco-item.preco-fob     = preco-item.preco-fob
                       b-preco-item.dec-1         = preco-item.dec-1
                       b-preco-item.preco-min-cif = preco-item.preco-venda
                       b-preco-item.preco-min-fob = preco-item.preco-fob.
            END.
         END.
      END.
   END.
END.


