/* Programa: upc-ft4002.p
**    Autor: Toninho - 18/Jul/2007
** Objetivo: Atender apenas as Sequencias que foram reservadas
**
*/

/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Global Variable Definitions **********************************************/
DEF NEW GLOBAL SHARED VAR h-browse      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-query       AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-btATendSeq  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-btCancel    AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR h-nome-abrev  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-nr-pedcli   AS WIDGET-HANDLE NO-UNDO.


/* Variable Definitions *****************************************************/
DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-campo AS WIDGET-HANDLE NO-UNDO.

DEF VAR i-ct AS INT.
DEF VAR h-buffer      AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE hb-nr-sequencia AS HANDLE  NO-UNDO.

/* Variavies criadas na viewer dinamicamente*********************************/


/* Main Block ***************************************************************/
IF p-ind-event = "AFTER-INITIALIZE" THEN DO.
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:TYPE <> "field-group" THEN DO:
         CASE h-objeto:NAME.
             WHEN 'nome-abrev' THEN
                 ASSIGN h-nome-abrev = h-objeto.
             WHEN 'nr-pedcli' THEN
                 ASSIGN h-nr-pedcli = h-objeto.
             WHEN "btCancel" THEN
                 ASSIGN h-btCancel = h-objeto.
             WHEN "fPage2"THEN DO.
                 ASSIGN h-campo = h-objeto:FIRST-CHILD.
                 ASSIGN h-campo = h-campo:FIRST-CHILD.
                 DO WHILE VALID-HANDLE(h-campo):
                    IF h-campo:NAME = "btATendSeq" THEN
                       ASSIGN h-btATendSeq = h-campo.

                    IF NOT VALID-HANDLE(h-query) AND 
                       h-campo:NAME = "brson2" THEN DO.
                       ASSIGN h-browse = h-campo
                              h-query = h-browse:QUERY.
                    END.
                    ASSIGN h-campo = h-campo:NEXT-SIBLING.
                 END.
             END.
         END CASE.
         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
END.

IF p-ind-event = "AFTER-CHANGE-PAGE " AND
   SELF:NAME = 'Page2' THEN DO.

   IF VALID-HANDLE(h-btCancel) AND h-btCancel:SENSITIVE AND
      VALID-HANDLE(h-btATendSeq) AND h-btATendSeq:SENSITIVE THEN DO.

      ASSIGN h-buffer = h-query:GET-BUFFER-HANDLE(1)
             hb-nr-sequencia = h-buffer:BUFFER-FIELD(1).
    
       h-query:GET-FIRST.
       REPEAT WHILE NOT h-query:QUERY-OFF-END.
           FIND ped-item-res WHERE
                ped-item-res.nr-pedcli = h-nr-pedcli:SCREEN-VALUE AND
                ped-item-res.nome-abrev = h-nome-abrev:SCREEN-VALUE AND
                ped-item-res.nr-sequencia = hb-nr-sequencia:BUFFER-VALUE
                NO-LOCK NO-ERROR.

           IF AVAIL ped-item-res AND
              ped-item-res.faturado = NO THEN DO.

              h-query:REPOSITION-TO-ROW(h-query:CURRENT-RESULT-ROW).
              h-browse:SELECT-FOCUSED-ROW().

              APPLY 'choose' TO h-btATendSeq.
           END.

           h-query:GET-NEXT.
       END.
   END.
END.
