/* Programa: upc-cp0323.p
** Objetivo: Dar manuten‡Æo nos Dados complementares do Reporte de Repetitivo,
**           referentes …s customiza‡äes da Tear Tˆxtil Ind.Com.Ltda.
** Autor...: Prodb - Toninho  Outubro/2004
*/

/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Global Variable Definitions **********************************************/
DEF NEW GLOBAL SHARED VAR h-it-codigo AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-cod-refer AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-lote-serie AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-dep-pad-sai AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-cod-depos  AS HANDLE.

/* Variable Definitions *****************************************************/
DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.
DEF VAR c-objeto AS CHAR NO-UNDO.

/* Variavies criadas na viewer dinamicamente*********************************/

/* Inicializa‡Æo */
ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

/* Main Block ***************************************************************/

IF p-ind-event = "BEFORE-INITIALIZE" THEN DO:
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:TYPE <> "field-group" THEN DO:
         IF h-objeto:NAME = "fi-it-codigo" THEN
            ASSIGN h-it-codigo = h-objeto.

         IF h-objeto:NAME = "fi-cod-depos" THEN
            ASSIGN h-cod-depos = h-objeto.

         IF h-objeto:NAME = "fi-cod-estabel" THEN DO.
            ON 'entry':U OF h-objeto PERSISTENT RUN esupc/upc-cp0324l3.p.
         END.

         IF h-objeto:NAME = "fi-dep-pad-sai" THEN DO.
            ASSIGN h-dep-pad-sai = h-objeto.
            ON 'entry':U OF h-objeto PERSISTENT RUN esupc/upc-cp0324l3.p.
         END.

         IF h-objeto:NAME = "fi-cod-refer" THEN DO.
            ASSIGN h-cod-refer = h-objeto.
            ON 'leave':U OF h-objeto PERSISTENT RUN esupc/upc-cp0324l1.p.
         END.

         IF h-objeto:NAME = "fi-lote-serie" THEN DO.
            ASSIGN h-lote-serie = h-objeto.
            ON 'leave':U OF h-objeto PERSISTENT RUN esupc/upc-cp0324l2.p.
         END.
         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
END.

IF p-ind-event = "BEFORE-ASSIGN" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v01in379.w" THEN DO:
   FIND item WHERE 
        item.it-codigo = h-it-codigo:SCREEN-VALUE NO-LOCK NO-ERROR.

   IF item.tipo-con-est = 4 THEN DO.
      IF LOOKUP(SUBSTR(h-lote-serie:SCREEN-VALUE,1,2),"PP,PD,RP,RD") = 0 THEN DO.
         MESSAGE "Lote deve inciar com PP,PD,RP,RD" VIEW-AS ALERT-BOX.
         APPLY 'entry' TO h-lote-serie.
         RETURN 'NOK'. 
      END.

      IF NOT h-lote-serie:SCREEN-VALUE MATCHES "*" + h-cod-refer:SCREEN-VALUE THEN DO.
         MESSAGE "Lote deve ser composto de PP,PD,RP,RD + Referencia"
                 VIEW-AS ALERT-BOX.
         APPLY 'entry' TO h-lote-serie.
         RETURN 'NOK'.
      END.
   END.
END.
