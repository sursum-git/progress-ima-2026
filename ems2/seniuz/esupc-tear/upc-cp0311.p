/* Programa: upc-cp0311.p
** Objetivo: Dar manuten‡Æo nos Dados complementares do Reporte de Produ‡Æo,
**           referentes …s customiza‡äes da Tear Tˆxtil Ind.Com.Ltda.
** Autor...: Prodb - Toninho  Mar‡o/2004
*/

/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Global Variable Definitions **********************************************/
DEF NEW GLOBAL SHARED VAR h-container AS HANDLE. 
DEF NEW GLOBAL SHARED VAR h-objeto AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-cod-refer AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-lote-serie AS HANDLE.
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

/* Variable Definitions *****************************************************/
DEF VAR c-objeto AS CHAR NO-UNDO.
DEF VAR l-baixa-pronto AS LOGICAL.

/* Variavies criadas na viewer dinamicamente*********************************/
ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

/* Main Block ***************************************************************/

IF p-ind-event = "INITIALIZE" AND
   p-ind-object = "CONTAINER" THEN
   ASSIGN h-container = p-wgh-object.

IF p-ind-event = "BEFORE-INITIALIZE" THEN DO:
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:TYPE <> "field-group" THEN DO:
         IF h-objeto:NAME = "fi-cod-refer-acabado" THEN 
            ASSIGN h-cod-refer = h-objeto.

         IF h-objeto:NAME = "fi-lote-serie-acabado" THEN 
            ASSIGN h-lote-serie = h-objeto.

         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
END.

IF p-ind-event = "ASSIGN" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v43in271.w" THEN DO:
   FIND ord-prod WHERE 
        ROWID(ord-prod) = p-row-table NO-ERROR.

   FIND item WHERE 
        item.it-codigo = ord-prod.it-codigo NO-LOCK NO-ERROR.

   IF item.tipo-con-est = 4 THEN DO.
      IF LOOKUP(SUBSTR(h-lote-serie:SCREEN-VALUE,1,2),"PP,PD,RP,RD") = 0 THEN DO.
         MESSAGE "Lote deve inciar com PP,PD,RP,RD" VIEW-AS ALERT-BOX.
         RUN select-page IN h-container (INPUT 2). 
         APPLY 'entry' TO h-lote-serie.
         RETURN "NOK". 
      END.
        
      IF NOT h-lote-serie:SCREEN-VALUE MATCHES "*" + h-cod-refer:SCREEN-VALUE THEN DO.
         MESSAGE "Lote deve ser composto de PP,PD,RP,RD + Referencia"
                 VIEW-AS ALERT-BOX.
         RUN select-page IN h-container (INPUT 2). 
         APPLY 'entry' TO h-lote-serie.
         RETURN "NOK".
      END.
   END.
END.

IF p-ind-event = "END-UPDATE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v43in271.w" THEN DO:

   FIND ord-prod WHERE 
        ROWID(ord-prod) = p-row-table NO-ERROR.

   IF ord-prod.cod-depos = "cru" THEN DO.
      FIND cru-prog WHERE 
           cru-prog.it-codigo = ord-prod.it-codigo AND 
           cru-prog.cod-refer = ord-prod.cod-refer NO-ERROR.
      IF AVAIL cru-prog THEN 
         ASSIGN cru-prog.processo = cru-prog.processo - ord-prod.qt-reportada.
   END.

   IF ord-prod.tipo <> 4 THEN DO.
      FIND ref-item-ext WHERE 
           ref-item-ext.it-codigo = ord-prod.it-codigo AND 
           ref-item-ext.cod-refer = ord-prod.cod-refer NO-ERROR.

      IF AVAIL ref-item-ext THEN DO.
         IF ref-item-ext.qtd-pron > ord-prod.qt-reportada THEN DO.
            RUN esupc/epc-cp0311.w (INPUT ref-item-ext.qtd-pron, OUTPUT l-baixa-pronto).

            IF l-baixa-pronto THEN 
               ASSIGN ref-item-ext.qtd-pron = ref-item-ext.qtd-pron - ord-prod.qt-reportada.
            ELSE 
               ASSIGN ref-item-ext.qtd-pron = 0.
         END.
         ELSE
            ASSIGN ref-item-ext.qtd-pron = 0.
         ASSIGN ref-item-ext.usu-ult-pron = c-seg-usuario
                ref-item-ext.dt-ult-pron  = TODAY.
      END.
   END.
END.
