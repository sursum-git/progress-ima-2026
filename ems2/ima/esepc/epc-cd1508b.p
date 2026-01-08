/* Programa: upc-cd1508b.p
** Objetivo: Solicitar Pre‡os para Rolo Perfeito e Defeituoso 
** Autor...: SeniuZ - Toninho  Setembro/2012
*/

/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Global Variable Definitions **********************************************/

/* Variavies criadas na viewer dinamicamente*********************************/
DEF NEW GLOBAL SHARED VAR wh-all-ref  AS HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR h-cod-refer AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-it-codigo AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR h-preco-min-cif AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-preco-min-fob AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-preco-venda AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-preco-fob AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-dt-inival AS WIDGET-HANDLE NO-UNDO.

/* Variable Definitions *****************************************************/
DEF VAR c-objeto AS CHAR NO-UNDO.
DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").


/* Main Block ***************************************************************/





IF p-ind-event = "BEFORE-INITIALIZE" THEN DO:
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:TYPE <> "field-group" THEN DO:
         IF h-objeto:NAME = "preco-venda" THEN DO.
            ASSIGN h-objeto:FORMAT = ">>>,>>9.99999"
                   h-preco-venda = h-objeto.
            ON "LEAVE" OF h-objeto PERSISTENT RUN esepc/epc-cd1508b2.p.
         END.

         IF h-objeto:NAME = 'dt-inival' THEN
            ASSIGN h-dt-inival = h-objeto.

         IF h-objeto:NAME = "preco-fob" THEN
            ASSIGN h-objeto:FORMAT = ">>>,>>9.99999"
                   h-preco-fob = h-objeto.

         IF h-objeto:NAME = "preco-min-cif" THEN
            ASSIGN h-objeto:FORMAT = ">>>,>>9.99999"
                   h-preco-min-cif = h-objeto.

         IF h-objeto:NAME = "preco-min-fob" THEN
            ASSIGN h-objeto:FORMAT = ">>>,>>9.99999"
                   h-preco-min-fob = h-objeto.

         IF h-objeto:NAME = "cod-refer" THEN
            ASSIGN h-cod-refer = h-objeto.

         IF h-objeto:NAME = "it-codigo" THEN 
            ASSIGN h-it-codigo = h-objeto.

         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.

   CREATE TOGGLE-BOX wh-all-ref
       ASSIGN FRAME              = p-wgh-frame
              WIDTH              = 10
              HEIGHT             = 0.88
              ROW                = 2.3
              COL                = 35.7
              LABEL              = "Todas" 
              VISIBLE            = YES
              SENSITIVE          = NO
              TRIGGERS:
                  ON "VALUE-CHANGED" PERSISTENT RUN esepc/epc-cd1508b1.p (INPUT h-cod-refer).
                  ON "LEAVE" PERSISTENT RUN esepc/epc-cd1508b3.p.
              END TRIGGERS.

   wh-all-ref:MOVE-AFTER-TAB(h-cod-refer).
   h-preco-venda:MOVE-AFTER-TAB(h-it-codigo).
END.

IF p-ind-event = "AFTER-INITIALIZE" THEN DO.
   IF NOT h-it-codigo:SENSITIVE THEN DO.
      APPLY 'ENTRY' TO h-preco-venda.
      RETURN NO-APPLY.
   END.
END.

IF p-ind-event = "AFTER-ENABLE" THEN 
   ASSIGN wh-all-ref:SENSITIVE = h-cod-refer:SENSITIVE
          h-cod-refer:SENSITIVE = NO.

IF p-ind-event = "AFTER-DISPLAY" THEN 
   ASSIGN wh-all-ref:SCREEN-VALUE = 'YES'.

IF p-ind-event = "BEFORE-ASSIGN" THEN DO:
   IF LOGICAL(wh-all-ref:SCREEN-VALUE) AND
      h-cod-refer:SCREEN-VALUE = '' THEN DO.
      FIND item WHERE
           item.it-codigo = h-it-codigo:SCREEN-VALUE NO-LOCK NO-ERROR.
  
      FIND FIRST ref-item WHERE
                 ref-item.it-codigo = item.it-codigo NO-LOCK NO-ERROR.

      IF AVAIL ref-item THEN DO.
         ASSIGN h-cod-refer:SCREEN-VALUE = ref-item.cod-refer.
         APPLY 'LEAVE' TO h-cod-refer.
      END.
   END.
END.

