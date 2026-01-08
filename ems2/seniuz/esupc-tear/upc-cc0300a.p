/* Programa: upc-cc0300a.p
** Objetivo: Informar uma descricao complementar para as Entregas,
**           esta descricao ser  mostrada no Pedido de Compra. 
** Autor...: DBNet - Toninho  Mar‡o/2005
*/

/* Parameter Definitions ****************************************************/
define input parameter p-ind-event  as character.
define input parameter p-ind-object as character.
define input parameter p-wgh-object as handle.
define input parameter p-wgh-frame  as widget-handle.
define input parameter p-cod-table  as character.
define input parameter p-row-table  as rowid.

/* Global Variable Definitions **********************************************/
DEF NEW GLOBAL SHARED VAR h-fPage3 AS HANDLE NO-UNDO.

/* Variavies criadas na viewer dinamicamente*********************************/
DEF NEW GLOBAL SHARED VAR wh-obs-entrega AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR tx-obs-entrega AS HANDLE NO-UNDO.

/* Variable Definitions *****************************************************/
DEF VAR h-objeto AS HANDLE.
DEF VAR c-objeto AS CHAR NO-UNDO.

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

/* Main Block ***************************************************************/
IF p-ind-event = "BEFORE-INITIALIZE" THEN DO:
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.

   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:TYPE <> "field-group" THEN DO:
         IF h-objeto:NAME = "fpage3" THEN
            ASSIGN h-fPage3 = h-objeto.

         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
END.

IF p-ind-event = "AFTER-INITIALIZE" THEN DO:
   CREATE TEXT tx-obs-entrega
     ASSIGN FRAME        = h-fPage3
            FORMAT       = "x(17)"
            WIDTH        = 17
            SCREEN-VALUE = "Observ. Entrega:"
            ROW          = 1.9
            COL          = 2.6
            VISIBLE      = YES.

   CREATE FILL-IN wh-obs-entrega
     ASSIGN FRAME             = h-fPage3
            SIDE-LABEL-HANDLE = tx-obs-entrega:HANDLE
            FORMAT            = "x(60)"
            NAME              = "fi-obs-entrega"
            WIDTH             = 60.5
            HEIGHT            = 0.88
            ROW               = 1.8
            COL               = 14.5
            LABEL             = "Observ. Entrega:"
            VISIBLE           = YES
            SENSITIVE         = YES.
END.


IF p-ind-event = "BEFORE-CHANGE-PAGE" AND 
   p-ind-object = "CONTAINER" THEN DO:
   IF VALID-HANDLE(wh-obs-entrega) THEN DO:
      APPLY 'entry' TO wh-obs-entrega.

      FIND pedido-compr WHERE
           ROWID(pedido-compr) = p-row-table NO-LOCK NO-ERROR.

      IF AVAIL pedido-compr THEN
         ASSIGN wh-obs-entrega:SCREEN-VALUE = pedido-compr.compl-entrega.
   END.
END.


IF p-ind-event = "AFTER-ASSIGN" THEN DO.
   FIND pedido-compr WHERE
        ROWID(pedido-compr) = p-row-table NO-ERROR.

   ASSIGN pedido-compr.compl-entrega = wh-obs-entrega:SCREEN-VALUE.
END.

