/* Programa: 
** Objetivo: 
**           
** Autor...: 
*/

/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Global Variable Definitions **********************************************/
{include/i-vrtab.i ped-venda}.

/* Variable Definitions *****************************************************/
DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-campo AS WIDGET-HANDLE NO-UNDO.

/* Variavies criadas na viewer dinamicamente*********************************/

/* Main Block ***************************************************************/

IF p-ind-event = "AFTER-INITIALIZE" THEN DO.
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:TYPE <> "field-group" THEN DO:
         IF h-objeto:NAME = "fPage1"THEN DO.
            ASSIGN h-campo = h-objeto:FIRST-CHILD.
            ASSIGN h-campo = h-campo:FIRST-CHILD.
            DO WHILE VALID-HANDLE(h-campo):
               IF h-campo:NAME = "dt-base-dup" THEN 
                  ON 'leave':U OF h-campo PERSISTENT RUN esupc/upc-ft4003bl1.p.
               ASSIGN h-campo = h-campo:NEXT-SIBLING.
            END.
         END.
         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
END.
