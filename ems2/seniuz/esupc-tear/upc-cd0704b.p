/* Programa: upc-cd0704b.p
** Autor...: Seniuz - Toninho  Setembro/2011
*/

/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Variable Definitions *****************************************************/
DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.

/* Main Block ***************************************************************/

IF p-ind-event = "BEFORE-INITIALIZE" AND
   p-ind-object = "VIEWER" THEN DO.
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
       IF h-objeto:TYPE <> "field-group" THEN DO:
          IF h-objeto:NAME = "e-mail" THEN DO.
             ASSIGN h-objeto:FORMAT = "x(62)".
          END.
          ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
       END.
       ELSE 
          ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
END.
