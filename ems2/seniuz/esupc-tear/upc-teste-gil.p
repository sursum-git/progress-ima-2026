/* Programa: upc-teste.p
** Objetivo: Desabilitar os Botäes de navega‡Æo para os representantes
** Autor...: DBNet - Toninho  Agosto/2005
** Observ..: 
*/

/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Global Variable Definitions **********************************************/
DEFINE NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h-e-mail      AS HANDLE.

DEFINE VAR h-objeto       AS WIDGET-HANDLE NO-UNDO.
DEFINE VAR c-objeto       AS CHAR NO-UNDO.

/* Main Block ***************************************************************/
ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
ASSIGN h-objeto = h-objeto:FIRST-CHILD.
DO WHILE VALID-HANDLE(h-objeto):
   IF h-objeto:TYPE <> "field-group" THEN DO:
      IF h-objeto:NAME = "e-mail" THEN DO.
         ASSIGN h-e-mail = h-objeto. 

         MESSAGE "c-seg-usuario: " c-seg-usuario SKIP
                 "p-ind-event: "  p-ind-event    SKIP
                 "p-ind-object: " p-ind-object   SKIP
                 "p-wgh-object: " p-wgh-object   SKIP
                 "p-wgh-frame: "  p-wgh-frame    SKIP
                 "p-cod-table: "  p-cod-table    SKIP
                 "c-objeto: "     c-objeto       SKIP
                 "h-objeto: "     h-objeto:NAME
                 VIEW-AS ALERT-BOX.
      END.
      ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
   END.
   ELSE 
      ASSIGN h-objeto = h-objeto:FIRST-CHILD.
END.

