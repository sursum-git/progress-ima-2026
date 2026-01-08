/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Variable Definitions *****************************************************/
DEF NEW GLOBAL SHARED VAR wh-browse     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-last-col    AS HANDLE NO-UNDO.

DEF VAR c-objeto AS CHAR NO-UNDO.
DEF VAR h_frame  AS HANDLE.


/* Variavies criadas na viewer dinamicamente*********************************/
ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

/* Main Block ***************************************************************/
IF p-ind-event = "INITIALIZE" AND 
   p-ind-object = "BROWSER" AND 
   p-cod-table = "TT-DOC-PEND-APROV" THEN DO:

   ASSIGN h_frame = p-wgh-frame:FIRST-CHILD
          h_frame = h_frame:FIRST-CHILD.

   DO WHILE h_frame <> ?: /*Passando campo a campo da tela para efetuar altera‡äes*/
      IF h_frame:TYPE <> "field-group" THEN DO:
         IF h_frame:NAME = 'br_table' THEN DO:
            ASSIGN wh-browse = h_frame:HANDLE.
            wh-browse:ALLOW-COLUMN-SEARCHING = TRUE.
            wh-browse:CLEAR-SORT-ARROWS().
            wh-browse:FIRST-COLUMN:SORT-ASCENDING = FALSE. 

            ON 'START-SEARCH':U OF wh-browse PERSISTENT RUN esepc/epc-cd1720-s.p.
         END.
         ASSIGN h_frame = h_frame:NEXT-SIBLING.
      END.
      ELSE
         ASSIGN h_frame = h_frame:FIRST-CHILD.
   END.
END.

