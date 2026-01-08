/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Global Variable Definitions **********************************************/
DEF NEW GLOBAL SHARED VAR h-objeto AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-browse AS HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR h-campo AS WIDGET-HANDLE NO-UNDO.

/* Variable Definitions *****************************************************/
DEF VAR c-objeto AS CHAR NO-UNDO.

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

/* Main Block ***************************************************************/

IF p-ind-event = "BEFORE-INITIALIZE" THEN DO:
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:TYPE <> "field-group" THEN DO:
         IF h-objeto:NAME = "c-inicial" THEN DO.
            h-objeto:HELP = "<ENTER>-Marca Refˆrencia Digitada   <F8>-Desmarca TODAS Referˆncias".
            ON 'ENTER':U OF h-objeto PERSISTENT RUN esupc/upc-cd1506e1.p.
            ON 'LEAVE':U OF h-objeto PERSISTENT RUN esupc/upc-cd1506e1.p.
            ON 'F8':U OF h-objeto PERSISTENT RUN esupc/upc-cd1506e1.p.
         END.
         /*
         IF h-objeto:NAME = 'fpage1' THEN DO.
             ASSIGN h-campo = h-objeto:FIRST-CHILD.
             ASSIGN h-campo = h-campo:FIRST-CHILD.
             DO WHILE VALID-HANDLE(h-campo):

                 IF h-campo:NAME = "brSource" THEN DO.
                    ASSIGN h-browse = h-campo.

                 END.

                 IF h-campo:NAME = 'btAddTarget' THEN 
                    ON 'choose':U OF h-campo PERSISTENT RUN esupc/upc-cd1506c1.p.

                ASSIGN h-campo = h-campo:NEXT-SIBLING.
             END.
         END.
         */
         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
END.
