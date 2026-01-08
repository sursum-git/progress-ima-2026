DEF INPUT PARAM p-ind-event        AS CHAR          NO-UNDO.
DEF INPUT PARAM p-ind-object       AS CHAR          NO-UNDO.
DEF INPUT PARAM p-wgh-object       AS HANDLE        NO-UNDO.
DEF INPUT PARAM p-wgh-frame        AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p-cod-table        AS CHAR          NO-UNDO.
DEF INPUT PARAM p-row-table        AS ROWID         NO-UNDO.

DEF NEW GLOBAL SHARED VAR h-bt-altera AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-bt-save   AS HANDLE.

DEF VAR h-page   AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.
DEF VAR c-objeto AS CHAR NO-UNDO.
DEF VAR h-browse AS WIDGET-HANDLE NO-UNDO.

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:private-data, "~/").

IF p-ind-event = "AFTER-INITIALIZE" THEN DO:
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.

   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:NAME = "fpage1" THEN
         ASSIGN h-page = h-objeto.

      ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
   END.

   ASSIGN h-objeto = h-page:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:NAME = "browse-1" THEN
         ASSIGN h-browse = h-objeto.
      
      IF h-objeto:NAME = 'btUpdateSon1' THEN
         ASSIGN h-bt-altera = h-objeto.

      IF h-objeto:NAME = 'bt-sav' THEN
         ASSIGN h-bt-save = h-objeto.

      ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
   END.
   ON 'ENTRY':U OF h-browse PERSISTENT RUN esepc/epc-im0100a-e1.p (INPUT h-browse). 
END.

