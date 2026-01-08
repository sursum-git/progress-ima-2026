DEF INPUT PARAM p-ind-event      AS CHAR          NO-UNDO.
DEF INPUT PARAM p-ind-object     AS CHAR          NO-UNDO.
DEF INPUT PARAM p-wgh-object     AS HANDLE        NO-UNDO.
DEF INPUT PARAM p-wgh-frame      AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p-cod-table      AS CHAR          NO-UNDO.
DEF INPUT PARAM p-row-table      AS ROWID         NO-UNDO.

DEF NEW GLOBAL SHARED VAR h-bt-30  AS WIDGET-HANDLE NO-UNDO.

DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-browse AS WIDGET-HANDLE NO-UNDO.

IF p-ind-event = "BEFORE-INITIALIZE" THEN DO:
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.

   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:NAME = "br-destina" THEN DO.
         ASSIGN h-browse = h-objeto.
         ON 'entry':U OF h-objeto PERSISTENT RUN esupc/upc-cr0102a-c1.p (INPUT h-browse). 
      END.
      
      IF h-objeto:TYPE <> "field-group" THEN
         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
END.

