DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER          NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER          NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE             NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE      NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER          NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID              NO-UNDO.

DEF VAR h-frame  AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR c-cbEmpresa AS CHAR.

IF p-ind-event = 'INITIALIZE' THEN DO.
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:NAME = 'cbEmpresa' THEN DO.
         ON 'value-changed':U OF h-objeto PERSISTENT RUN esepc/epc-btb962ad-m2.p.
      END.

      IF h-objeto:NAME = 'bt-ok' THEN
         ON 'mouse-select-click':U OF h-objeto PERSISTENT RUN esepc/epc-btb962ad-m1.p.

      ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
   END.
END.

