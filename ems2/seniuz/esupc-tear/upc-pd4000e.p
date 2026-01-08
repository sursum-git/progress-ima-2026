DEF INPUT PARAM p-ind-event      AS CHAR          NO-UNDO.
DEF INPUT PARAM p-ind-object     AS CHAR          NO-UNDO.
DEF INPUT PARAM p-wgh-object     AS HANDLE        NO-UNDO.
DEF INPUT PARAM p-wgh-frame      AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p-cod-table      AS CHAR          NO-UNDO.
DEF INPUT PARAM p-row-table      AS ROWID         NO-UNDO.
DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR wh-copia-reservas AS WIDGET-HANDLE NO-UNDO.

IF p-ind-event = "BEFORE-INITIALIZE" THEN DO:
   CREATE TOGGLE-BOX wh-copia-reservas
          ASSIGN FRAME = p-wgh-frame
                 WIDTH        = 25
                 ROW          = 2.4
                 COL          = 47
                 LABEL        = "Transferencia de Reservas"
                 VISIBLE      = YES
                 SENSITIVE    = YES.
                
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:TYPE <> "field-group" THEN DO:
         IF h-objeto:NAME = 'tg-exp-dt-entrega' THEN 
            ASSIGN h-objeto:ROW = 1.6.                 

         IF h-objeto:NAME = 'bt-ok' THEN DO.
            ON 'MOUSE-SELECT-CLICK':U OF h-objeto PERSISTENT RUN esupc/upc-pd4000em1.p.
            ON 'ENTER':U OF h-objeto PERSISTENT RUN esupc/upc-pd4000em1.p. 
         END.

         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
END.

