/* Parameter Definitions ****************************************************/
DEF INPUT PARAM p-ind-event      AS CHAR          NO-UNDO.
DEF INPUT PARAM p-ind-object     AS CHAR          NO-UNDO.
DEF INPUT PARAM p-wgh-object     AS HANDLE        NO-UNDO.
DEF INPUT PARAM p-wgh-frame      AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p-cod-table      AS CHAR          NO-UNDO.
DEF INPUT PARAM p-recid-table    AS RECID         NO-UNDO.

/* Global Variable Definitions **********************************************/
DEF NEW GLOBAL SHARED VAR h-bt-rp-vlr-dg  AS WIDGET-HANDLE NO-UNDO.

DEF VAR h-objeto          AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-campo           AS WIDGET-HANDLE NO-UNDO.

IF p-ind-event = 'INITIALIZE' AND
   p-ind-object = 'VIEWER' THEN DO.

   CREATE BUTTON h-bt-rp-vlr-dg
          ASSIGN FRAME = p-wgh-frame
                 WIDTH        = 7
                 HEIGHT       = 1.22
                 ROW          = 16.8
                 COL          = 8
                 VISIBLE      = YES
                 SENSITIVE    = YES
                 TOOLTIP      = "Replica Valor Posicionado para Campos Subsequentes"
                 TRIGGERS:
                     ON "CHOOSE":U PERSISTENT RUN esepc/epc-utb061za-1.p (INPUT p-wgh-frame).
                       
                END TRIGGERS.

   h-bt-rp-vlr-dg:LOAD-IMAGE("image/im-copy.gif").


   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-campo = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-campo):
      IF h-campo:TYPE <> "field-group" THEN DO:
         IF h-campo:NAME = 'v_val_cotac_dia_mes' THEN
            ON "LEAVE":U OF h-campo PERSISTENT RUN esepc/epc-utb061za-2.p.

         ASSIGN h-campo = h-campo:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-campo= h-campo:FIRST-CHILD.
   END.
END.

