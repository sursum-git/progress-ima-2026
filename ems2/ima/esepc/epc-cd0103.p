/* Parameter Definitions ****************************************************/

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Global Variable Definitions **********************************************/
DEF NEW GLOBAL SHARED VAR wh-image  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-bt-rp-vlr-dg  AS WIDGET-HANDLE NO-UNDO.

DEF VAR h-objeto          AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-campo           AS WIDGET-HANDLE NO-UNDO.


IF p-ind-event = 'INITIALIZE' AND
   p-ind-object = 'VIEWER' THEN DO.

   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-campo = h-objeto:FIRST-CHILD.

   DO WHILE VALID-HANDLE(h-campo):
      IF h-campo:TYPE <> "field-group" THEN DO:
         IF h-campo:NAME = 'cotacao' THEN
            ON "LEAVE":U OF h-campo PERSISTENT RUN esepc/epc-cd0103b.p.

         ASSIGN h-campo = h-campo:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-campo=h-campo:FIRST-CHILD.
   END.
END.


IF p-ind-event = 'BEFORE-INITIALIZE' AND
   p-ind-object = 'VIEWER' THEN DO.
    
    CREATE BUTTON h-bt-rp-vlr-dg
          ASSIGN FRAME = p-wgh-frame
                 WIDTH        = 7
                 HEIGHT       = 1.22
                 ROW          = 14.20
                 COL          = 67.80
                 VISIBLE      = YES
                 SENSITIVE    = NO
                 TOOLTIP      = "Replica Valor Posicionado para Campos Subsequentes"
                 TRIGGERS:
                     ON "CHOOSE":U PERSISTENT RUN esepc/epc-cd0103a.p (INPUT p-wgh-frame).
                        
                END TRIGGERS.

   h-bt-rp-vlr-dg:LOAD-IMAGE("image/im-copy.gif").
END.

IF p-ind-event = "ENABLE" AND
   p-ind-object = "VIEWER"  THEN DO: 
   ASSIGN h-bt-rp-vlr-dg:SENSITIVE = YES.
END.

IF p-ind-event = "DISABLE" AND
   p-ind-object = "VIEWER"  THEN DO: 
   ASSIGN h-bt-rp-vlr-dg:SENSITIVE = NO.
END.
