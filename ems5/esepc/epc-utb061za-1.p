DEF INPUT PARAMETER p-wgh-frame AS HANDLE NO-UNDO.
 
DEF NEW GLOBAL SHARED VAR h-cotacao AS HANDLE NO-UNDO.

DEF VAR h-objeto          AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-campo           AS WIDGET-HANDLE NO-UNDO.

IF NOT VALID-HANDLE(h-cotacao) THEN RETURN NO-APPLY.
IF h-cotacao:NAME <> 'v_val_cotac_dia_mes' THEN RETURN NO-APPLY.

ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
ASSIGN h-campo = h-objeto:FIRST-CHILD.

DO WHILE VALID-HANDLE(h-campo):
   IF h-campo:TYPE <> "field-group" THEN DO:
      IF h-campo:NAME = 'v_val_cotac_dia_mes' THEN
         IF h-campo:INDEX > h-cotacao:INDEX THEN
            ASSIGN h-campo:SCREEN-VALUE = h-cotacao:SCREEN-VALUE.

      ASSIGN h-campo = h-campo:NEXT-SIBLING.
   END.
   ELSE 
      ASSIGN h-campo= h-campo:FIRST-CHILD.
END.


