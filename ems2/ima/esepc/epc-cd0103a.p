DEF INPUT PARAMETER p-wgh-frame AS HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR h-cotacao AS HANDLE NO-UNDO.

DEF VAR h-objeto          AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-campo           AS WIDGET-HANDLE NO-UNDO.

ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
ASSIGN h-campo = h-objeto:FIRST-CHILD.

DO WHILE VALID-HANDLE(h-campo):
   IF h-campo:TYPE <> "field-group" THEN DO:
      IF h-campo:NAME = 'cotacao' THEN
         IF h-campo:INDEX > h-cotacao:INDEX AND 
            h-campo:SENSITIVE THEN
            ASSIGN h-campo:SCREEN-VALUE = h-cotacao:SCREEN-VALUE.

      ASSIGN h-campo = h-campo:NEXT-SIBLING.
   END.
   ELSE 
      ASSIGN h-campo= h-campo:FIRST-CHILD.
END.


