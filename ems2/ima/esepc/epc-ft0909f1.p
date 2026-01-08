DEF INPUT PARAMETER p-wgh-frame AS HANDLE.

DEF NEW GLOBAL SHARED VAR h-bt-imprime AS WIDGET-HANDLE NO-UNDO.

DEF VAR h-frame  AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.

ASSIGN h-frame = p-wgh-frame:FIRST-CHILD           
       h-objeto = h-frame:FIRST-CHILD.

DO WHILE h-objeto <> ?: 
   IF h-objeto:TYPE <> "field-group" THEN DO:

      IF h-objeto:NAME = 'btDetalhar' THEN
         ASSIGN h-bt-imprime:SENSITIVE = h-objeto:SENSITIVE.

      IF h-objeto:NAME = 'brCartaCorrecao' THEN DO.
         ON 'ROW-DISPLAY':U OF h-objeto DO.
             ASSIGN h-bt-imprime:SENSITIVE = h-objeto:SENSITIVE.   
         END.
      END.
      
      ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
   END.
   ELSE
      ASSIGN h-objeto = h-objeto:FIRST-CHILD.
END.

