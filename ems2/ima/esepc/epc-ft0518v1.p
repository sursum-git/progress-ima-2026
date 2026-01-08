DEF INPUT PARAM h-objeto AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR h-ed-justif AS HANDLE NO-UNDO.

IF h-objeto:SCREEN-VALUE = '2' THEN DO.
   ASSIGN h-ed-justif:SENSITIVE = YES.
   APPLY 'ENTRY' TO h-ed-justif.
END.
ELSE
   ASSIGN h-ed-justif:SCREEN-VALUE = ""
          h-ed-justif:SENSITIVE = NO.
