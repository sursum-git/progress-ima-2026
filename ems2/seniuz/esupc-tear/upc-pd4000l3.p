DEF INPUT PARAMETER h-cb-frame AS HANDLE.
DEF NEW GLOBAL SHARED VAR wh-acond AS WIDGET-HANDLE NO-UNDO.

ASSIGN h-cb-frame:SCREEN-VALUE = "Complementos".
APPLY 'value-changed' TO h-cb-frame.

APPLY 'entry' TO wh-acond.
RETURN NO-APPLY.
