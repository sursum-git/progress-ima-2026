DEF NEW GLOBAL SHARED VAR h-bt-prazo-med AS WIDGET-HANDLE NO-UNDO.

IF VALID-HANDLE(h-bt-prazo-med) AND
   h-bt-prazo-med:SENSITIVE THEN
   APPLY 'choose' TO h-bt-prazo-med.

