DEF NEW GLOBAL SHARED VAR wh-composi      AS widget-handle no-undo.

FIND composi WHERE
     composi.cod-composi = wh-composi:SCREEN-VALUE NO-LOCK NO-ERROR.

IF NOT AVAIL composi THEN DO.
   MESSAGE 'Composiá∆o n∆o Cadastrada...' VIEW-AS ALERT-BOX.
   APPLY 'ENTRY' TO wh-composi.
   RETURN NO-APPLY.
END.
