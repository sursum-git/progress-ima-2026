DEF NEW GLOBAL SHARED VAR wh-rlgp      AS widget-handle no-undo.

IF wh-rlgp:SCREEN-VALUE < "1" or
   wh-rlgp:SCREEN-VALUE > "4" THEN DO:
   MESSAGE "C¢digo de Recomenda‡Æo de Lavagem deve estar entre 1 e 4." VIEW-AS ALERT-BOX.
   APPLY 'ENTRY' TO wh-rlgp.
   RETURN NO-APPLY.
END.
