DEF NEW GLOBAL SHARED VAR wh-peso-liq      AS widget-handle no-undo.
DEF NEW GLOBAL SHARED VAR wh-fator-conv    AS widget-handle no-undo.

ASSIGN wh-fator-conv:SCREEN-VALUE  = STRING(1 / DECIMAL(wh-peso-liq:SCREEN-VALUE)).


