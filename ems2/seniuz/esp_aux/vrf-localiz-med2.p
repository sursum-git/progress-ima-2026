DEF VAR i-ct AS INT.
DEF VAR i-ct2 AS INT.

FOR EACH ob-etiqueta WHERE
         ob-etiqueta.cod-estabel = '5' AND 
         ob-etiqueta.situacao = 3 NO-LOCK.

    IF ob-etiqueta.localizacao = '' THEN
       ASSIGN i-ct = i-ct + 1.
    ELSE
       ASSIGN i-ct2 = i-ct2 + 1.
END.

MESSAGE i-ct SKIP
        i-ct2 SKIP
        i-ct + i-ct2 
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
