DEF VAR i-ct AS INT.
DEF VAR i-ct2 AS INT.
DEF VAR i-ct3 AS INT.

FOR EACH bc-etiqueta WHERE
         bc-etiqueta.cod-estado = 2 NO-LOCK.

    FIND ob-etiqueta WHERE
         ob-etiqueta.progressivo = bc-etiqueta.progressivo NO-LOCK NO-ERROR.

    IF AVAIL ob-etiqueta THEN 
       ASSIGN i-ct2 = i-ct2 + 1.
    ELSE
       ASSIGN i-ct3 = i-ct3 + 1.
END.

FOR EACH ob-etiqueta WHERE
         ob-etiqueta.cod-estabel = '5' AND 
         ob-etiqueta.situacao = 3 NO-LOCK.
    ASSIGN i-ct = i-ct + 1.
END.

MESSAGE 'BC' i-ct2 SKIP
        'OB' i-ct SKIP
        'BC sem OB' i-ct3
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

