DEF VAR i-ct AS INT.
FOR EACH ob-etiqueta WHERE
         ob-etiqueta.cod-estabel = '5' AND
         ob-etiqueta.situacao = 3 AND 
         ob-etiqueta.localizacao <> '' NO-LOCK.
    ASSIGN i-ct = i-ct + 1.
END.

MESSAGE i-ct
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
/*
Localizadas = 5792
Sem Localizacao = 994140
*/ 
