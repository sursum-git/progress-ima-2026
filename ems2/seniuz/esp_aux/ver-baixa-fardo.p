DEFINE TEMP-TABLE tt-baixa 
       FIELD dt-baixa   LIKE mp-fardo.dt-baixa 
       FIELD peso       AS DEC
       INDEX indice1 dt-baixa.

OUTPUT TO c:\lixo\baixa-dez.txt.

FOR EACH mp-fardo WHERE
         mp-fardo.situacao = 4 AND
         MONTH(mp-fardo.dt-baixa) = 12 AND
         YEAR(mp-fardo.dt-baixa) = 2010 NO-LOCK.
    FIND tt-baixa WHERE
         tt-baixa.dt-baixa = mp-fardo.dt-baixa NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-baixa THEN DO:
       CREATE tt-baixa.
       ASSIGN tt-baixa.dt-baixa = mp-fardo.dt-baixa.
    END.
    ASSIGN tt-baixa.peso = tt-baixa.peso + mp-fardo.peso.
END.
FOR EACH tt-baixa NO-LOCK.
    DISP tt-baixa.dt-baixa
         tt-baixa.peso.
END.
