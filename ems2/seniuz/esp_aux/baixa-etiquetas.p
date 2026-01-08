DEF TEMP-TABLE tt-aux
    FIELD c-linha AS CHAR FORMAT "x(35)".

DEF VAR i-etq LIKE ob-etiqueta.num-etiqueta.

INPUT FROM c:\lixo\inventario2.txt.
REPEAT.
    CREATE tt-aux.
    IMPORT tt-aux.
END.

FOR EACH tt-aux.
    
    ASSIGN i-etq = int(substr(tt-aux.c-linha,7,9)).

    FIND ob-etiqueta WHERE
         ob-etiqueta.num-etiqueta = i-etq
         SHARE-LOCK NO-ERROR.

    IF NOT AVAIL ob-etiqueta THEN DO.
       MESSAGE 'Etiqueta' i-etq 'n∆o cadastrada...'
           VIEW-AS ALERT-BOX INFO BUTTONS OK.

    END.
    ELSE
       ASSIGN ob-etiqueta.situacao = 5. 
END.


