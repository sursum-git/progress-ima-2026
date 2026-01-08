FUNCTION fn-calc-digito RETURNS CHAR
    ( INPUT c-num-calc AS CHAR):

    DEF VAR i-soma AS INT.
    DEF VAR i-ct AS INT.
    DEF VAR i-dig AS INT.

    DO i-ct = 1 TO LENGTH(c-num-calc).
       IF i-ct MODULO 2 = 0 THEN DO.
          ASSIGN i-soma = i-soma + INT(SUBSTR(c-num-calc,i-ct,1)) * 3. 

       END.
       ELSE DO.
          ASSIGN i-soma = i-soma + INT(SUBSTR(c-num-calc,i-ct,1)). 
       END.
    END.

    ASSIGN i-dig = IF i-soma MODULO 10 <> 0 
                   THEN 10 - (i-soma MODULO 10)
                   ELSE i-soma MODULO 10.

    RETURN STRING(i-dig).
END.


DEF VAR i-num-bar AS INT.

FIND ob-etiqueta WHERE
     ob-etiqueta.num-etiqueta = 115443.

ASSIGN i-num-bar = INT(STRING(ob-etiqueta.num-etiqueta) + fn-calc-digito(INPUT STRING(ob-etiqueta.num-etiqueta,"999999999"))).

MESSAGE i-num-bar
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


