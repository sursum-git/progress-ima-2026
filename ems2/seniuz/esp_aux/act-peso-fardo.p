DEF VAR i-fardo AS INT.
DEF VAR de-peso AS DEC FORMAT ">>>,>>>,>>9.99".
DEF VAR de-peso1 AS DEC FORMAT ">>>,>>>,>>9.99".
DEF VAR de-baixar AS DEC.

ASSIGN de-peso = 0.
FOR EACH mp-fardo WHERE mp-fardo.situacao = 3 NO-LOCK.
    ASSIGN i-fardo = i-fardo + 1
           de-peso = de-peso + mp-fardo.peso.
END.

ASSIGN de-baixar = round(4924.30 / i-fardo, 2).
FOR EACH mp-fardo WHERE mp-fardo.situacao = 3.
/*    ASSIGN mp-fardo.peso = mp-fardo.peso - de-baixar. */
END.

DISP 4924.30 - de-baixar * i-fardo LABEL "Falta Baixar"
     i-fardo LABEL "Total de Fardos"
     de-baixar LABEL "Abater por Fardo".
