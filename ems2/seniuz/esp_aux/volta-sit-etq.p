DEF VAR i-ct AS INT.
DEF VAR c-progressivo AS CHAR FORMAT "x(30)".

INPUT FROM "M:\EMS206\especificos\Seniuz\Seguran‡a\etq-sem-saldo.txt" NO-ECHO.
REPEAT WITH WIDTH 550.
    SET c-progressivo.

    FIND bc-etiqueta WHERE
         bc-etiqueta.progressivo = c-progressivo NO-ERROR.
    
    ASSIGN bc-etiqueta.cod-estado = 2.

    ASSIGN i-ct = i-ct + 1.
    DISP i-ct.
    PAUSE 0.

END.
