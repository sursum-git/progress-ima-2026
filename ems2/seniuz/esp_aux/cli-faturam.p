DEF VAR de-total AS DEC FORMAT ">>>,>>>,>>9.99".
OUTPUT TO "c:/temp/lixo.txt".
FOR EACH nota-fiscal WHERE nota-fiscal.cod-estabel  = "2"
                  AND nota-fiscal.serie        = "1"
                  AND nota-fiscal.dt-emis-nota >= 01/01/2004
                  AND nota-fiscal.dt-emis-nota <= 12/31/2004
                  AND nota-fiscal.emite-dup     = YES
                NO-LOCK,
    EACH emitente OF nota-fiscal NO-LOCK
   
    BREAK BY emitente.lim-credito:

    ASSIGN de-total = de-total + nota-fiscal.vl-tot-nota.

    IF LAST-OF(emitente.lim-credito) THEN DO:
       DISP emitente.lim-credito
            de-total(TOTAL).
       ASSIGN de-total = 0.
    END.
END.
OUTPUT CLOSE.
