OUTPUT TO c:/temp/pago-trans.csv.

FOR EACH mov-ap WHERE mov-ap.dt-transacao >= 01/01/2009
                  AND mov-ap.dt-transacao <= 12/31/2009
                NO-LOCK,
    EACH emitente WHERE emitente.cod-emitente = mov-ap.cod-fornec
                    AND emitente.cod-gr-forn  = 8
                  NO-LOCK
    BREAK BY mov-ap.cod-fornec:
    
    ACCUMULATE mov-ap.valor-mov (TOTAL BY mov-ap.cod-fornec).

    IF LAST-OF(mov-ap.cod-fornec) THEN DO.
       PUT emitente.cod-emitente ";"
           emitente.nome-emit ";"
           (ACCUM TOTAL BY mov-ap.cod-fornec mov-ap.valor-mov)
           SKIP.
    END.
END.
OUTPUT CLOSE.
