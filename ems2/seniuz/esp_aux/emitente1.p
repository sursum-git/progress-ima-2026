OUTPUT TO c:/temp/emitente_sp.csv.

FOR EACH emitente WHERE emitente.estado = "sp"
                    AND emitente.cidade = "s∆o paulo"
                    AND emitente.identific <> 2
                  NO-LOCK.
    PUT UNFORMAT 
        emitente.cod-emitente ";"
        emitente.nome-emit ";"
        "*" + emitente.telefone[1] ";"
        "*" + emitente.telefone[2] ";"
        emitente.cidade 
        SKIP.
END.
OUTPUT CLOSE.
