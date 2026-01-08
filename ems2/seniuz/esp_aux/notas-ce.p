OUTPUT TO "c:\lixo\notas-ce.csv".
FOR EACH nota-fiscal WHERE nota-fiscal.cod-estabel = "2"
                       AND nota-fiscal.serie       = "1"
                       AND nota-fiscal.dt-cancela  = ?
                       AND nota-fiscal.estado      = "ce"
                     NO-LOCK.
    FIND emitente WHERE emitente.cod-emitente = nota-fiscal.cod-emitente NO-LOCK.
    put nota-fiscal.dt-emis-nota ";"
        nota-fiscal.cidade-cif ";"
        nota-fiscal.cidade ";"
        emitente.nome-abrev ";"
        nota-fiscal.nome-transp
        SKIP.
END.
OUTPUT CLOSE.
