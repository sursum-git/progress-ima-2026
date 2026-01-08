OUTPUT TO "c:\lixo\notas_frete_fob.csv".
PUT "Codigo;" "Nome Transportador;" "Data NF;" "Numero NF;" "Cliente;" "Cidade;" "UF;" "Valor NF"
    SKIP.
FOR EACH nota-fiscal WHERE nota-fiscal.cod-estabel  =  "2"
                       AND nota-fiscal.serie        =  "1"
                       AND nota-fiscal.dt-cancela   =  ?
                       AND nota-fiscal.dt-emis-nota >= 12/01/2005 
                       AND nota-fiscal.dt-emis-nota <= 01/31/2006
                       AND nota-fiscal.cidade-cif   =  ""
                     NO-LOCK
    BREAK BY nota-fiscal.nome-transp:

    IF FIRST-OF(nota-fiscal.nome-transp) THEN DO:
       FIND transporte WHERE transporte.nome-abrev = nota-fiscal.nome-transp NO-LOCK.
       PUT transporte.cod-transp ";"
           transporte.nome ";".
    END.
    ELSE
       PUT ";" ";".
    FIND emitente WHERE emitente.cod-emitente = nota-fiscal.cod-emitente NO-LOCK.
    PUT nota-fiscal.dt-emis-nota ";"
        nota-fiscal.nr-nota-fis ";"
        emitente.nome-emit ";"
        nota-fiscal.cidade ";"
        nota-fiscal.estado ";"
        nota-fiscal.vl-tot-nota
        SKIP.
    IF LAST-OF(nota-fiscal.nome-transp) THEN
       PUT " " SKIP.
END.
OUTPUT CLOSE.
