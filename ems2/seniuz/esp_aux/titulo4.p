OUTPUT TO c:/lixo/lixo.csv.
PUT "Emp;Est;Esp;Documento;Parc;Emitente" SKIP.
FOR EACH titulo NO-LOCK,
    EACH his-tit OF titulo WHERE his-tit.historico MATCHES "*Titulo em Ser.*" 
                           NO-LOCK:
    PUT titulo.ep-codigo ";"
        titulo.cod-estabel ";"
        titulo.cod-esp ";"
        titulo.nr-docto ";"
        titulo.parcela ";"
        titulo.cod-emitente
        SKIP.
END.
OUTPUT CLOSE.
