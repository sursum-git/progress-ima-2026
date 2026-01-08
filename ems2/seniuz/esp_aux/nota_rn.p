DEF VAR l-prim AS LOG.
OUTPUT TO "c:\lixo\notas_rn.txt".
FOR EACH nota-fiscal WHERE nota-fiscal.estado = "rn"
                       AND nota-fiscal.dt-cancela = ?
                       AND nota-fiscal.cod-estabel = "2"
                       AND nota-fiscal.serie = "1"
                     NO-LOCK.
    FIND emitente OF nota-fiscal NO-LOCK.

    PUT emitente.nome-emit ";"
        emitente.cgc ";"
        emitente.ins-estadual ";"
        nota-fiscal.nr-nota-fis ";"
        nota-fiscal.dt-emis-nota ";"
        nota-fiscal.vl-tot-nota ";".

    ASSIGN l-prim = YES.
    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK:
        FIND ITEM OF it-nota-fisc NO-LOCK.

        IF NOT l-prim THEN
           PUT ";;;;;;".
        ELSE
           ASSIGN l-prim = NO.
        PUT it-nota-fisc.it-codigo ";"
            ITEM.desc-item FORMAT "x(36)" ";"
            it-nota-fisc.qt-faturada[1] ";"
            it-nota-fisc.vl-bicms-it ";"
            it-nota-fisc.vl-icms-it
            SKIP.
    END.
END.
OUTPUT CLOSE.
