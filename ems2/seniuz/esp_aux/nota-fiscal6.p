OUTPUT TO "c:/lixo/lixo.csv" CONVERT SOURCE "ibm850".
PUT "Cliente;Nome-Abrev;Dt-emissao;Repres;UF;Item;Descricao;Quantidade" SKIP.
FOR EACH nota-fiscal WHERE nota-fiscal.cod-estabel  =  "2"
                     /*AND nota-fiscal.serie        =  "1"
                       AND nota-fiscal.dt-emis-nota >= 01/01/2006*/
                       AND nota-fiscal.dt-cancela    = ?
                       AND nota-fiscal.esp-docto     = 22
                       AND nota-fiscal.emite-dup     = YES
                     NO-LOCK,
    EACH it-nota-fisc OF nota-fiscal NO-LOCK:
    FIND emitente WHERE emitente.cod-emitente = nota-fiscal.cod-emitente NO-LOCK NO-ERROR.
    FIND ITEM WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK NO-ERROR.
    PUT nota-fiscal.cod-emitente ";"
        emitente.nome-abrev ";"
        nota-fiscal.dt-emis-nota ";"
        nota-fiscal.no-ab-reppri ";"
        emitente.estado ";"
        it-nota-fisc.it-codigo ";"
        ITEM.desc-item ";"
        it-nota-fisc.qt-faturada[1]
        SKIP.
END.
OUTPUT CLOSE.
