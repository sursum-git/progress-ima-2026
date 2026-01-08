OUTPUT TO "c:/lixo/nota-fiscal.csv" CONVERT SOURCE "ibm850".
PUT "Nr.Nota;" "Dt.EmissÆo;" "Cliente;" "CC;" "Composi‡Æo;" "Item;" 
    "Descri‡Æo;" "Valor Item;" "Valor ICMS"
    SKIP.

FOR EACH nota-fiscal WHERE nota-fiscal.cod-estabel  =  "2"
                       AND nota-fiscal.serie        =  "1"
                       AND nota-fiscal.dt-emis-nota >= 09/01/2002
                       AND nota-fiscal.dt-emis-nota <= 07/31/2004
                       AND nota-fiscal.dt-cancela   =  ?
                     NO-LOCK,
    EACH it-nota-fisc OF nota-fiscal NO-LOCK,
    EACH ITEM WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK,
    EACH familia-ext WHERE familia-ext.fm-codigo = item.fm-codigo
                       AND (int(familia-ext.cod-composi) = 8 OR 
                            int(familia-ext.cod-composi) = 16 OR 
                            INT(familia-ext.cod-composi) = 23 OR 
                            int(familia-ext.cod-composi) = 25 OR 
                            int(familia-ext.cod-composi) = 31 OR 
                            int(familia-ext.cod-composi) = 32 OR
                            int(familia-ext.cod-composi) = 41 OR
                            int(familia-ext.cod-composi) = 51) NO-LOCK:
    
    FIND emitente WHERE emitente.cod-emitente = nota-fiscal.cod-emitente NO-LOCK.
    FIND composi WHERE composi.cod-composi = familia-ext.cod-composi NO-LOCK.

    PUT nota-fiscal.nr-nota-fis ";"
        nota-fiscal.dt-emis-nota ";"
        emitente.nome-emit ";"
        familia-ext.cod-composi ";"
        composi.descricao ";"
        it-nota-fisc.it-codigo ";"
        ITEM.desc-item FORMAT "x(36)" ";"
        it-nota-fisc.vl-tot-item ";"
        it-nota-fisc.vl-icms-it
        SKIP.
END.
OUTPUT CLOSE.
