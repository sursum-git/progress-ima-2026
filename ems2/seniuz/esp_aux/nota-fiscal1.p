OUTPUT TO "c:/lixo/nota-fiscal.csv" CONVERT SOURCE "ibm850".
PUT "Nr.Nota;" "Dt.EmissÆo;" "Cliente;" "CC;" "Composi‡Æo;" "Item;" 
    "Descri‡Æo;" "Valor Item;" "Valor ICMS"
    SKIP.

FOR EACH nota-fiscal WHERE nota-fiscal.cod-estabel  =  "2"
                       AND nota-fiscal.serie        =  "1"
                       AND nota-fiscal.dt-emis-nota >= 01/01/2007
                       AND nota-fiscal.dt-emis-nota <= 01/31/2007
                       AND nota-fiscal.dt-cancela   =  ?
                     NO-LOCK,
    EACH it-nota-fisc OF nota-fiscal NO-LOCK,
    EACH item-ext WHERE item-ext.it-codigo = it-nota-fisc.it-codigo 
                    AND (item-ext.cod-composi = "05" OR 
                         item-ext.cod-composi = "12" OR 
                         item-ext.cod-composi = "18" OR 
                         item-ext.cod-composi = "21" OR 
                         item-ext.cod-composi = "30" OR 
                         item-ext.cod-composi = "40" OR
                         item-ext.cod-composi = "43" OR
                         item-ext.cod-composi = "48") NO-LOCK:

    
    FIND emitente WHERE emitente.cod-emitente = nota-fiscal.cod-emitente NO-LOCK.
    FIND ITEM WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK.
    FIND composi WHERE composi.cod-composi = item-ext.cod-composi NO-LOCK.

    PUT nota-fiscal.nr-nota-fis ";"
        nota-fiscal.dt-emis-nota ";"
        emitente.nome-emit ";"
        item-ext.cod-composi ";"
        composi.descricao ";"
        it-nota-fisc.it-codigo ";"
        ITEM.desc-item FORMAT "x(36)" ";"
        it-nota-fisc.vl-tot-item ";"
        it-nota-fisc.vl-icms-it
        SKIP.
END.
OUTPUT CLOSE.
