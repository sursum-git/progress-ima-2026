OUTPUT TO "c:\lixo\item.csv" CONVERT SOURCE "ibm850".

FOR EACH ITEM WHERE ITEM.ge-codigo >= 50
                AND ITEM.ge-codigo <= 59
              NO-LOCK,
    EACH item-ext OF ITEM NO-LOCK:

    PUT ITEM.it-codigo ";"
        ITEM.desc-item ";"
        ITEM-ext.largura ";"
        ITEM.class-fiscal ";"
        ITEM.un ";"
        ITEM.ge-codigo ";"
        ITEM.fm-codigo ";"
        ITEM.cod-estab ";"
        ITEM-ext.cod-composi ";"
        ITEM.peso-liquido ";"
        ITEM.ind-item-fat
        SKIP.
END.
OUTPUT CLOSE.
