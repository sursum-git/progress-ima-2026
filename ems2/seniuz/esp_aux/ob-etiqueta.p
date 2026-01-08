OUTPUT TO c:/temp/Prod_novembro.csv.
FOR EACH ob-etiqueta WHERE ob-etiqueta.dt-emissao >= 11/01/2008
                       AND ob-etiqueta.dt-emissao <= 11/30/2008 NO-LOCK
                     BREAK BY ob-etiqueta.it-codigo
                           BY ob-etiqueta.cod-qualid
                           BY ob-etiqueta.nuance:
    ACCUMULATE ob-etiqueta.quantidade (TOTAL BY ob-etiqueta.nuance).
                   
    IF LAST-OF(ob-etiqueta.nuance) THEN DO:
       FIND ITEM WHERE ITEM.it-codigo = ob-etiqueta.it-codigo NO-LOCK.
       PUT UNFORMAT 
           ITEM.it-codigo ";"
           ITEM.desc-item ";"
           ob-etiqueta.cod-qualid ";"
           ob-etiqueta.nuance ";"
           (ACCUM TOTAL BY ob-etiqueta.nuance ob-etiqueta.quantidade)
           SKIP.
    END.
END.
OUTPUT CLOSE.
