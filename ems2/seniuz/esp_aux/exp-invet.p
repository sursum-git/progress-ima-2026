OUTPUT TO m:\inv-502181.csv.
FOR EACH inv-acab WHERE
         inv-acab.it-codigo = '502181' AND
         inv-acab.cod-refer = '0101010' and
         inv-acab.lote BEGINS "RP" NO-LOCK.
    PUT inv-acab.it-codigo ";" 
        inv-acab.cod-refer ";" 
        inv-acab.lote ";" 
        inv-acab.docto ";" 
        inv-acab.qtd-inv ";"
        SKIP.
END.
