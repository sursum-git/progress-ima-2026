DEF VAR tot-icms AS DEC.
OUTPUT TO c:/temp/it-nota-fisc.csv CONVERT SOURCE "ibm850".
PUT "Nota/Item;Num.NF/Item;Serie/Produto;Data;Cliente;Valor;Val.ICMS" SKIP.
FOR EACH nota-fiscal WHERE nota-fiscal.cod-estabel  = '2'
                       AND nota-fiscal.serie        = '1'
                       AND nota-fiscal.dt-emis-nota >= 05/01/2005
                       AND nota-fiscal.dt-emis-nota <= 04/30/2010
                       AND nota-fiscal.dt-cancela   =  ?
                       AND nota-fiscal.esp-docto    = 22 /* nfs */
                     NO-LOCK:
    FIND FIRST it-nota-fisc OF nota-fiscal NO-LOCK.
    FIND ITEM WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK.
    IF ITEM.ge-codigo >= 50 AND ITEM.ge-codigo <= 59 THEN DO:
       FIND emitente WHERE emitente.cod-emitente = nota-fiscal.cod-emitente
                     NO-LOCK.
       ASSIGN tot-icms = 0.
       FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK:
           ASSIGN tot-icms = tot-icms + it-nota-fisc.vl-icms-it.
       END.
       PUT "Nota" ";"
           nota-fiscal.nr-nota-fis ";"
           nota-fiscal.serie ";"
           nota-fiscal.dt-emis-nota ";"
           emitente.nome-emit ";"
           nota-fiscal.vl-tot-nota ";"
           tot-icms
           SKIP.
        FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK:
            FIND ITEM WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK.
            PUT "Item" ";"
                it-nota-fisc.it-codigo ";"
                ITEM.desc-item
                SKIP.
        END.
    END.
END.
OUTPUT CLOSE.
