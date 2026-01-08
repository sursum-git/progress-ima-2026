FOR EACH inv-acab WHERE inv-acab.it-codigo BEGINS "5" 
                    AND SUBSTR(STRING(inv-acab.docto),1,4) <> "1459"
                  NO-LOCK.
    DISP inv-acab.it-codigo
         inv-acab.cod-refer
         inv-acab.qtd-inv(TOTAL)
         inv-acab.docto(COUNT)
         inv-acab.nr-ob
         inv-acab.nr-seq-etq.
END.
