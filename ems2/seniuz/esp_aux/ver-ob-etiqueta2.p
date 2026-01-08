DEF BUFFER b-ped-item-rom FOR ped-item-rom.

OUTPUT TO "c:/lixo/duplicidade.csv".
PUT "Pedido;Nome-Abrev;Seq-Ped;Num-OB;Seq-Etq;NF" SKIP.

FOR EACH ped-item-rom BY ped-item-rom.nr-ob
                      BY ped-item-rom.nr-seq-etq.
    FIND b-ped-item-rom WHERE
         b-ped-item-rom.nr-ob = ped-item-rom.nr-ob AND
         b-ped-item-rom.nr-seq-etq = ped-item-rom.nr-seq-etq
         NO-LOCK NO-ERROR.

    IF AMBIGUOUS b-ped-item-rom THEN DO:
       FOR EACH b-ped-item-rom WHERE
                b-ped-item-rom.nr-ob = ped-item-rom.nr-ob AND
                b-ped-item-rom.nr-seq-etq = ped-item-rom.nr-seq-etq
                NO-LOCK.

           FIND ped-item-res WHERE 
                ped-item-res.nome-abrev   = b-ped-item-rom.nome-abrev   AND
                ped-item-res.nr-pedcli    = b-ped-item-rom.nr-pedcli    AND
                ped-item-res.nr-sequencia = b-ped-item-rom.nr-sequencia  
                NO-LOCK NO-ERROR.                                            

           IF AVAIL ped-item-res AND
              ped-item-res.faturado = YES THEN
              PUT b-ped-item-rom.nr-pedcli ";"
                  b-ped-item-rom.nome-abrev ";"
                  b-ped-item-rom.nr-sequencia ";"
                  b-ped-item-rom.nr-ob ";"
                  b-ped-item-rom.nr-seq-etq ";"
                  IF AVAIL ped-item-res THEN STRING(ped-item-res.nr-nota-fis)
                                        ELSE ""
                  SKIP.
       END.
    END.
END.
OUTPUT CLOSE.
