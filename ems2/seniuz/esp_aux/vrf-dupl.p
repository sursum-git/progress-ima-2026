DEF BUFFER b-ped-item-rom FOR ped-item-rom.

OUTPUT TO c:\temp\dupl.txt.

FOR EACH ped-item-res WHERE
         ped-item-res.faturado = NO.

    FOR EACH b-ped-item-rom WHERE
             b-ped-item-rom.nome-abrev = ped-item-res.nome-abrev AND
             b-ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli AND
             b-ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia
             NO-LOCK.

        FIND ped-item-rom WHERE
             ped-item-rom.nr-ob = b-ped-item-rom.nr-ob AND
             ped-item-rom.nr-seq-etq = b-ped-item-rom.nr-seq-etq
             NO-LOCK NO-ERROR.

        IF AMBIGUOUS ped-item-rom THEN DO.
           FOR EACH ped-item-rom WHERE
                    ped-item-rom.nr-ob = b-ped-item-rom.nr-ob AND
                    ped-item-rom.nr-seq-etq = b-ped-item-rom.nr-seq-etq
                    NO-LOCK.
               FIND ob-etiqueta WHERE
                    ob-etiqueta.nr-ob = ped-item-rom.nr-ob AND
                    ob-etiqueta.nr-seq = ped-item-rom.nr-seq-etq
                    NO-LOCK NO-ERROR.

               PUT b-ped-item-rom.nr-ob ";"
                   b-ped-item-rom.nr-seq-etq ";"
                   ped-item-rom.nr-pedcli ";"
                   ped-item-rom.nome-abrev ";"
                   ped-item-res.it-codigo ";"
                   ped-item-res.cod-refer ";"
                   ob-etiqueta.num-etiqueta
                   SKIP.
           END.
        END.
    END.
END.
OUTPUT CLOSE.
