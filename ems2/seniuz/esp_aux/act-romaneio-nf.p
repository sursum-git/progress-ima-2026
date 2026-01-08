FOR EACH nota-fiscal WHERE
        nota-fiscal.cod-estab         = '2' AND
        nota-fiscal.serie             = '1' AND
        nota-fiscal.nr-nota-fis       = '0112268' NO-LOCK.
   
       FOR EACH ped-item-res WHERE
                ped-item-res.cod-estabel = nota-fiscal.cod-estabel AND
                ped-item-res.serie       = nota-fiscal.serie AND
                ped-item-res.nr-nota-fis = INT(nota-fiscal.nr-nota-fis) AND
                ped-item-res.faturado    = YES NO-LOCK
                BY ped-item-res.nr-sequencia.
   
           FOR EACH ped-item-rom WHERE
                    ped-item-rom.nome-abrev = ped-item-res.nome-abrev AND
                    ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli AND
                    ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia
                    BREAK BY ped-item-rom.nr-volume.
   
               FIND ob-etiqueta WHERE
                    ob-etiqueta.nr-ob = ped-item-rom.nr-ob AND
                    ob-etiqueta.nr-seq =  ped-item-rom.nr-seq-etq

/*                    ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta */
                    NO-LOCK NO-ERROR.

               ASSIGN ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta.
           END.
       END.
END.
