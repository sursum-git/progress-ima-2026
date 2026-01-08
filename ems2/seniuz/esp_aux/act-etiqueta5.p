       FOR EACH ped-item-res.
           FOR EACH ped-item-rom WHERE
                    ped-item-rom.nome-abrev = ped-item-res.nome-abrev AND
                    ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli AND
                    ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia
                    BREAK BY ped-item-rom.nr-volume.

               FIND ob-etiqueta WHERE
                    ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                    SHARE-LOCK NO-ERROR.

               IF NOT AVAIL ob-etiqueta THEN NEXT.

               IF ped-item-res.faturado THEN
                  ASSIGN ob-etiqueta.situacao = 5.
               ELSE
                  ASSIGN ob-etiqueta.situacao = 4.
           END.
       END.
