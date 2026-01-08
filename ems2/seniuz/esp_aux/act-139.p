FOR EACH nota-fiscal WHERE
         nota-fiscal.dt-emis >= 02.01.2012  NO-LOCK.

    DISP nota-fiscal.nr-nota-fis.
    PAUSE 0.

   FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.
       FOR EACH ped-item-res WHERE 
                ped-item-res.nome-abrev    = nota-fiscal.nome-ab-cli AND 
                ped-item-res.nr-pedcli     = it-nota-fisc.nr-pedcli AND
                ped-item-res.it-codigo     = it-nota-fisc.it-codigo AND 
                ped-item-res.nr-sequencia  = it-nota-fisc.nr-seq-ped.

           ASSIGN ped-item-res.faturado    = YES 
                  ped-item-res.cod-estabel = nota-fiscal.cod-estabel
                  ped-item-res.serie       = nota-fiscal.serie
                  ped-item-res.nr-nota-fis = INT(nota-fiscal.nr-nota-fis).

           FOR EACH ped-item-rom WHERE
                    ped-item-rom.nome-abrev = ped-item-res.nome-abrev AND
                    ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli AND
                    ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia
                    EXCLUSIVE-LOCK.
               FIND ob-etiqueta WHERE
                    ob-etiqueta.cod-estabel = ped-item-rom.cod-estabel AND
                    ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                    SHARE-LOCK NO-ERROR.
               IF AVAIL ob-etiqueta THEN
                  ASSIGN ob-etiqueta.situacao = 5.
           END.
       END.
   END.
END.

