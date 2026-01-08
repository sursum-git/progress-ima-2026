FIND nota-fiscal WHERE 
     nota-fiscal.cod-estabel = '5' AND
     nota-fiscal.serie = '3' AND
     nota-fiscal.nr-pedcli = '210666'.

   FOR EACH ped-item-res WHERE
            ped-item-res.cod-estabel = nota-fiscal.cod-estabel AND
            ped-item-res.serie       = nota-fiscal.serie AND
            ped-item-res.nr-pedcli = nota-fiscal.nr-pedcli.
            
       ASSIGN ped-item-res.serie       = ""
              ped-item-res.nr-nota-fis = 0
              ped-item-res.faturado    = NO.

       FOR EACH ped-item-rom WHERE
                ped-item-rom.nome-abrev = ped-item-res.nome-abrev AND
                ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli AND
                ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia
                BREAK BY ped-item-rom.nr-volume.

           FIND ob-etiqueta WHERE
                ob-etiqueta.cod-estabel = ped-item-rom.cod-estabel AND
                ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                SHARE-LOCK NO-ERROR.
           IF AVAIL ob-etiqueta THEN
              ASSIGN ob-etiqueta.situacao = 3.

           DELETE ped-item-rom.
       END.
       DELETE ped-item-res.
   END.
/* Fim Seniuz */ 


