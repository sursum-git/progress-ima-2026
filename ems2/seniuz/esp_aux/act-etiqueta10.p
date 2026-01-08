FOR EACH ob-etiqueta WHERE
         ob-etiqueta.situacao = 2.

    IF ob-etiqueta.localiz = '' THEN NEXT.
    FIND ped-item-rom WHERE
         ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta
         NO-LOCK NO-ERROR.

    IF AVAIL ped-item-rom THEN DO.
       ASSIGN ob-etiqueta.situacao = 4.
       FIND ped-item-res WHERE
            ped-item-res.nr-pedcli = ped-item-rom.nr-pedcli AND
            ped-item-res.nome-abrev = ped-item-rom.nome-abrev AND
            ped-item-res.nr-sequencia = ped-item-rom.nr-sequencia
            NO-LOCK NO-ERROR.
       IF AVAIL ped-item-res  AND
          ped-item-res.faturado = YES THEN
          ASSIGN ob-etiqueta.situacao = 5.

    END.
    ELSE
        ASSIGN ob-etiqueta.situacao = 3. 
END.
