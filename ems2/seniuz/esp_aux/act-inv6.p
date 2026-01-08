FOR EACH ob-etiqueta WHERE
         ob-etiqueta.situacao = 5 AND
         ob-etiqueta.dt-fatur = TODAY.

    FIND ped-item-rom WHERE
         ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta NO-LOCK NO-ERROR.

    IF AVAIL ped-item-rom THEN DO.
       FIND FIRST ped-item-res WHERE
             ped-item-res.nome-abrev   = ped-item-rom.nome-abrev AND
             ped-item-res.nr-pedcli    = ped-item-rom.nr-pedcli  AND
             ped-item-res.nr-sequencia = ped-item-rom.nr-sequencia NO-LOCK NO-ERROR.
    
       IF AVAIL ped-item-res AND ped-item-res.faturado THEN NEXT.

       ASSIGN ob-etiqueta.situacao = 4.
    END.
    ELSE
       ASSIGN ob-etiqueta.situacao = 3.
END.
