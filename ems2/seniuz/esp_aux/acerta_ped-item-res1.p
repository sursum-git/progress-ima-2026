DEF BUFFER b-ped-item-res FOR ped-item-res.

FOR EACH ped-item-res NO-LOCK.
    FIND last b-ped-item-res 
         WHERE b-ped-item-res.nome-abrev = ped-item-res.nome-abrev
           AND b-ped-item-res.nr-pedcli  = ped-item-res.nr-pedcli
           AND b-ped-item-res.it-codigo  = ped-item-res.it-codigo
           AND b-ped-item-res.cod-refer  = ped-item-res.cod-refer
           AND b-ped-item-res.nr-sequencia = ped-item-res.nr-sequencia
           AND RECID(b-ped-item-res) > RECID(ped-item-res)
         NO-LOCK NO-ERROR.
    FIND ped-venda WHERE ped-venda.nome-abrev = ped-item-res.nome-abrev
                     AND ped-venda.nr-pedcli  = ped-item-res.nr-pedcli
                   NO-LOCK NO-ERROR.
    IF AVAIL b-ped-item-res THEN
    DISP ped-item-res.nome-abrev
         ped-item-res.nr-pedcli
         RECID(ped-item-res)
         RECID(b-ped-item-res)
         ped-venda.cod-sit-ped WHEN AVAIL ped-venda VIEW-AS FILL-IN.
END.
