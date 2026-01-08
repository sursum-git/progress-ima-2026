DEF VAR de-tot-rom AS DEC.

FOR EACH ped-item-res WHERE
         ped-item-res.faturado = NO NO-LOCK.

    FIND ped-item OF ped-item-res NO-LOCK NO-ERROR.

    IF ped-item.cod-sit-item <> 6 THEN NEXT.

    DISP ped-item-res.nr-pedcli
         ped-item.nr-sequencia
         ped-item-res.qt-pedida.
    
    ASSIGN de-tot-rom = 0.
    FOR EACH ped-item-rom WHERE
             ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli AND
             ped-item-rom.nome-abrev = ped-item-res.nome-abrev AND
             ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia NO-LOCK.

        FIND ob-etiqueta WHERE
             ob-etiqueta.cod-estab = '1' AND
             ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
             NO-LOCK NO-ERROR.

        DISP ped-item-rom.num-etiqueta
             ob-etiqueta.localiz.
    END.
    

END.
