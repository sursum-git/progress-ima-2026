DEF VAR de-qt-res AS DEC.       

FOR EACH ped-item-res WHERE
        ped-item-res.faturado = NO.

    IF ped-item-res.lote BEGINS 'ca' OR 
       ped-item-res.lote BEGINS 'sc' THEN NEXT.

    ASSIGN de-qt-res = 0.
    FOR EACH ped-item-rom WHERE
             ped-item-rom.nome-abrev = ped-item-res.nome-abrev AND
             ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli AND
             ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia 
             NO-LOCK.
       
        FIND ob-etiqueta WHERE
             ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
             NO-LOCK NO-ERROR.

        ASSIGN de-qt-res = de-qt-res + ob-etiqueta.quantidade.
    END.

    IF de-qt-res <> ped-item-res.qt-pedida THEN DO.
       DISP ped-item-res.nr-pedcli
            ped-item-res.nome-abrev
            ped-item-res.nr-sequencia
            ped-item-res.qt-pedida
            ped-item-res.lote
            de-qt-res.
    END.
END.
