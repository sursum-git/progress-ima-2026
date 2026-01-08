DEF INPUT PARAMETER p-nr-pedcli AS CHAR.

FIND ped-venda WHERE
     ped-venda.nr-pedcli = p-nr-pedcli NO-LOCK NO-ERROR.

FIND ped-venda-ext WHERE
     ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND 
     ped-venda-ext.nr-pedido = ped-venda.nr-pedido
     SHARE-LOCK NO-ERROR.

ASSIGN ped-venda-ext.l-etiqueta = NO.

FOR EACH ped-item-res OF ped-venda SHARE-LOCK.
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
