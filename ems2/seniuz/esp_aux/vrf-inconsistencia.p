FIND ped-venda  140483.

FOR EACH ped-item OF ped-venda.
    FIND ped-item-res OF ped-item NO-LOCK NO-ERROR.
    FOR EACH ped-item-rom WHERE
             ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli AND
             ped-item-rom.nome-abrev = ped-item-res.nome-abrev AND
             ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia NO-LOCK.

        FIND ob-etiqueta WHERE
             ob-etiqueta.cod-estabel = ped-venda.cod-estabel AND
             ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
             NO-LOCK NO-ERROR.

        IF NOT AVAIL ob-etiqueta THEN DO.
           DISP ped-item-rom.num-etiqueta
                'FOI ELIMINADA'
                WITH WIDTH 550.
           NEXT.
        END.

        IF ob-etiqueta.situacao <> 4 THEN DO.
           DISP ped-item-rom.num-etiqueta
                'ALTERADA A SITUACAO DE RESERVADA PARA ESTOQUE'
                WITH WIDTH 550.
           NEXT.
        END.
    END.
END.



