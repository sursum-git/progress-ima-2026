DEF VAR de-tot-qt AS DEC.

OUTPUT TO m:\ped-transp.txt.
FOR EACH ped-venda WHERE
         ped-venda.cod-sit-ped <= 2 NO-LOCK
         BY ped-venda.nome-transp
         BY ped-venda.dt-entrega.

    ASSIGN de-tot-qt = 0.
    FOR EACH ped-item OF ped-venda WHERE
             ped-item.cod-sit-item = 1 NO-LOCK.
        ASSIGN de-tot-qt = de-tot-qt + ped-item.qt-pedida.
    END.

    DISP ped-venda.nr-pedcli
         ped-venda.nome-abrev
         ped-venda.nome-transp
         ped-venda.dt-entrega
         de-tot-qt.
END.
OUTPUT CLOSE.

