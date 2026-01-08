FOR EACH ped-venda WHERE
         ped-venda.cod-sit-ped = 1.

    FIND ped-venda-ext WHERE
         ped-venda-ext.nr-pedido = ped-venda.nr-pedido
         NO-LOCK NO-ERROR.

    IF AVAIL ped-venda-ext THEN NEXT.

    CREATE ped-venda-ext.
    ASSIGN ped-venda-ext.cod-estabel = ped-venda.cod-estabel
           ped-venda-ext.nr-pedido = ped-venda.nr-pedido
           ped-venda-ext.nome-abrev = ped-venda.nome-abrev
           ped-venda-ext.tp-pedido = 'Normal'
           ped-venda-ext.tp-frete = 'CIF Total'
           ped-venda-ext.tp-entrega = 'Na Data'
           ped-venda-ext.tp-pagto = 'Normal'.
END.
