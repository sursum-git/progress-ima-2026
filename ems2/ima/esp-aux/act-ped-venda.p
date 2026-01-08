FIND PED-VENDA 206857.

CREATE ped-venda-ext.
ASSIGN ped-venda-ext.nr-pedido = ped-venda.nr-pedido
       ped-venda-ext.tp-pedido = 'Opera‡Æo Triangular'
       ped-venda-ext.nome-abrev = ped-venda.nome-abrev
       ped-venda-ext.tp-frete = 'CIF'
       ped-venda-ext.tp-pagto = 'Normal'.

FOR EACH ped-item OF ped-venda.
    CREATE ped-item-ext.
    ASSIGN ped-item-ext.cod-estabel  = ped-venda.cod-estabel  /* daf */
           ped-item-ext.nome-abrev   = ped-item.nome-abrev
           ped-item-ext.nr-pedcli    = ped-item.nr-pedcli
           ped-item-ext.nr-sequencia = ped-item.nr-sequencia
           ped-item-ext.it-codigo    = ped-item.it-codigo
           ped-item-ext.cod-refer    = ped-item.cod-refer
           ped-item-ext.retirar-corte = NO
           ped-item-ext.reservado    = NO
           ped-item-ext.lote         = 'RP' + ped-item.cod-refer.
END.
