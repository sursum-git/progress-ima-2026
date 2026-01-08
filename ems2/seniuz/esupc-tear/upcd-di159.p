DEFINE PARAMETER BUFFER p-table FOR ped-venda.

FOR EACH ped-item OF p-table.
    FIND ped-item-res WHERE 
         ped-item-res.nome-abrev   = ped-item.nome-abrev AND 
         ped-item-res.nr-pedcli    = ped-item.nr-pedcli AND 
         ped-item-res.it-codigo    = ped-item.it-codigo AND 
         ped-item-res.cod-refer    = ped-item.cod-refer AND
         ped-item-res.nr-sequencia = ped-item.nr-sequencia 
         NO-ERROR.

    IF AVAIL ped-item-res THEN DO.
       MESSAGE 'Este Pedido possui Itens Reservados, Imposs¡vel Elimina-lo...' VIEW-AS ALERT-BOX ERROR.
       RETURN 'NOK'.
    END.
END.

FIND ped-venda-ext WHERE
     ped-venda-ext.nr-pedido = p-table.nr-pedido NO-ERROR.
IF AVAIL ped-venda-ext THEN
   DELETE ped-venda-ext.
