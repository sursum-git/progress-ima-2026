FIND ped-venda WHERE
     ped-venda.nr-pedcli = '124367' NO-LOCK NO-ERROR.
IF AVAIL ped-venda THEN DO:
   FIND ped-venda-ext WHERE
        ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-ERROR.
   IF AVAIL ped-venda-ext THEN DO:
       ASSIGN ped-venda-ext.tp-pedido = 'Bonifica‡Æo'.
       DISP ped-venda-ext.nr-pedcli
            ped-venda-ext.nr-pedido
            ped-venda-ext.tp-pedido.
   END.
END.
