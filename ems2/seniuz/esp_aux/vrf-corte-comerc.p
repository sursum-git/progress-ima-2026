OUTPUT TO c:\temp\erro-corte.txt.

PUT "Cliente       Pedido     Sit      Seq  Item         Refer               Quant  Acond"
     SKIP.
FOR EACH ped-venda WHERE
         ped-venda.cod-sit-ped <= 5 AND 
         ped-venda.cod-sit-ped <> 3 NO-LOCK.

    FOR EACH ped-item OF ped-venda.
        FIND ped-item-ext OF ped-item NO-LOCK NO-ERROR.

        IF NOT AVAIL ped-item-ext THEN NEXT.

        FIND corte-comerc WHERE
             corte-comerc.descricao = ped-item-ext.acondicionamento
             NO-LOCK NO-ERROR.

        IF NOT AVAIL corte-comerc THEN
           PUT ped-venda.nome-abrev
               ped-venda.nr-pedcli    AT 15
               ped-venda.cod-sit-ped  AT 27
               ped-item.nr-sequencia  AT 32
               ped-item.it-codigo     AT 40 FORMAT "x(10)"
               ped-item.cod-refer     AT 53
               ped-item.qt-pedida     AT 65
               ped-item-ext.acond     AT 80
               SKIP.
               
    END.
END.
