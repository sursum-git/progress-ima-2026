FOR EACH ped-item-res WHERE ped-item-res.cod-estabel = ''.
    FIND ped-venda WHERE ped-venda.nome-abrev = ped-item-res.nome-abrev
                     AND ped-venda.nr-pedcli  = ped-item-res.nr-pedcli
                   NO-LOCK.
    DISP ped-item-res.nr-pedcli
         ped-item-res.cod-estabel
         ped-venda.cod-estabel. 

   /*ASSIGN ped-item-res.cod-estabel = ped-venda.cod-estabel.*/
END.
