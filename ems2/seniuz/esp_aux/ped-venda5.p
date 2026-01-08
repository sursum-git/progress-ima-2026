FOR EACH ped-venda WHERE ped-venda.cod-sit-ped < 3 OR 
                         ped-venda.cod-sit-ped = 5 NO-LOCK.
    FIND transporte WHERE transporte.nome-abrev = ped-venda.nome-transp NO-LOCK.
    IF transporte.nome MATCHES "*desativado*" THEN
       DISP ped-venda.nome-abrev
            ped-venda.nr-pedcli
            ped-venda.nome-transp
            transporte.nome WITH WIDTH 300.
END.
