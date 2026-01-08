FOR EACH transporte WHERE transporte.nome MATCHES "*desat*" 
                       OR transporte.nome MATCHES "*nao*" 
                       OR transporte.nome MATCHES "*excl*" NO-LOCK.
    DISP transporte.cod-transp
     transporte.nome.

    /*
    FOR EACH ped-venda WHERE ped-venda.nome-transp = transporte.nome-abrev 
                         AND (ped-venda.cod-sit-ped < 3 or
                              ped-venda.cod-sit-ped = 5) NO-LOCK:
        DISP transporte.cod-transp
             transporte.nome
             ped-venda.nome-abrev
             ped-venda.nr-pedcli.
    END.
    */
END.
