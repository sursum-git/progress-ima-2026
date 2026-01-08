FOR EACH ped-item WHERE (ped-item.it-codigo = "500271" OR 
                         ped-item.it-codigo = "500272") NO-LOCK,
    EACH ped-venda OF ped-item WHERE ped-venda.dt-implant >= 01/01/2007
                                 AND ped-venda.dt-implant <= 01/31/2007
                               NO-LOCK.
    DISP ped-item.qt-pedida(TOTAL).
END.

FOR EACH ped-item WHERE (ped-item.it-codigo = "500271" OR 
                         ped-item.it-codigo = "500272")
                    AND ped-item.dt-canseq >= 01/01/2007
                    AND ped-item.dt-canseq <= 01/31/2007
                               NO-LOCK.
    DISP ped-item.nr-pedcli
         ped-item.nome-abrev
         ped-item.dt-canseq
         ped-item.qt-pedida(TOTAL).
END.
