DEF VAR de-qt-pedida AS DEC.
DEF VAR de-qt-faturada AS DEC.

FOR EACH ped-venda WHERE
         ped-venda.cod-sit-ped = 2.

    ASSIGN de-qt-pedida = 0.
    FOR EACH ped-item OF ped-venda.
        ASSIGN de-qt-pedida = de-qt-pedida + ped-item.qt-pedida.
    END.

    ASSIGN de-qt-faturada = 0.
    FOR EACH nota-fiscal WHERE
             nota-fiscal.nome-ab-cli = ped-venda.nome-abrev AND
             nota-fiscal.nr-pedcli = ped-venda.nr-pedcli NO-LOCK.

        FOR EACH it-nota-fisc OF nota-fiscal.
            ASSIGN de-qt-faturada = de-qt-faturada + it-nota-fisc.qt-faturada[1].
        END.
    END.

    IF de-qt-faturada = 0 THEN NEXT.
    
    FIND FIRST ped-item OF ped-venda WHERE
               ped-item.cod-sit-item = 1 NO-ERROR.
    IF AVAIL ped-item THEN NEXT.

    ASSIGN ped-venda.cod-sit-ped = 3.
    
    /*
    IF de-qt-faturada = de-qt-pedida THEN
        ASSIGN ped-venda.cod-sit-ped = 3.
    ELSE
        ASSIGN ped-venda.cod-sit-ped = 2.
    */
END.
