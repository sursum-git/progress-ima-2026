FOR EACH saldo-estoq WHERE
         saldo-estoq.qt-aloc-ped > 0 NO-LOCK.

    IF saldo-estoq.qtidade-atu > saldo-estoq.qt-aloc-ped THEN NEXT.

    DISP saldo-estoq.it-codigo
         saldo-estoq.lote
         saldo-estoq.qtidade-atu
         saldo-estoq.qt-aloc-ped (TOTAL).

    FOR EACH ped-item WHERE
             ped-item.cod-sit-item = 1 AND
             ped-item.it-codigo = saldo-estoq.it-codigo AND
             ped-item.cod-refer = saldo-estoq.cod-refer NO-LOCK.

        DISP ped-item.nr-pedcli
             ped-item.qt-pedida
             ped-item.qt-log-aloc (TOTAL).
    END.
END.


