DEF VAR i-cont AS INT.
FOR EACH ped-venda WHERE (ped-venda.cod-sit-ped < 3 OR ped-venda.cod-sit-ped = 5)
                     AND ped-venda.cod-cond-pag = 0
                   NO-LOCK.
    FIND FIRST cond-ped WHERE cond-ped.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.
    IF NOT AVAIL cond-ped THEN
       DISP ped-venda.nr-pedcli
            ped-venda.nome-abrev
            ped-venda.cod-sit-ped VIEW-AS FILL-IN
            ped-venda.cod-cond-pag.
    ELSE DO:
        ASSIGN i-cont = 0.
        FOR EACH cond-ped WHERE cond-ped.nr-pedido = ped-venda.nr-pedido NO-LOCK.
            ASSIGN i-cont = i-cont + 1.
        END.
        IF i-cont > 10 THEN
           DISPLAY ped-venda.nr-pedcli
                   ped-venda.nome-abrev
                   i-cont.
    END.
END.

