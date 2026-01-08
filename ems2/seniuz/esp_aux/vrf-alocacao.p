DEF VAR de-tot-aloc AS DEC.

FOR EACH saldo-estoq WHERE
         saldo-estoq.qt-aloc-ped > 0 NO-LOCK.

    ASSIGN de-tot-aloc = 0.
    FOR EACH ped-item WHERE
             ped-item.cod-sit-item = 1 AND              
             ped-item.it-codigo = saldo-estoq.it-codigo AND
             ped-item.cod-refer = saldo-estoq.cod-refer NO-LOCK.
 
        FIND ped-venda OF ped-item NO-LOCK NO-ERROR.
        IF NOT ped-venda.completo THEN NEXT.

        ASSIGN de-tot-aloc = de-tot-aloc + ped-item.qt-log-aloc.
    END.

    IF saldo-estoq.qt-aloc-ped <> de-tot-aloc THEN DO.

       DISP saldo-estoq.it-codigo
            saldo-estoq.cod-refer
            saldo-estoq.qtidade-atu
            saldo-estoq.qt-aloc-ped  COLUMN-LABEL "Aloc Saldo" 
              de-tot-aloc COLUMN-LABEL "Aloc PED"
            saldo-estoq.qt-aloc-ped - de-tot-aloc.

       FOR EACH ped-item WHERE
                ped-item.cod-sit-item = 1 AND
                ped-item.it-codigo = saldo-estoq.it-codigo AND
                ped-item.cod-refer = saldo-estoq.cod-refer NO-LOCK.

           FIND ped-venda OF ped-item NO-LOCK NO-ERROR.
           IF NOT ped-venda.completo THEN NEXT.

           /*IF ped-venda.cod-sit-ped <> 1  THEN NEXT.*/

           DISP ped-item.nr-pedcli
                INT(ped-venda.cod-sit-ped)
                ped-item.qt-pedida
                ped-item.qt-log-aloc.
       END.
    END.
END.

/*
DEF VAR l-erro AS LOG.
FOR EACH ped-venda WHERE
         ped-venda.cod-sit-ped = 4 NO-LOCK.

    FIND pre-fatur OF ped-venda NO-LOCK NO-ERROR.
    IF AVAIL pre-fatur THEN NEXT.

    FOR EACH ped-item OF ped-venda WHERE
             ped-item.cod-sit-item = 1 NO-LOCK.

        IF ped-item.qt-pedida <> ped-item.qt-log THEN
           DISP ped-venda.nr-pedcli
                ped-item.nr-sequencia
                ped-item.it-codigo
                ped-item.qt-pedida
                ped-item.qt-log-aloca.
           
    END.
END.
*/
