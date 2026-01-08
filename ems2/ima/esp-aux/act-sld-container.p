DEF VAR de-qt-vend AS DEC.

FOR EACH pp-container WHERE
         pp-container.situacao = 1 NO-LOCK.

    FOR EACH pp-it-container OF pp-container.
        ASSIGN de-qt-vend = 0.
        RUN pi-ver-ped.

        IF de-qt-vend = pp-it-container.qt-vendida  THEN NEXT.

        DISP pp-container.nr-container
             pp-it-container.it-codigo
             pp-it-container.cod-refer
             pp-it-container.qt-vendida
             de-qt-vend LABEL "QT Ped".

        PAUSE 0.

        //ASSIGN pp-it-container.qt-vendida = de-qt-vend.
    END.
END.

PROCEDURE pi-ver-ped.
    FOR EACH ped-item WHERE
             ped-item.it-codigo = pp-it-container.it-codigo AND
             ped-item.cod-refer = pp-it-container.cod-refer NO-LOCK.

        IF ped-item.cod-sit-item = 6 THEN NEXT.

        FIND ped-venda OF ped-item NO-LOCK NO-ERROR.
        IF NOT AVAIL ped-venda THEN NEXT.
        IF ped-venda.cod-sit-ped = 6 THEN NEXT.
             
        FIND ped-venda-ext WHERE
             ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND
             ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.
        IF NOT AVAIL ped-venda-ext THEN NEXT.


        IF ped-venda-ext.nr-container <> pp-container.nr-container THEN NEXT.

        ASSIGN de-qt-vend = de-qt-vend + ped-item.qt-pedida.
    END.
END PROCEDURE.


/*
PROCEDURE pi-ver-ped.
    ASSIGN de-qt-vend = 0.
    FOR EACH ped-venda-ext WHERE
             ped-venda-ext.nr-container = pp-container.nr-container NO-LOCK.

        FIND ped-venda WHERE
             ped-venda.nr-pedido = ped-venda-ext.nr-pedido NO-LOCK NO-ERROR.

        IF NOT AVAIL ped-venda THEN NEXT.

        IF ped-venda.cod-sit-ped = 6 THEN NEXT.

        FOR EACH ped-item OF ped-venda WHERE
                 ped-item.it-codigo = pp-it-container.it-codigo AND
                 ped-item.cod-refer = pp-it-container.cod-refer NO-LOCK.

            IF ped-item.cod-sit-item = 6 THEN NEXT.

            ASSIGN de-qt-vend = de-qt-vend + ped-item.qt-pedida.
        END.
    END.
END PROCEDURE.
*/
