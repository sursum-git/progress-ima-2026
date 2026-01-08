DEF BUFFER b-ped-item-ext FOR ped-item-ext.

/*OUTPUT TO "c:/lixo/ped-item-ext.txt".*/
FOR EACH ped-item-ext NO-LOCK.
    FIND ped-venda WHERE ped-venda.nome-abrev = ped-item-ext.nome-abrev
                     AND ped-venda.nr-pedcli  = ped-item-ext.nr-pedcli
                   NO-LOCK NO-ERROR.
    IF NOT AVAIL ped-venda THEN DO:
       DISPLAY ped-item-ext.nome-abrev
               ped-item-ext.nr-pedcli
               WITH WIDTH 300.
       FIND ped-venda WHERE ped-venda.nr-pedido = int(ped-item-ext.nr-pedcli)
                      NO-LOCK NO-ERROR.
       IF AVAIL ped-venda THEN DO:
          DISPLAY ped-venda.nome-abrev
                  ped-venda.nr-pedcli
                  ped-venda.cod-sit-ped VIEW-AS FILL-IN
                  WITH WIDTH 300.
          FIND b-ped-item-ext WHERE b-ped-item-ext.nome-abrev   = ped-venda.nome-abrev
                                AND b-ped-item-ext.nr-pedcli    = ped-venda.nr-pedcli
                                AND b-ped-item-ext.nr-sequencia = ped-item-ext.nr-sequencia
                                AND b-ped-item-ext.it-codigo    = ped-item-ext.it-codigo
                                AND b-ped-item-ext.cod-refer    = ped-item-ext.cod-refer
                              NO-LOCK NO-ERROR.
          DISPLAY AVAIL b-ped-item-ext WITH WIDTH 300.
          /*
          IF NOT AVAIL b-ped-item-ext THEN DO:
             FIND b-ped-item-ext WHERE RECID(b-ped-item-ext) = RECID(ped-item-ext).
             IF AVAIL b-ped-item-ext THEN
                ASSIGN b-ped-item-ext.nome-abrev = ped-venda.nome-abrev
                       b-ped-item-ext.faturado   = IF ped-venda.cod-sit-ped = 3 THEN YES
                                                                                ELSE NO.
          END.
          else
             delete ped-item-ext.
          */
       END.
    END.
END.
/*OUTPUT CLOSE.*/
