DEF BUFFER b-ped-item-res FOR ped-item-res.

/*OUTPUT TO "c:/lixo/ped-item-res.txt".*/
FOR EACH ped-item-res NO-LOCK.
    FIND ped-venda WHERE ped-venda.nome-abrev = ped-item-res.nome-abrev
                     AND ped-venda.nr-pedcli  = ped-item-res.nr-pedcli
                   NO-LOCK NO-ERROR.
    IF NOT AVAIL ped-venda THEN DO:
       DISPLAY ped-item-res.nome-abrev
               ped-item-res.nr-pedcli
               ped-item-res.dt-trans
               ped-item-res.hr-trans
               ped-item-res.faturado
               WITH WIDTH 300.
       FIND ped-venda WHERE ped-venda.nr-pedido = int(ped-item-res.nr-pedcli)
                      NO-LOCK NO-ERROR.
       IF AVAIL ped-venda THEN DO:
          DISPLAY ped-venda.nome-abrev
                  ped-venda.nr-pedcli
                  ped-venda.cod-sit-ped VIEW-AS FILL-IN
                  WITH WIDTH 300.
          FIND b-ped-item-res WHERE b-ped-item-res.nome-abrev   = ped-venda.nome-abrev
                                AND b-ped-item-res.nr-pedcli    = ped-venda.nr-pedcli
                                AND b-ped-item-res.nr-sequencia = ped-item-res.nr-sequencia
                                AND b-ped-item-res.it-codigo    = ped-item-res.it-codigo
                                AND b-ped-item-res.cod-refer    = ped-item-res.cod-refer
                              NO-LOCK NO-ERROR.
          DISPLAY AVAIL b-ped-item-res WITH WIDTH 300.
          /*
          IF NOT AVAIL b-ped-item-res THEN DO:
             FIND b-ped-item-res WHERE RECID(b-ped-item-res) = RECID(ped-item-res).
             IF AVAIL b-ped-item-res THEN
                ASSIGN b-ped-item-res.nome-abrev = ped-venda.nome-abrev
                       b-ped-item-res.faturado   = IF ped-venda.cod-sit-ped = 3 THEN YES
                                                                                ELSE NO.
          END.
          else
             delete ped-item-res.
          */
       END.
    END.
END.
/*OUTPUT CLOSE.*/
