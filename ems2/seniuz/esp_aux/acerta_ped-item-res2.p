DEF BUFFER b-ped-item-res FOR ped-item-res.

FOR EACH ped-item-res NO-LOCK.
    FIND ped-item USE-INDEX ch-item-ped
                  WHERE ped-item.nome-abrev   = ped-item-res.nome-abrev
                    AND ped-item.nr-pedcli    = ped-item-res.nr-pedcli
                    AND ped-item.it-codigo    = ped-item-res.it-codigo
                    AND ped-item.cod-refer    = ped-item-res.cod-refer
                    AND ped-item.nr-sequencia = ped-item-res.nr-sequencia
                   NO-LOCK NO-ERROR.
    IF NOT AVAIL ped-item THEN DO:
       DISP ped-item-res.nome-abrev
            ped-item-res.nr-pedcli
            ped-item-res.nr-sequencia.
       /*
       FIND b-ped-item-res WHERE RECID(b-ped-item-res) = RECID(ped-item-res).
       delete b-ped-item-res.
       */
    END.
END.
