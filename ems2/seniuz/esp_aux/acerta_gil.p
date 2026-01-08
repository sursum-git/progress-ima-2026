DEF VAR i-nao-encont AS INT.
DEF VAR i-ok AS INT.
DEF VAR i-nok AS INT.

FOR EACH ped-item-res WHERE ped-item-res.it-codigo BEGINS "5" NO-LOCK.
    FIND ped-item-ext WHERE ped-item-ext.nome-abrev   = ped-item-res.nome-abrev
                        AND ped-item-ext.nr-pedcli    = ped-item-res.nr-pedcli
                        AND ped-item-ext.it-codigo    = ped-item-res.it-codigo
                        AND ped-item-ext.cod-refer    = ped-item-res.cod-refer
                        AND ped-item-ext.nr-sequencia = ped-item-res.nr-sequencia
                      NO-LOCK NO-ERROR.
    IF NOT AVAIL ped-item-ext THEN DO.
       FIND ITEM WHERE ITEM.it-codigo = ped-item-res.it-codigo NO-LOCK.
       IF ITEM.tipo-con-est = 4 THEN DO.
          ASSIGN i-nao-encont = i-nao-encont + 1.
          /*
          DISP ped-item-res.nome-abrev
               ped-item-res.nr-pedcli
               ped-item-res.it-codigo
               ped-item-res.cod-refer.
          */
       END.
    END.
    ELSE DO:
       IF ped-item-ext.reservado THEN
          ASSIGN i-ok = i-ok + 1.
       ELSE
          ASSIGN i-nok = i-nok + 1.
    END.
END.
DISP i-nao-encont
     i-ok
     i-nok.

/* Resultados em 07/04/2005: NÆo encontr: 1.839, Ok: 39.679 e NÆo Ok: 0 */
/*
FOR EACH ped-item-ext.
    FIND ped-item-res OF ped-item-ext NO-LOCK NO-ERROR.
    ASSIGN ped-item-ext.reservado = AVAIL ped-item-res.
END.
*/
