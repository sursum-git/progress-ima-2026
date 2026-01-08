DEF TEMP-TABLE tt-aux 
    FIELD it-codigo    LIKE inv-acab.it-codigo
    FIELD cod-refer    LIKE inv-acab.cod-refer
    FIELD lote         LIKE inv-acab.lote
    FIELD qtd-inv      LIKE inv-acab.qtd-inv.
    
DEF VAR de-qtidade-atu LIKE saldo-estoq.qtidade-atu.
DEF VAR da-dt-inventario AS DATE.
DEF VAR i-nr-ficha     LIKE inventario.nr-ficha.

ASSIGN da-dt-inventario = TODAY.

INPUT FROM c:\temp\invent.csv.
REPEAT.
    CREATE tt-aux.
    IMPORT DELIMITER ";" tt-aux.
END.

RUN pi-atualiza.

PROCEDURE pi-atualiza.

    FOR EACH tt-aux WHERE
             tt-aux.qtd-inv > 0 NO-LOCK
       BREAK BY tt-aux.it-codigo
             BY tt-aux.cod-refer
             BY tt-aux.lote:

       FIND LAST movto-estoq WHERE
                 movto-estoq.it-codigo = tt-aux.it-codigo 
                 NO-LOCK NO-ERROR.

       FIND item-uni-estab WHERE
            item-uni-estab.it-codigo   = tt-aux.it-codigo AND
            item-uni-estab.cod-estabel = IF AVAIL movto-estoq
                                         THEN movto-estoq.cod-estabel
                                         ELSE '2'
            NO-LOCK NO-ERROR.

       ASSIGN de-qtidade-atu = 0.
       FOR EACH saldo-estoq WHERE
                saldo-estoq.cod-estabel = item-uni-estab.cod-estabel  AND
                saldo-estoq.cod-depos   = item-uni-estab.deposito-pad AND
                saldo-estoq.it-codigo   = tt-aux.it-codigo            AND
                saldo-estoq.lote        = tt-aux.lote NO-LOCK.
            ASSIGN de-qtidade-atu = de-qtidade-atu + saldo-estoq.qtidade-atu.
       END.

       FIND LAST inventario WHERE
                 inventario.dt-saldo = da-dt-inventario
                 USE-INDEX nr-ficha NO-LOCK NO-ERROR.

       ASSIGN i-nr-ficha = IF AVAIL inventario 
                           THEN inventario.nr-ficha + 1
                           ELSE 1.

       CREATE inventario.
       ASSIGN inventario.dt-saldo       = da-dt-inventario
              inventario.nr-ficha       = i-nr-ficha
              inventario.cod-estabel    = item-uni-estab.cod-estabel
              inventario.cod-depos      = item-uni-estab.deposito-pad
              inventario.it-codigo      = tt-aux.it-codigo
              inventario.cod-refer      = tt-aux.cod-refer
              inventario.lote           = tt-aux.lote
              inventario.dt-ult-entra   = da-dt-inventario
              inventario.dt-ult-saida   = IF AVAIL movto-estoq
                                          THEN movto-estoq.dt-trans
                                          ELSE da-dt-inventario
              inventario.qtidade-atu    = de-qtidade-atu
              inventario.val-apurado[1] = tt-aux.qtd-inv 
              inventario.situacao       = 4.
    END.
END PROCEDURE.
