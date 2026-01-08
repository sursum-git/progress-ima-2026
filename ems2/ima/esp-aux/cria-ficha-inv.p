DEF BUFFER b-movto-estoq FOR movto-estoq.

DEF VAR i-nr-ficha         AS INTEGER.
DEF VAR de-qtidade-atu     LIKE saldo-estoq.qtidade-atu.
DEF VAR da-data-inv        AS DATE FORMAT "99/99/9999".

ASSIGN da-data-inv = 12.31.2019.

FIND LAST inventario WHERE
          inventario.dt-saldo = da-data-inv NO-LOCK USE-INDEX nr-ficha NO-ERROR.

ASSIGN i-nr-ficha = IF AVAIL inventario 
                    THEN inventario.nr-ficha 
                    ELSE 0.

FOR EACH item WHERE
         item.ge-codigo >= 50 AND
         item.ge-codigo <= 60 NO-LOCK.

    DISP item.it-codigo.
    PAUSE 0.

    FOR EACH saldo-estoq WHERE 
             saldo-estoq.it-codigo = ITEM.it-codigo AND 
             saldo-estoq.qtidade-atu > 0 NO-LOCK.

        ASSIGN de-qtidade-atu = 0.
        RUN esapi/calc-saldo-data.p (INPUT saldo-estoq.cod-estabel,
                                     INPUT saldo-estoq.cod-depos,
                                     INPUT saldo-estoq.it-codigo,
                                     INPUT saldo-estoq.cod-refer,
                                     INPUT saldo-estoq.lote,
                                     INPUT da-data-inv, 
                                     OUTPUT de-qtidade-atu).

        IF de-qtidade-atu = 0 THEN NEXT.

        ASSIGN i-nr-ficha = i-nr-ficha + 1.

        FIND LAST b-movto-estoq WHERE
                  b-movto-estoq.cod-estabel = saldo-estoq.cod-estabel AND
                  b-movto-estoq.it-codigo = saldo-estoq.it-codigo AND
                  b-movto-estoq.cod-refer = saldo-estoq.cod-refer AND
                  b-movto-estoq.lote = saldo-estoq.lote AND
                  b-movto-estoq.tipo-trans = 1
                  NO-LOCK NO-ERROR.

        FIND LAST movto-estoq WHERE
                  movto-estoq.cod-estabel = saldo-estoq.cod-estabel AND
                  movto-estoq.it-codigo = saldo-estoq.it-codigo AND
                  movto-estoq.cod-refer = saldo-estoq.cod-refer AND
                  movto-estoq.lote = saldo-estoq.lote AND
                  movto-estoq.tipo-trans = 2
                  NO-LOCK NO-ERROR.

        CREATE inventario.
        ASSIGN inventario.dt-saldo       = da-data-inv
               inventario.nr-ficha       = i-nr-ficha
               inventario.cod-estabel    = saldo-estoq.cod-estabel
               inventario.cod-depos      = saldo-estoq.cod-depos
               inventario.it-codigo      = saldo-estoq.it-codigo
               inventario.cod-refer      = saldo-estoq.cod-refer
               inventario.lote           = saldo-estoq.lote
               inventario.dt-ult-entra   = IF AVAIL b-movto-estoq
                                           THEN b-movto-estoq.dt-trans
                                           ELSE da-data-inv
               inventario.dt-ult-saida   = IF AVAIL movto-estoq
                                           THEN movto-estoq.dt-trans
                                           ELSE da-data-inv
               inventario.qtidade-atu    = de-qtidade-atu
               inventario.val-apurado[1] = de-qtidade-atu 
               inventario.situacao       = 4.
    END.
END.


