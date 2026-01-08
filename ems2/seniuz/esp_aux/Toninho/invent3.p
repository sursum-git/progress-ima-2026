DEF VAR de-qtidade-atu AS DEC.

FOR EACH inventario WHERE
         inventario.dt-saldo = 12.31.2015 AND
         inventario.nr-ficha = 1485.

    /*
    DISP inventario.it-codig
         inventario.cod-refer
         INT(inventario.situacao)
         inventario.dt-atualiz
         inventario.qtidade-atu
         inventario.val-apurado[1].
    PAUSE 0.
    */

    ASSIGN inventario.situacao = 4.

    /*
    RUN esapi/calc-saldo-data.p (INPUT inventario.cod-estabel,
                                 INPUT inventario.cod-depos,
                                 INPUT inventario.it-codigo,
                                 INPUT inventario.cod-refer,
                                 INPUT inventario.lote,
                                 INPUT inventario.dt-saldo,
                                 OUTPUT de-qtidade-atu).

    ASSIGN inventario.qtidade-atu = de-qtidade-atu
           inventario.situacao = 4
           inventario.ind-sit-invent-wms = 1
           inventario.dt-atualiz = ?.
    */       
END.
