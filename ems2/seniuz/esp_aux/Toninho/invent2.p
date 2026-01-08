FOR EACH inventario WHERE
         inventario.dt-saldo = 12.31.2015 AND
         inventario.it-codigo = '880038'.

    DISP inventario.cod-depos
         inventario.cod-refer
         inventario.val-apurado[1].

    ASSIGN inventario.cod-depos = 'ALM'.
END.
