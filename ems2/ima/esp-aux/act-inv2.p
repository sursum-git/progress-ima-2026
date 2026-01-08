FOR EACH inventario WHERE
         inventario.dt-saldo = 04.01.2018 BY inventario.nr-ficha.

    DISP inventario.it-codigo
         inventario.cod-refer
         inventario.qtidade-atu
         inventario.val-apurado[1] (TOTAL)
         WITH WIDTH 550.


    ASSIGN inventario.situacao = 4
           inventario.dt-atualiza = ?. 

    IF inventario.val-apurado[1] = 0 THEN
       ASSIGN inventario.val-apurado[1] = ?. 
END.
