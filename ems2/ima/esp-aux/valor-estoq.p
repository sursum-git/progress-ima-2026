DEF VAR de-preco AS DECIMAL.
DEF VAR de-tot AS DEC FORMAT ">>>,>>>,>>9.99".

FOR EACH inventario WHERE
         inventario.dt-saldo = 12.31.2018.

    ASSIGN de-preco = 1.
    FIND LAST item-doc-est WHERE
              item-doc-est.it-codigo = inventario.it-codigo NO-LOCK NO-ERROR.
    IF AVAIL item-doc-est THEN
       ASSIGN de-preco = item-doc-est.preco-unit[1].
    ELSE DO.
        FIND LAST pr-it-per WHERE 
                  pr-it-per.it-codigo = inventario.it-codigo NO-LOCK NO-ERROR.
        IF AVAIL pr-it-per THEN
           ASSIGN de-preco = pr-it-per.val-unit-mat-m[1].
    END.

    ASSIGN de-tot = de-tot + (inventario.val-apurado[1] * de-preco).
END.

DISP de-tot.

