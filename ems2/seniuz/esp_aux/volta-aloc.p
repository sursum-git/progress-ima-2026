DEF VAR c-item AS CHAR.
DEF VAR c-ref AS CHAR.
DEF VAR c-lote AS CHAR.
DEF VAR de-qtd AS DEC.

INPUT FROM "M:\EMS206\especificos\Seniuz\Seguran‡a\saldo-aloca.txt".
REPEAT.
    SET c-item
        c-ref
        c-lote
        de-qtd.

    FIND saldo-estoq WHERE
         saldo-estoq.it-codigo = c-item AND
         saldo-estoq.cod-refer = c-ref AND
         saldo-estoq.lote = c-lote NO-ERROR.

    ASSIGN saldo-estoq.qt-alocada = de-qtd.


END.
