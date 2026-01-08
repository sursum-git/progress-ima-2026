DEFINE VARIABLE dtotal AS DECIMAL     NO-UNDO.
DEFINE VARIABLE ipedido AS INTEGER     NO-UNDO.
UPDATE ipedido.
FOR EACH pedido-compr
    WHERE num-pedido = ipedido.
    FOR EACH ordem-compra OF pedido-compr.
        ASSIGN dtotal = 0.
        FOR EACH prazo-compra OF ordem-compra.
            ASSIGN dtotal = dtotal + prazo-compra.qtd-sal-forn.

        END.
        ASSIGN ordem-compra.qt-acum-nec = dTotal
               ordem-compra.qt-solic    = dTotal.
    END.
END.
