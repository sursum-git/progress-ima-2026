DEF VAR de-qtd-receb AS DEC.

FOR EACH ordem-compra WHERE 
         ordem-compra.numero-ordem = 71532905 SHARE-LOCK.
         //ordem-compra.num-pedido = tt-pedidos.num-pedido SHARE-LOCK.

    ASSIGN de-qtd-receb = 0.
    FOR EACH item-doc-est WHERE
             item-doc-est.num-pedido = ordem-compra.num-pedido AND
             item-doc-est.numero-ordem = ordem-compra.numero-ordem NO-LOCK.

        FIND docum-est OF item-doc-est NO-LOCK NO-ERROR.
        IF docum-est.ce-atual = NO THEN NEXT.

        ASSIGN de-qtd-receb = de-qtd-receb + item-doc-est.quantidade.
    END.

    FOR EACH prazo-compra OF ordem-compra WHERE
             prazo-compra.quant-saldo <> 0 SHARE-LOCK.
    
        ASSIGN prazo-compra.dec-1 = prazo-compra.quantidade - de-qtd-receb
               prazo-compra.quant-saldo = prazo-compra.quantidade - de-qtd-receb
               prazo-compra.quant-receb = de-qtd-receb.
    
        ASSIGN prazo-compra.dec-1 = 0.
    
        IF prazo-compra.quant-saldo < 0 THEN
           ASSIGN prazo-compra.quant-saldo = 0.
    
        IF prazo-compra.quant-saldo = 0 THEN
           ASSIGN ordem-compra.situacao = 6. // Recebida

    END.
END.




