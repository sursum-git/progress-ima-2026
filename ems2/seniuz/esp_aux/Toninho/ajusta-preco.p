DEF VAR de-preco LIKE preco-item.preco-venda.

FOR EACH preco-item WHERE
         preco-item.nr-tabpre = 'tab a12' NO-LOCK.

    ASSIGN de-preco = preco-item.preco-fob * 1.10.

    DISP  preco-item.cod-refer
          preco-item.preco-venda
          de-preco.
    PAUSE 0.

    ASSIGN preco-item.preco-fob = de-preco.
END.

