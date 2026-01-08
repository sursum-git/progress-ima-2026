
FOR EACH saldo-estoq WHERE
         saldo-estoq.qtidade-atu > 0 AND
         saldo-estoq.cod-refer <> saldo-estoq.lote NO-LOCK.

    DISP saldo-estoq.it-codigo
         saldo-estoq.cod-refer
         saldo-estoq.lote
         saldo-estoq.qtidade-atu
         WITH WIDTH 550.
END.
